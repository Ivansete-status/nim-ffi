import std/[macros, tables]
import chronos
import ../ffi_thread_request, ../ffi_types

var registeredRequests {.compileTime.}: Table[uint, NimNode]

proc fnv1aHash32*(s: string): uint =
  const
    FNV_offset_basis: uint = 2166136261'u32.uint
    FNV_prime: uint = 16777619'u32.uint
  var hash: uint = FNV_offset_basis
  for c in s:
    hash = hash xor uint(ord(c))
    hash = hash * FNV_prime
  return hash

proc generateProc(p: NimNode, args: varargs[NimNode]): NimNode =
  let wrapperPragmas =
    nnkPragma.newTree(ident("dynlib"), ident("exportc"), ident("cdecl"))
  let origBody = p.body
  let wrapperBody = quote:
    initializeLibrary()
    `origBody`

  let origParams = p.params
  let wrapperProc = newProc(
    name = p.name, params = @[origParams], pragmas = wrapperPragmas, body = wrapperBody
  )

  var res = newStmtList()
  res.add(wrapperProc)
  return res

proc buildFfiNewReqProc(reqTypeName, body: NimNode): NimNode =
  # Standard FFI params
  var formalParams = newSeq[NimNode]()

  var procNode: NimNode
  if body.kind == nnkStmtList and body.len == 1:
    procNode = body[0] # unwrap single statement
  else:
    procNode = body

  if procNode.kind != nnkLambda:
    error "registerFFI expects a lambda definition. Found: " & $procNode.kind

  # T: typedesc[CreateNodeRequest]
  let typedescParam = newIdentDefs(
    ident("T"), # param name
    nnkBracketExpr.newTree(ident("typedesc"), reqTypeName), # typedesc[T]
  )
  formalParams.add(typedescParam)

  # Other fixed FFI params
  formalParams.add(newIdentDefs(ident("callback"), ident("FFICallBack")))
  formalParams.add(newIdentDefs(ident("userData"), ident("pointer")))

  # Add original lambda params
  let procParams = procNode[3]
  for p in procParams[1 .. ^1]:
    formalParams.add(p)

  # Build `ptr FFIThreadRequest`
  let retType = newNimNode(nnkPtrTy)
  retType.add(ident("FFIThreadRequest"))

  formalParams = @[retType] & formalParams

  # Build body
  let reqObjIdent = ident("reqObj")
  var newBody = newStmtList()
  newBody.add(
    quote do:
      var `reqObjIdent` = createShared(T)
  )

  for p in procParams[1 .. ^1]:
    let fieldNameIdent = ident($p[0])
    let fieldTypeNode = p[1]

    # Extract type name as string
    var typeStr: string
    if fieldTypeNode.kind == nnkIdent:
      typeStr = $fieldTypeNode
    elif fieldTypeNode.kind == nnkBracketExpr:
      typeStr = $fieldTypeNode[0] # e.g., `ptr` in `ptr[Waku]`
    else:
      typeStr = "" # fallback

    # Apply .alloc() only to cstrings
    if typeStr == "cstring":
      newBody.add(
        quote do:
          `reqObjIdent`[].`fieldNameIdent` = `fieldNameIdent`.alloc()
      )
    else:
      newBody.add(
        quote do:
          `reqObjIdent`[].`fieldNameIdent` = `fieldNameIdent`
      )

  # FFIThreadRequest.init using fnv1aHash32
  newBody.add(
    quote do:
      let typeStr = $T
      var ret =
        FFIThreadRequest.init(callback, userData, fnv1aHash32(typeStr), `reqObjIdent`)
      return ret
  )

  # Build the proc node
  result = newProc(
    name = postfix(ident("ffiNewReq"), "*"),
    params = formalParams,
    body = newBody,
    pragmas = newEmptyNode(),
  )

  echo result.repr

proc buildFfiDeleteReqProc(reqTypeName: NimNode): NimNode =
  ## Generates something like:
  ## proc ffiDeleteReq(self: ptr CreateNodeRequest) =
  ##   deallocShared(self[].configJson)
  ##   deallocShared(self)

  result = newProc(
    name = ident("ffiDeleteReq"),
    params =
      @[
        newEmptyNode(), # return type is empty (void)
        newIdentDefs(ident("self"), nnkPtrTy.newTree(reqTypeName)),
      ],
    body = newStmtList(
      nnkCall.newTree(
        ident("deallocShared"),
        nnkBracketExpr.newTree(
          nnkBracketExpr.newTree(ident("self"), newEmptyNode()), # self[]
          ident("configJson"),
        ),
      ),
      nnkCall.newTree(ident("deallocShared"), ident("self")),
    ),
    procType = nnkProcDef,
  )

proc buildProcessFFIRequestProc(reqTypeName, reqHandler, body: NimNode): NimNode =
  ## Builds:
  ## proc processFFIRequest(T: typedesc[CreateNodeRequest];
  ##                        configJson: cstring;
  ##                        appCallbacks: AppCallbacks;
  ##                        waku: ptr Waku) ...

  # Require: ident: ptr SomeType
  if reqHandler.kind != nnkExprColonExpr:
    error(
      "Second argument must be a typed parameter, e.g., waku: ptr Waku. Found: " &
        $reqHandler.kind
    )

  let rhs = reqHandler[1]
  if rhs.kind != nnkPtrTy:
    error("Second argument must be a pointer type, e.g., waku: ptr Waku")

  var procNode = body
  if procNode.kind == nnkStmtList and procNode.len == 1:
    procNode = procNode[0]

  if procNode.kind != nnkLambda:
    error "registerFFI expects a lambda definition. Found: " & $procNode.kind

  var formalParams = newSeq[NimNode]()

  # First param: T: typedesc[reqTypeName]
  let typedescParam =
    newIdentDefs(ident("T"), nnkBracketExpr.newTree(ident("typedesc"), reqTypeName))
  formalParams.add(typedescParam)

  # Add original lambda params
  let procParams = procNode[3]
  for p in procParams[1 .. ^1]:
    formalParams.add(p)

  # Add pointer handler param at the end
  formalParams.add(
    newIdentDefs(reqHandler[0], rhs) # e.g., waku: ptr Waku
  )

  # Return type first
  formalParams = @[procParams[0]] & formalParams

  # Pragmas
  let pragmas =
    if procNode.len >= 5:
      procNode[4]
    else:
      newEmptyNode()

  # Body
  let bodyNode =
    if procNode.body.kind == nnkStmtList:
      procNode.body
    else:
      newStmtList(procNode.body)

  # Build proc
  result = newProc(
    name = postfix(ident("processFFIRequest"), "*"),
    params = formalParams,
    body = bodyNode,
    procType = nnkProcDef,
    pragmas = pragmas,
  )
  echo result.repr

macro registerFFI*(reqTypeName, reqHandler, body: untyped): untyped =
  let ffiNewReqProc = buildFfiNewReqProc(reqTypeName, body)
  let processProc = buildProcessFFIRequestProc(reqTypeName, reqHandler, body)
  result = newStmtList(ffiNewReqProc, processProc)

macro ffiGenerateProcess*(): untyped =
  ## Generates a case statement that handles all the possible registered FFI requests
  let reqParam = ident"request"
  let reqHandlerParam = ident"reqHandler"

  var caseStmt =
    newTree(nnkCaseStmt, newDotExpr(newTree(nnkBracketExpr, reqParam), ident"reqId"))

  for caseKey, typeIdent in registeredRequests.pairs:
    let castExpr = newTree(
      nnkCast,
      newTree(nnkPtrTy, typeIdent),
      newDotExpr(newTree(nnkBracketExpr, reqParam), ident"reqContent"),
    )
    let callExpr = newCall(ident"process", castExpr, reqHandlerParam)
    caseStmt.add newTree(nnkOfBranch, newLit(caseKey), nnkStmtList.newTree(callExpr))

  caseStmt.add newTree(
    nnkElse,
    nnkStmtList.newTree(
      newCall(
        ident"nilProcess", newDotExpr(newTree(nnkBracketExpr, reqParam), ident"reqId")
      )
    ),
  )

  let retFutSym = ident"retFut"

  result = quote:
    proc process*[R](
        T: type FFIThreadRequest,
        `reqParam`: ptr FFIThreadRequest,
        `reqHandlerParam`: ptr R,
    ) {.async.} =
      let `retFutSym` = `caseStmt`
      handleRes(await `retFutSym`, `reqParam`)

  echo result.repr
