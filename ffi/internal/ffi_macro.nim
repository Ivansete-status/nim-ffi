import std/[macros, tables]
import chronos
import ../ffi_types

var registeredRequests* {.threadvar.}: Table[string, FFIRequestProc]

proc buildFfiNewReqProc(reqTypeName, body: NimNode): NimNode =
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
        FFIThreadRequest.init(callback, userData, typeStr.cstring, `reqObjIdent`)
      return ret
  )

  # Build the proc node
  result = newProc(
    name = postfix(ident("ffiNewReq"), "*"),
    params = formalParams,
    body = newBody,
    pragmas = newEmptyNode(),
  )

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

  ## Builds:
  ## proc processFFIRequest*(request: pointer; waku: ptr Waku) ...

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

  let typedescParam =
    newIdentDefs(ident("T"), nnkBracketExpr.newTree(ident("typedesc"), reqTypeName))

  # Build formal params: (returnType, request: pointer, waku: ptr Waku)
  let procParams = procNode[3]
  var formalParams: seq[NimNode] = @[]
  formalParams.add(procParams[0]) # return type
  formalParams.add(typedescParam)
  formalParams.add(newIdentDefs(ident("request"), ident("pointer")))
  formalParams.add(newIdentDefs(reqHandler[0], rhs)) # e.g. waku: ptr Waku

  # Inject cast/unpack/defer into the body
  let bodyNode =
    if procNode.body.kind == nnkStmtList:
      procNode.body
    else:
      newStmtList(procNode.body)

  let newBody = newStmtList()
  let reqIdent = ident("req")

  newBody.add quote do:
    let `reqIdent`: ptr `reqTypeName` = cast[ptr `reqTypeName`](request)
    defer:
      ffiDeleteReq(`reqIdent`)

  # automatically unpack fields into locals
  for p in procParams[1 ..^ 1]:
    let fieldName = p[0] # Ident

    newBody.add quote do:
      let `fieldName` = `reqIdent`[].`fieldName`

  # Append user's lambda body
  newBody.add(bodyNode)

  result = newProc(
    name = postfix(ident("processFFIRequest"), "*"),
    params = formalParams,
    body = newBody,
    procType = nnkProcDef,
    pragmas =
      if procNode.len >= 5:
        procNode[4]
      else:
        newEmptyNode(),
  )

proc addNewRequestToRegistry(reqTypeName, reqHandler: NimNode): NimNode =
  ## Adds a new request to the registeredRequests table.
  ## The key is the hash of the request type name, and the value is the NimNode
  ## representing the request type.

  # Build: request[].reqContent
  let reqContent =
    newDotExpr(newTree(nnkDerefExpr, ident("request")), ident("reqContent"))

  # Build Future[Result[string, string]] return type
  let returnType = nnkBracketExpr.newTree(
    ident("Future"),
    nnkBracketExpr.newTree(ident("Result"), ident("string"), ident("string")),
  )

  # Extract the type from reqHandler (generic: ptr Waku, ptr Foo, ptr Bar, etc.)
  let rhsType =
    if reqHandler.kind == nnkExprColonExpr:
      reqHandler[1] # Use the explicit type
    else:
      error "Second argument must be a typed parameter, e.g. waku: ptr Waku"

  # Build: cast[ptr Waku](reqHandler) or cast[ptr Foo](reqHandler) dynamically
  let castedHandler = newTree(
    nnkCast,
    rhsType, # The type, e.g. ptr Waku
    ident("reqHandler"), # The expression to cast
  )

  let callExpr = newCall(
    newDotExpr(reqTypeName, ident("processFFIRequest")), ident("request"), castedHandler
  )

  var newBody = newStmtList()
  newBody.add(
    quote do:
      return await `callExpr`
  )

  # Build:
  # proc(request: pointer, reqHandler: pointer):
  #     Future[Result[string, string]] {.async.} =
  #   CreateNodeRequest.processFFIRequest(request, reqHandler)
  let asyncProc = newProc(
    name = newEmptyNode(), # anonymous proc
    params =
      @[
        returnType,
        newIdentDefs(ident("request"), ident("pointer")),
        newIdentDefs(ident("reqHandler"), ident("pointer")),
      ],
    body = newBody,
    pragmas = nnkPragma.newTree(ident("async")),
  )

  let reqTypeNameStr = $reqTypeName

  # Use a string literal instead of reqTypeNameStr
  let key = newLit($reqTypeName)
  # Generate: registeredRequests["CreateNodeRequest"] = <generated proc>
  result =
    newAssignment(newTree(nnkBracketExpr, ident("registeredRequests"), key), asyncProc)

macro registerFFI*(reqTypeName, reqHandler, body: untyped): untyped =
  let ffiNewReqProc = buildFfiNewReqProc(reqTypeName, body)
  let processProc = buildProcessFFIRequestProc(reqTypeName, reqHandler, body)
  let addNewReqToReg = addNewRequestToRegistry(reqTypeName, reqHandler)
  result = newStmtList(ffiNewReqProc, processProc, addNewReqToReg)

macro ffiGenerateProcess*(): untyped =
  ## Generates a case statement that handles all the possible registered FFI requests
  let reqParam = ident"request"
  let reqHandlerParam = ident"reqHandler"

  var caseStmt =
    newTree(nnkCaseStmt, newDotExpr(newTree(nnkBracketExpr, reqParam), ident"reqId"))

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
