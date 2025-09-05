import std/[macros, tables]
import chronos
import ../ffi_types

var registeredRequests* {.threadvar.}: Table[cstring, FFIRequestProc]

proc extractFieldsFromLambda(body: NimNode): seq[NimNode] =
  ## Extracts the fields (params) from the given lambda body.
  var procNode = body
  if procNode.kind == nnkStmtList and procNode.len == 1:
    procNode = procNode[0]
  if procNode.kind != nnkLambda:
    error "registerReqFFI expects a lambda proc, found: " & $procNode.kind

  let params = procNode[3] # parameters list
  result = @[]
  for p in params[1 .. ^1]: # skip return type
    result.add newIdentDefs(p[0], p[1])

proc buildRequestType(reqTypeName: NimNode, body: NimNode): NimNode =
  ## Builds:
  ## type <reqTypeName>* = object
  ##   <lambdaParam1Name>: <lambdaParam1Type>
  ##   ...

  var procNode = body
  if procNode.kind == nnkStmtList and procNode.len == 1:
    procNode = procNode[0]
  if procNode.kind != nnkLambda:
    error "registerReqFFI expects a lambda proc, found: " & $procNode.kind

  let params = procNode[3] # formal params of the lambda
  var fields: seq[NimNode] = @[]
  for p in params[1 .. ^1]: # skip return type at index 0
    let name = p[0]
    let typ = p[1]
    # Field must be nnkIdentDefs(name, type, defaultExpr)
    fields.add newTree(nnkIdentDefs, name, typ, newEmptyNode())

  # Wrap fields in a rec list
  let recList = newTree(nnkRecList, fields)

  # object type node: object [of?] [] [pragma?] recList
  let objTy = newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), recList)

  # Export the type (CreateNodeRequest*)
  let typeName =
    if reqTypeName.kind == nnkPostfix:
      reqTypeName
    else:
      postfix(reqTypeName, "*")

  result =
    newNimNode(nnkTypeSection).add(newTree(nnkTypeDef, typeName, newEmptyNode(), objTy))

proc buildFfiNewReqProc(reqTypeName, body: NimNode): NimNode =
  var formalParams = newSeq[NimNode]()

  var procNode: NimNode
  if body.kind == nnkStmtList and body.len == 1:
    procNode = body[0] # unwrap single statement
  else:
    procNode = body

  if procNode.kind != nnkLambda:
    error "registerReqFFI expects a lambda definition. Found: " & $procNode.kind

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

proc buildFfiDeleteReqProc(reqTypeName: NimNode, fields: seq[NimNode]): NimNode =
  ## Generates:
  ## proc ffiDeleteReq(self: ptr <reqTypeName>) =
  ##   deallocShared(self[].<cstringField>)
  ##   deallocShared(self)

  # Build the body
  var body = newStmtList()
  for f in fields:
    if $f[1] == "cstring": # only dealloc cstring fields
      body.add newCall(
        ident("deallocShared"),
        newDotExpr(newTree(nnkDerefExpr, ident("self")), ident($f[0])),
      )

  # Always free the whole object at the end
  body.add newCall(ident("deallocShared"), ident("self"))

  # Build the parameter: (self: ptr <reqTypeName>)
  let selfParam = newIdentDefs(ident("self"), newTree(nnkPtrTy, reqTypeName))

  # Build the proc definition
  result = newProc(
    name = postfix(ident("ffiDeleteReq"), "*"),
    params = @[newEmptyNode()] & @[selfParam], # ✅ properly wrapped in a sequence
    body = body,
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
    error "registerReqFFI expects a lambda definition. Found: " & $procNode.kind

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

macro registerReqFFI*(reqTypeName, reqHandler, body: untyped): untyped =
  ## Registers a request that will be handled by the ffi thread.
  ## The request should be sent from the ffi consumer thread.
  ##

  # Extract lambda params to generate fields
  let fields = extractFieldsFromLambda(body)

  let typeDef = buildRequestType(reqTypeName, body)
  let ffiNewReqProc = buildFfiNewReqProc(reqTypeName, body)
  let processProc = buildProcessFFIRequestProc(reqTypeName, reqHandler, body)
  let addNewReqToReg = addNewRequestToRegistry(reqTypeName, reqHandler)
  let deleteProc = buildFfiDeleteReqProc(reqTypeName, fields)
  result = newStmtList(typeDef, ffiNewReqProc, deleteProc, processProc, addNewReqToReg)

  echo "Registered FFI request: " & result.repr

macro processReq*(reqType: typed, args: varargs[untyped]): untyped =
  ## Expands T.processReq(a,b,...) into the sendRequest boilerplate.

  # Collect the passed arguments as NimNodes
  var callArgs = @[reqType, ident("callback"), ident("userData")]
  for a in args:
    callArgs.add a

  # Build: ffiNewReq(reqType, callback, userData, arg1, arg2, ...)
  let newReqCall = newCall(ident("ffiNewReq"), callArgs)

  # Build: ffi_context.sendRequestToFFIThread(ctx, <newReqCall>)
  let sendCall = newCall(
    newDotExpr(ident("ffi_context"), ident("sendRequestToFFIThread")),
    ident("ctx"),
    newReqCall,
  )

  result = quote:
    block:
      let res = `sendCall`
      if res.isErr:
        let msg = "error in sendRequestToFFIThread: " & res.error
        callback(RET_ERR, unsafeAddr msg[0], cast[csize_t](msg.len), userData)
        return RET_ERR
      return RET_OK
