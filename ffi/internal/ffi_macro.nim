import std/macros

import chronos

macro ffi*(p: typed, args: varargs[untyped]): untyped =
  ## p: the proc definition AST
  ## args: expected to be (RequestTypeValue, MsgTypeValue)

  if p.kind != nnkProcDef:
    error("ffi pragma can only be applied to proc definitions", p)

  if args.len != 2:
    error(
      "ffi pragma requires exactly two arguments: (RequestTypeValue, MsgTypeValue)", p
    )

  let reqType = args[0]
  let msgType = args[1]

  let origProc = p
  let origName = p.name
  let exportedName = origName

  result = newStmtList()
  result.add(origProc)

  # Build exported wrapper proc:
  let wrapperParams = newSeq[NimNode]()
  wrapperParams.add(newIdentDefs(ident("ctx"), newPtrType(ident("WakuContext"))))
  wrapperParams.add(newIdentDefs(ident("callback"), ident("WakuCallBack")))
  wrapperParams.add(newIdentDefs(ident("userData"), ident("pointer")))

  let wrapperPragmas = nnkPragma.newTree(ident("dynlib"), ident("exportc"))

  let wrapperBody = quote:
    initializeLibrary()
    checkLibwakuParams(ctx, callback, userData)
    handleRequest(
      ctx, `reqType`, PeerManagementRequest.createShared(`msgType`), callback, userData
    )
    return 0.cint

  let wrapperProc = newProc(
    name = exportedName,
    params = wrapperParams,
    pragmas = wrapperPragmas,
    body = wrapperBody,
    returnType = ident("cint"),
  )

  result.add(wrapperProc)
