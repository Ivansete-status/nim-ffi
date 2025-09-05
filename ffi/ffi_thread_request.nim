## This file contains the base message request type that will be handled.
## The requests are created by the main thread and processed by
## the Waku Thread.

import std/[json, macros], results, tables
import chronos, chronos/threadsync
import ./ffi_types, ./internal/ffi_macro, ./alloc, ./ffi_context
import waku/factory/waku
# import ../../../library/waku_thread_requests/requests/peer_manager_request

type FFIThreadRequest* = object
  callback: FFICallBack
  userData: pointer
  reqId: cstring
  reqContent*: pointer

proc init*(
    T: typedesc[FFIThreadRequest],
    callback: FFICallBack,
    userData: pointer,
    reqId: cstring,
    reqContent: pointer,
): ptr type T =
  var ret = createShared(FFIThreadRequest)
  ret[].callback = callback
  ret[].userData = userData
  ret[].reqId = reqId.alloc()
  ret[].reqContent = reqContent
  return ret

proc deleteRequest(request: ptr FFIThreadRequest) =
  deallocShared(request[].reqId)
  deallocShared(request)

proc handleRes[T: string | void](
    res: Result[T, string], request: ptr FFIThreadRequest
) =
  ## Handles the Result responses, which can either be Result[string, string] or
  ## Result[void, string].

  defer:
    deleteRequest(request)

  if res.isErr():
    foreignThreadGc:
      let msg = "libwaku error: handleRes fireSyncRes error: " & $res.error
      request[].callback(
        RET_ERR, unsafeAddr msg[0], cast[csize_t](len(msg)), request[].userData
      )
    return

  foreignThreadGc:
    var msg: cstring = ""
    when T is string:
      msg = res.get().cstring()
    request[].callback(
      RET_OK, unsafeAddr msg[0], cast[csize_t](len(msg)), request[].userData
    )
  return

proc nilProcess(reqId: cstring): Future[Result[string, string]] {.async.} =
  return err("This request type is not implemented: " & $reqId)

proc process*[R](
    T: type FFIThreadRequest,
    request: ptr FFIThreadRequest,
    reqHandler: ptr R,
    registeredRequests: ptr Table[cstring, FFIRequestProc],
) {.async.} =
  let reqId = $request[].reqId

  let retFut =
    if not registeredRequests[].contains(reqId):
      nilProcess(request[].reqId)
    else:
      registeredRequests[][reqId](request[].reqContent, reqHandler)
  handleRes(await retFut, request)
