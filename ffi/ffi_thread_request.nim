## This file contains the base message request type that will be handled.
## The requests are created by the main thread and processed by
## the Waku Thread.

import std/macros
import std/json, results
import chronos, chronos/threadsync
import ./ffi_types, ./internal/ffi_macro
import waku/factory/waku
import ../../../library/waku_thread_requests/requests/peer_manager_request
# type FFIRequestProcessor* =
#   concept
#       proc process[R](
#         T: type FFIThreadRequest, request: ptr FFIThreadRequest, reqHandler: ptr R
#       )

type FFIThreadRequest* = object
  callback: FFICallBack
  userData: pointer
  reqId: uint
  reqContent*: pointer

proc init*(
    T: typedesc[FFIThreadRequest],
    callback: FFICallBack,
    userData: pointer,
    reqId: uint,
    reqContent: pointer,
): ptr type T =
  var ret = createShared(FFIThreadRequest)
  ret[].callback = callback
  ret[].userData = userData
  ret[].reqId = reqId
  ret[].reqContent = reqContent
  return ret

proc deleteRequest(request: ptr FFIThreadRequest) =
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

proc nilProcess(reqId: uint): Future[Result[string, string]] {.async.} =
  return err("This request type is not implemented: " & $reqId)

ffiGenerateProcess()

# dumpAstGen:
#   proc process*[R](
#       T: type FFIThreadRequest, request: ptr FFIThreadRequest, reqHandler: ptr R
#   ) {.async.} =
#     # reqHandler represents the object that actually processes the request.
#     let retFut =
#       case request[].reqId
#       of 1:
#         cast[ptr PeerManagementRequest](request[].reqContent).process(reqHandler)
#       else:
#         nilProcess(request[].reqId)

#     handleRes(await retFut, request)
