{.pragma: exported, exportc, cdecl, raises: [].}
{.pragma: callback, cdecl, raises: [], gcsafe.}
{.passc: "-fPIC".}

import std/[options, atomics, os, net, locks]
import chronicles, chronos, chronos/threadsync, taskpools/channels_spsc_single, results
import ./ffi_types

type FFIContext* = object
  libraryName: cstring
  ffiThread: Thread[(ptr FFIContext)]
    # represents the main thread in charge of attending SDK consumer actions
  watchdogThread: Thread[(ptr FFIContext)]
    # monitors the FFI thread and notifies the FFI SDK consumer if it hangs
  lock: Lock
  reqChannel: ChannelSPSCSingle[ptr FFIThreadRequest]
  reqSignal: ThreadSignalPtr # to notify the FFI Thread that a new request is sent
  reqReceivedSignal: ThreadSignalPtr
    # to signal main thread, interfacing with the FFI thread, that FFI thread received the request
  userData*: pointer
  eventCallback*: pointer
  eventUserdata*: pointer
  running: Atomic[bool] # To control when the threads are running

const git_version* {.strdefine.} = "n/a"

template callEventCallback(ctx: ptr FFIContext, eventName: string, body: untyped) =
  if isNil(ctx[].eventCallback):
    error eventName & " - eventCallback is nil"
    return

  foreignThreadGc:
    try:
      let event = body
      cast[FFICallBack](ctx[].eventCallback)(
        RET_OK, unsafeAddr event[0], cast[csize_t](len(event)), ctx[].eventUserData
      )
    except Exception, CatchableError:
      let msg =
        "Exception " & eventName & " when calling 'eventCallBack': " &
        getCurrentExceptionMsg()
      cast[FFICallBack](ctx[].eventCallback)(
        RET_ERR, unsafeAddr msg[0], cast[csize_t](len(msg)), ctx[].eventUserData
      )

proc sendRequestToWakuThread*(
    ctx: ptr FFIContext,
    reqType: RequestType,
    reqContent: pointer,
    callback: FFICallBack,
    userData: pointer,
    timeout = InfiniteDuration,
): Result[void, string] =
  ctx.lock.acquire()
  # This lock is only necessary while we use a SP Channel and while the signalling
  # between threads assumes that there aren't concurrent requests.
  # Rearchitecting the signaling + migrating to a MP Channel will allow us to receive
  # requests concurrently and spare us the need of locks
  defer:
    ctx.lock.release()

  let req = FFIThreadRequest.createShared(reqType, reqContent, callback, userData)
  ## Sending the request
  let sentOk = ctx.reqChannel.trySend(req)
  if not sentOk:
    deallocShared(req)
    return err("Couldn't send a request to the waku thread: " & $req[])

  let fireSyncRes = ctx.reqSignal.fireSync()
  if fireSyncRes.isErr():
    deallocShared(req)
    return err("failed fireSync: " & $fireSyncRes.error)

  if fireSyncRes.get() == false:
    deallocShared(req)
    return err("Couldn't fireSync in time")

  ## wait until the Waku Thread properly received the request
  let res = ctx.reqReceivedSignal.waitSync(timeout)
  if res.isErr():
    deallocShared(req)
    return err("Couldn't receive reqReceivedSignal signal")

  ## Notice that in case of "ok", the deallocShared(req) is performed by the Waku Thread in the
  ## process proc. See the 'waku_thread_request.nim' module for more details.
  ok()

proc watchdogThreadBody(ctx: ptr FFIContext) {.thread.} =
  ## Watchdog thread that monitors the Waku thread and notifies the library user if it hangs.

  let watchdogRun = proc(ctx: ptr FFIContext) {.async.} =
    const WatchdogStartDelay = 10.seconds
    const WatchdogTimeinterval = 1.seconds
    const WakuNotRespondingTimeout = 3.seconds

    # Give time for the node to be created and up before sending watchdog requests
    await sleepAsync(WatchdogStartDelay)
    while true:
      await sleepAsync(WatchdogTimeinterval)

      if ctx.running.load == false:
        debug "Watchdog thread exiting because FFIContext is not running"
        break

      let wakuCallback = proc(
          callerRet: cint, msg: ptr cchar, len: csize_t, userData: pointer
      ) {.cdecl, gcsafe, raises: [].} =
        discard ## Don't do anything. Just respecting the callback signature.
      const nilUserData = nil

      trace "Sending watchdog request to FFI thread"

      sendRequestToWakuThread(
        ctx,
        RequestType.DEBUG,
        DebugNodeRequest.createShared(DebugNodeMsgType.CHECK_WAKU_NOT_BLOCKED),
        wakuCallback,
        nilUserData,
        WakuNotRespondingTimeout,
      ).isOkOr:
        error "Failed to send watchdog request to FFI thread", error = $error
        onWakuNotResponding(ctx)

  waitFor watchdogRun(ctx)

proc ffiThreadBody(ctx: ptr FFIContext) {.thread.} =
  ## FFI thread that attends library user API requests

  let ffiRun = proc(ctx: ptr FFIContext) {.async.} =
    var waku: Waku
    while true:
      await ctx.reqSignal.wait()

      if ctx.running.load == false:
        break

      ## Wait for a request from the ffi consumer thread
      var request: ptr FFIThreadRequest
      let recvOk = ctx.reqChannel.tryRecv(request)
      if not recvOk:
        error "ffi thread could not receive a request"
        continue

      ## Handle the request
      asyncSpawn FFIThreadRequest.process(request, addr waku)

      let fireRes = ctx.reqReceivedSignal.fireSync()
      if fireRes.isErr():
        error "could not fireSync back to requester thread", error = fireRes.error

  waitFor ffiRun(ctx)

proc createFFIContext*(libraryName: cstring): Result[ptr FFIContext, string] =
  ## This proc is called from the main thread and it creates
  ## the FFI working thread.
  var ctx = createShared(FFIContext, 1)
  ctx.reqSignal = ThreadSignalPtr.new().valueOr:
    return err("couldn't create reqSignal ThreadSignalPtr")
  ctx.reqReceivedSignal = ThreadSignalPtr.new().valueOr:
    return err("couldn't create reqReceivedSignal ThreadSignalPtr")
  ctx.lock.initLock()

  ctx.running.store(true)

  try:
    createThread(ctx.ffiThread, ffiThreadBody, ctx)
  except ValueError, ResourceExhaustedError:
    freeShared(ctx)
    return err("failed to create the Waku thread: " & getCurrentExceptionMsg())

  try:
    createThread(ctx.watchdogThread, watchdogThreadBody, ctx)
  except ValueError, ResourceExhaustedError:
    freeShared(ctx)
    return err("failed to create the watchdog thread: " & getCurrentExceptionMsg())

  return ok(ctx)

proc destroyFFIContext*(ctx: ptr FFIContext): Result[void, string] =
  ctx.running.store(false)

  let signaledOnTime = ctx.reqSignal.fireSync().valueOr:
    return err("error in destroyFFIContext: " & $error)
  if not signaledOnTime:
    return err("failed to signal reqSignal on time in destroyFFIContext")

  joinThread(ctx.ffiThread)
  joinThread(ctx.watchdogThread)
  ctx.lock.deinitLock()
  ?ctx.reqSignal.close()
  ?ctx.reqReceivedSignal.close()
  deallocShared(ctx.libraryName)
  freeShared(ctx)

  return ok()
