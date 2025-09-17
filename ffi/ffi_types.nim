import chronos

################################################################################
### Exported types

type FFICallBack* = proc(
  callerRet: cint, msg: ptr cchar, len: csize_t, userData: pointer
) {.cdecl, gcsafe, raises: [].}

const RET_OK*: cint = 0
const RET_ERR*: cint = 1
const RET_MISSING_CALLBACK*: cint = 2

### End of exported types
################################################################################

################################################################################
### FFI utils

type FFIRequestProc* =
  proc(request: pointer, reqHandler: pointer): Future[Result[string, string]] {.async.}

template foreignThreadGc*(body: untyped) =
  when declared(setupForeignThreadGc):
    setupForeignThreadGc()

  body

  when declared(tearDownForeignThreadGc):
    tearDownForeignThreadGc()

type onDone* = proc()

### End of FFI utils
################################################################################
