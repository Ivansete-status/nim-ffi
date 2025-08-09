type LifeCycleRequest {.ffi.} = object
  discard

type PeerManagerRequest {.ffi.} = object
  discard

type PeerManagerRequest* = object
  reqType: 12123491 ## random int
  reqContent: pointer
  callback: WakuCallBack
  userData: pointer

## createShared
## process
## deallocShared
## 
## 
## 