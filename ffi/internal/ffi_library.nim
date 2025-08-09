import std/macros, strformat

macro declareLibrary*(libraryName: static[string]): untyped =
  result = newStmtList()

  {.pragma: exported, exportc, cdecl, raises: [].}
  {.pragma: callback, cdecl, raises: [], gcsafe.}
  {.passc: "-fPIC".}

  when defined(linux):
    ## Generates {.passl: "-Wl,-soname,libwaku.so".}
    let soName = fmt"-Wl,-soname,lib{libraryName}.so"
    result.add(nnkPragmaStmt.newTree(ident"passl", newStrLitNode(soName)))

  ## proc lib{libraryName}NimMain() {.importc.}
  let procName = ident(fmt"lib{libraryName}NimMain")
  let importcPragma = nnkPragma.newTree(ident"importc")
  let procDef = newProc(
    name = procName,
    params = @[],
    pragmas = importcPragma,
    body = newEmptyNode(),
    returnType = newEmptyNode(), # no return value
  )
  result.add(procDef)

################################################################################
### Library setup

# To control when the library has been initialized
var initialized: Atomic[bool]

if defined(android):
  # Redirect chronicles to Android System logs
  when compiles(defaultChroniclesStream.outputs[0].writer):
    defaultChroniclesStream.outputs[0].writer = proc(
        logLevel: LogLevel, msg: LogOutputStr
    ) {.raises: [].} =
      echo logLevel, msg

proc initializeLibrary() {.exported.} =
  if not initialized.exchange(true):
    ## Every Nim library needs to call `<yourprefix>NimMain` once exactly, to initialize the Nim runtime.
    ## Being `<yourprefix>` the value given in the optional compilation flag --nimMainPrefix:yourprefix
    libwakuNimMain()
  when declared(setupForeignThreadGc):
    setupForeignThreadGc()
  when declared(nimGC_setStackBottom):
    var locals {.volatile, noinit.}: pointer
    locals = addr(locals)
    nimGC_setStackBottom(locals)
