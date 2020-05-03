import tables, common, hashes, helpers

type Function* = proc(cpu: VCPU) {.nimcall.}

var
  functions_map = initTable[Hash, Function]()
  exported_functions*: seq[string]

proc getFuncAddr*(name: string): int = hash(name)
proc getFunc*(h: Hash): Function = functions_map[h]

proc register*(name: string, f: Function) =
  let add = getFuncAddr(name)
  if functions_map.hasKey(add) or exported_functions.contains(name):
    raise newException(ValueError, "function " & name & " is already registered")

  functions_map[add] = f
  exported_functions.add(name)

proc nim_echo*(cpu: VCPU) =
  let
    code = addr cpu.code
    mem = cpu.pop()

  echo $cast[cstring](code + mem)

proc open*(cpu: VCPU) =
  discard

register("echo", nim_echo)
register("open", open)