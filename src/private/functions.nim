import tables, hashes
import common, helpers

type Function* = proc(cpu: VCPU) {.nimcall.}

var functions_map = initTable[Hash, Function]()

proc getFuncAddr*(name: string): int = hash(name)
proc getFunc*(h: Hash): Function =
  if functions_map.hasKey(h):
    return functions_map[h]
  return nil

proc register*(name: string, f: Function) =
  let add = getFuncAddr(name)
  if functions_map.hasKey(add):
    raise newException(ValueError, "function " & name & " is already registered")

  functions_map[add] = f

proc nim_echo*(cpu: VCPU) =
  let
    code = addr cpu.code
    mem = cpu.pop()

  echo $cast[cstring](code + mem)

proc add*(cpu: VCPU) =
  let
    a = cpu.pop()
    b = cpu.pop()
  cpu.push(a + b)

register("echo", nim_echo)
register("add", add)