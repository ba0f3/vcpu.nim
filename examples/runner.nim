import vcpu, parseutils, strutils
import vcpu/assembler

proc parseInput(cpu: VCPU, input: string): DWORD =
  if input.len == 0:
    return 0.DWORD
  var value: int
  if parseInt(input, value) != 0:
    return value.DWORD
  elif parseHex(input, value) != 0:
    return value.DWORD
  else:
    return cpu.addInput(input)

proc fun(paths: seq[string], r0="", r1="", r2="", r3="", r4="", r5="", r6="", r7="", r8=""): int=
  let cpu = newVCPU()
  for path in paths:
    cpu.reset()
    var a = newAssembler()
    let data = a.compileString(readFile(path))
    if not cpu.loadCode(data.cstring, data.len):
      return 1
    cpu.setReg(R0, cpu.parseInput(r0))
    cpu.setReg(R1, cpu.parseInput(r1))
    cpu.setReg(R2, cpu.parseInput(r2))
    cpu.setReg(R3, cpu.parseInput(r3))
    cpu.setReg(R4, cpu.parseInput(r4))
    cpu.setReg(R5, cpu.parseInput(r5))
    cpu.setReg(R6, cpu.parseInput(r6))
    cpu.setReg(R7, cpu.parseInput(r7))

    result = cpu.run().int
  echo "exit code: ", result

import cligen; dispatch(fun)
