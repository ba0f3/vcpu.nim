import vcpu, parseutils

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
    let data = readFile(path)
    if not cpu.loadCode(data.cstring, data.len):
      return 1
    cpu.regs.R[0] = cpu.parseInput(r0)
    cpu.regs.R[1] = cpu.parseInput(r1)
    cpu.regs.R[2] = cpu.parseInput(r2)
    cpu.regs.R[3] = cpu.parseInput(r3)
    cpu.regs.R[4] = cpu.parseInput(r4)
    cpu.regs.R[5] = cpu.parseInput(r5)
    cpu.regs.R[6] = cpu.parseInput(r6)
    cpu.regs.R[7] = cpu.parseInput(r7)


    cpu.run()
    echo cpu.regs
  result = 0

import cligen; dispatch(fun)
