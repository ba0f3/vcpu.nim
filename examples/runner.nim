import vcpu

proc fun(paths: seq[string], r0:DWORD=0, r1:DWORD=0, r2:DWORD=0, r3:DWORD=0, r4:DWORD=0, r5:DWORD=0, r6:DWORD=0, r7:DWORD=0, r8:DWORD=0): int=
  var cpu = initVCPU()
  for path in paths:
    cpu.reset()
    let data = readFile(path)
    if not cpu.loadCode(data.cstring, data.len):
      return 1
    if r0 != 0:
      cpu.regs.R[0] = r0
    if r1 != 0:
      cpu.regs.R[1] = r1
    if r2 != 0:
      cpu.regs.R[2] = r2
    if r3 != 0:
      cpu.regs.R[3] = r3
    if r4 != 0:
      cpu.regs.R[4] = r4
    if r5 != 0:
      cpu.regs.R[5] = r5
    if r6 != 0:
      cpu.regs.R[6] = r6
    if r7 != 0:
      cpu.regs.R[7] = r7
    cpu.run()
  result = 0

import cligen; dispatch(fun)
