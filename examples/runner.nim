import os, vcpu, strformat, streams

if paramCount() != 1:
  quit(&"Usage: {paramStr(0)} file")

let s = newFileStream(paramStr(1))
var cpu = initVCPU()
if cpu.loadCode(s):
  cpu.run()

