import unittest, streams
include vcpu


suite "vcpu tests":
  var cpu = initVCPU()
  test "init vpu":
    var code = newStringStream()
    code.write(NOP)
    code.write(NOP)
    code.write(NOP)
    code.write(HALT)
    code.setPosition(0)
    assert cpu.loadCode(code), "load code"
  test "test run":
    cpu.run()