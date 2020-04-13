import unittest, streams
include vcpu


suite "vcpu tests":
  var cpu = initVCPU()
  test "test mov":
    cpu.regs.R[1] = 5
    cpu.regs.R[2] = 1

    let code = [byte MOV, 0x01, 0x02, HALT]
    assert cpu.loadCode(code.unsafeAddr, sizeof(code)), "load code"
    cpu.run()

    assert cpu.regs.R[1] == 1

  test "test movmb":
    cpu.reset()
    cpu.regs.R[2] = 6
    let code = [byte MOVMB, 0x02, 0x05, 0x00, HALT, 0x03, 0x00]
    assert cpu.loadCode(code.unsafeAddr, sizeof(code)), "load code"
    cpu.run()

    assert cpu.regs.R[2] == 3

  test "test movmw":
    cpu.reset()
    cpu.regs.R[2] = 6
    assert cpu.loadCode([byte MOVMW, 0x02, 0x05, 0x00, HALT, 0x03, 0x04])
    cpu.run()

    assert cpu.regs.R[2] == 0x0403