import streams, macros, strutils

const
  DATA_SIZE = 128
  STACK_SIZE = 64

type
  BYTE* = byte
  WORD* = uint16
  DWORD* = uint32

  REGISTERS = object
    R*: array[8, DWORD]
    ZF {.bitsize:1.}: uint8
    CF {.bitsize:1.}: uint8
    PC: DWORD
    SP: DWORD

  VCPU = object
    code: array[DATA_SIZE, BYTE]
    codeLen: DWORD
    stack: array[STACK_SIZE, DWORD]
    regs*: REGISTERS

  Regs* {.size: 1.} = enum
    R0, R1, R2, R3, R4, R5, R6, R7

  OpCode* = enum
    NOP = 0x00,
    MOV, MOVMB, MOVMW, MOVB, MOVW, MOVBM, MOVWM,
    MOVMRB, MOVMRW, MOVMD, MOVD, MOVDM, MOVMRD,
    MOVRMB, MOVRMW, MOVRMD, #MOVVMB, MOVVMW, MOVVMD,
    JMP = 0x20, JZ, JNZ, JAE, JBE, JB, JA,
    ADVR, ADRR, ADRRL, SUBVR, SUBRR, SUBRRL,
    XOR, XORL,
    NOT, NOTL,
    ADVRD, SUBVRD,
    CMP = 0x50, CMPL,
    INC = 0x60, DEC, CALL, RET,
    PUSH = 0x90, POP,
    PRNT = 0xb0, PRNTX, PRNTS,
    HALT = 0xde

  BufferOverflowException* = object of OSError

converter opcode2byte*(o: OpCode): BYTE = o.BYTE
converter regs2byte*(o: Regs): BYTE = o.BYTE

template error(s: string) =
  echo "ERR: ", s
  break


template exception() = error("VCPU CRASH!!")
template LOW(x: auto): BYTE = cast[ptr BYTE](x.unsafeAddr)[]

macro dump(args: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  when defined(dump):
    result.add newCall("write", newIdentNode("stdout"), newStrLitNode("[VCPU] "))
    for n in args:
      result.add newCall("write", newIdentNode("stdout"), n)
      result.add newCall("write", newIdentNode("stdout"), newStrLitNode(" "))
    result.add newCall("write", newIdentNode("stdout"), newStrLitNode("\n"))

proc read[T](cpu: var VCPU, output: var T) =
  copyMem(addr output, addr cpu.code[cpu.regs.PC], sizeof(T))
  inc(cpu.regs.PC, sizeof(T))

proc read*[T](cpu: var VCPU, output: var T, pos: WORD|DWORD) =
  ## Read data directly from VCPU memory
  copyMem(addr output, addr cpu.code[pos], sizeof(T))

proc write*[T](cpu: var VCPU, input: T, pos: WORD|DWORD) =
  ## Write data directly into VCPU memory
  if pos.int + sizeof(T) >= DATA_SIZE:
    raise newException(BufferOverflowException, "not enough free space")
  copyMem(addr cpu.code[pos], input.unsafeAddr, sizeof(T))

proc reset*(cpu: var VCPU) =
  for i in 0..<8:
    cpu.regs.R[i] = 0
  cpu.regs.PC = 0
  cpu.regs.SP = STACK_SIZE
  cpu.regs.ZF = 0
  cpu.regs.CF = 0
  zeroMem(addr cpu.code, DATA_SIZE)
  cpu.codeLen = 0
  zeroMem(addr cpu.stack, sizeof(cpu.stack))

proc initVCPU*(): VCPU =
  result.reset()

proc loadCode*(cpu: var VCPU, code: pointer, codeLen: int): bool =
  dump "loading code, len:", codeLen
  copyMem(addr cpu.code, code, codeLen)
  cpu.codeLen = codeLen.DWORD
  return true

proc loadCode*(cpu: var VCPU, code: openArray[byte]): bool {.inline.} =
  cpu.loadCode(unsafeAddr code, code.len)

proc loadCode*(cpu: var VCPU, stream: Stream): bool {.inline.} =
  let code = stream.readAll()
  cpu.loadCode(code.cstring, code.len)

proc addCode*(cpu: var VCPU, code: pointer, codeLen: DWORD): DWORD =
  dump "adding code, len:", codeLen
  if cpu.codeLen + codeLen >= DATA_SIZE.DWORD:
    raise newException(BufferOverflowException, "buffer overflow")
  result = cpu.codeLen
  copyMem(addr cpu.code[result], code, codeLen)
  cpu.codeLen += codeLen

proc addCode*(cpu: var VCPU, code: openArray[byte]): DWORD {.inline.} =
  cpu.addCode(unsafeAddr code, code.len.DWORD)

proc addCode*(cpu: var VCPU, stream: Stream): DWORD {.inline.} =
  let code = stream.readAll()
  cpu.addCode(code.cstring, code.len.DWORD)

proc addInput*[T: BYTE|WORD|DWORD|string](cpu: var VCPU, input: T): DWORD {.inline.} =
  ## Add user input value, returns its address in memory
  when T is string:
    result = cpu.addCode(input.cstring, input.len.DWORD)
    discard cpu.addInput(0.BYTE)
  else:
    cpu.addCode(input.unsafeAddr, sizeof(T).DWORD)

proc jmp*(cpu: var VCPU, `addr`: DWORD): bool =
  ## Jump to specified location in CPU memory
  if `addr` < cpu.codeLen:
    cpu.regs.PC = `addr`
    result = true

template push(cpu: var VCPU, value: DWORD) =
  dec(cpu.regs.SP)
  if cpu.regs.SP == 0xffffffff.DWORD:
    error "stack overflow"
  cpu.stack[cpu.regs.SP] = value

proc pop(cpu: var VCPU): DWORD {.inline.} =
  if cpu.stack[cpu.regs.SP] == STACK_SIZE:
    echo "ERR: stack underflow"
  result = cpu.stack[cpu.regs.SP]
  inc(cpu.regs.SP)

proc run*(cpu: var VCPU) =
  var
    op: OpCode
    b0, b1, b2: BYTE
    w0, w1: WORD
    d0, d1: DWORD

  while true:
    if cpu.regs.PC >= cpu.codeLen: exception
    cpu.read(op)
    case op
    of NOP:
      dump op
      discard
    of HALT:
      dump op
      break
    of MOV:
      # move from register to register
      cpu.read(b0)
      cpu.read(b1)
      dump op, b0.Regs, b1.Regs
      if (b0 >= 0 and b0 <= 7) and (b1 >= 0 and b1 <= 7):
        cpu.regs.R[b0] = cpu.regs.R[b1]
      else:
        error "invalid register"
    of MOVMB:
      # move and extend byte from memory to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      if w0 >= cpu.codeLen: exception
      dump op, b0.Regs, w0
      cpu.read(w1, w0)
      cpu.regs.R[b0] = w1
    of MOVMW:
      #  move and extend word from memory to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      if w0 >= cpu.codeLen: exception
      dump op, b0.Regs, w0
      cpu.read(w1, w0)
      cpu.regs.R[b0] = w1
    of MOVB:
      # move and extend byte to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      dump op, b0.Regs, b1
      cpu.regs.R[b0] = b1
    of MOVW:
      # move and extend word to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump MOVW, b0.Regs, w0
      cpu.regs.R[b0] = w0
    of MOVBM:
      #  move byte from register to memory location
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump op, b0.Regs, w0
      if w0 >= cpu.codeLen:
        exception
      cpu.write(cpu.regs.R[b0].BYTE, w0)
    of MOVWM:
      # move word from register to memory location
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump op, b0.Regs, w0
      if w0 >= cpu.codeLen: exception
      cpu.write(cpu.regs.R[b0], w0)
    of MOVMRB:
      # move and extend byte from memory to register get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      dump op, b0.Regs, b1.Regs
      if b1 > 8: exception
      if cpu.regs.R[b1] >= cpu.codeLen: exception
      cpu.read(b2, cpu.regs.R[b1])
      cpu.regs.R[b0] = b2
    of MOVMRW:
      # move and extend word from memory to register get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      dump op, b0.Regs, b1.Regs
      if b1 > 8: exception
      if cpu.regs.R[b1] >= cpu.codeLen: exception
      cpu.read(w0, cpu.regs.R[b1])
      cpu.regs.R[b0] = w0
    of MOVMD:
      # move double word from memory to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(d0)
      dump op, b0.Regs, d0
      if d0 >= cpu.codeLen: exception
      cpu.regs.R[b0] = d0
    of MOVD:
      # move value to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(d0)
      dump op, b0.Regs, d0
      cpu.regs.R[b0] = d0
    of MOVDM:
      # move double word from register to memory location
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump op, b0.Regs, w0
      if w0 >= cpu.codeLen: exception
      cpu.write(cpu.regs.R[b0], w0)
    of MOVMRD:
      # move double word from memory to register get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      if cpu.regs.R[b1] >= cpu.codeLen: exception
      cpu.read(d0, cpu.regs.R[b1])
      cpu.regs.R[b0] = d0
    of MOVRMB:
      # move byte from register to memory, get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs, "; dst =", cpu.regs.R[b0], " v =", cpu.regs.R[b1]
      if cpu.regs.R[b0] >= cpu.codeLen: exception
      cpu.write(cpu.regs.R[b1].BYTE, cpu.regs.R[b0])
    of MOVRMW:
      # move word from register to memory, get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      if cpu.regs.R[b0] >= cpu.codeLen: exception
      cpu.write(cpu.regs.R[b1].WORD, cpu.regs.R[b0])
    of MOVRMD:
      # move double word from register to memory, get addr from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      if cpu.regs.R[b0] >= cpu.codeLen: exception
      cpu.write(cpu.regs.R[b1], cpu.regs.R[b0])
    #of MOVVMB:
    #  # move byte from register to memory, get value from register
    #  cpu.read(b0)
    #  if b0 > 8: exception
    #  cpu.read(b1)
    #  if b1 > 8: exception
    #  dump op, b0.Regs, b1.Regs
    #  if cpu.regs.R[b0] >= cpu.codeLen: exception
    #  cpu.write(cpu.regs.R[b1].BYTE, cpu.regs.R[b0])
    of JMP:
      # unconditional jump
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      cpu.regs.PC = w0
    of JZ:
      # jump if equal
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 1:
        cpu.regs.PC = w0
    of JNZ:
      # jump if not equal
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 0:
        cpu.regs.PC = w0
    of JAE:
      # jump if above or equal
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 1 or cpu.regs.CF == 0:
        cpu.regs.PC = w0
    of JBE:
      # jump if below or equal
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 1 or cpu.regs.CF == 1:
        cpu.regs.PC = w0
    of JB:
      # jump if below
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 0 and cpu.regs.CF == 1:
        cpu.regs.PC = w0
    of JA:
      # jump if above
      cpu.read(w0)
      dump op, w0
      if w0 > cpu.codeLen: exception
      if cpu.regs.ZF == 0 and cpu.regs.CF == 0:
        cpu.regs.PC = w0
    of ADVR:
      # Add word value to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump op, b0.Regs, w0
      w1 = cpu.regs.R[b0].WORD + w0
      cpu.regs.ZF = if w1 == 0: 1 else: 0
      cpu.regs.CF = if w1 < cpu.regs.R[b0]: 1 else: 0
      cpu.regs.R[b0] = w1
    of ADRR:
      # Add two registers and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      d0 = cpu.regs.R[b0]
      d1 = cpu.regs.R[b1]
      d1 += d0
      cpu.regs.R[b0] = d1
      cpu.regs.ZF = if d1 == 0: 1 else: 0
      cpu.regs.CF = if d1 < d0: 1 else: 0
    of ADRRL:
      # Add two registers (low byte) and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      b2 = cpu.regs.R[b0].BYTE
      b1 = cpu.regs.R[b1].BYTE
      b1 = b1 + b2
      cpu.regs.R[b0] = b1
      cpu.regs.ZF = if b1 == 0: 1 else: 0
      cpu.regs.CF = if b1 < b2: 1 else: 0
    of SUBVR:
      # Substract word value from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(w0)
      dump op, b0.Regs, w0
      w1 = cpu.regs.R[b0].WORD - w0
      cpu.regs.ZF = if w1 == 0: 1 else: 0
      cpu.regs.CF = if w1 > cpu.regs.R[b0]: 1 else: 0
      cpu.regs.R[b0] = w1
    of SUBRR:
      # Substract two registers and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      d0 = cpu.regs.R[b0]
      d1 = cpu.regs.R[b1]
      d1 -= d0
      cpu.regs.R[b0] = d1
      cpu.regs.ZF = if d1 == 0: 1 else: 0
      cpu.regs.CF = if d1 > d0: 1 else: 0
    of SUBRRL:
      # Substract two registers (low byte) and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      b2 = cpu.regs.R[b0].BYTE
      b1 = cpu.regs.R[b1].BYTE
      b1 = b1 - b2
      cpu.regs.R[b0] = b1
      cpu.regs.ZF = if b1 == 0: 1 else: 0
      cpu.regs.CF = if d1 > b2: 1 else: 0
    of XOR:
      # Xor two registers and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      d0 = cpu.regs.R[b0]
      d1 = cpu.regs.R[b1]
      dump op, b0.Regs, b1.Regs, ";", d0, "^", d1, "=", d0 xor d1
      d0 = d0 xor d1
      cpu.regs.ZF = if d0 == 0: 1 else: 0
      cpu.regs.CF = 0
      cpu.regs.R[b0] = d0
    of XORL:
      # Xor two registers (lower bytes) and save result in first
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      b2 = cpu.regs.R[b0].BYTE
      b1 = cpu.regs.R[b1].BYTE
      b2 = b2 xor b1
      cpu.regs.ZF = if b2 == 0: 1 else: 0
      cpu.regs.CF = 0
      cpu.regs.R[b0] = b2
    of NOT:
      # Bitwise not on value in a register and save result in this register
      cpu.read(b0)
      if b0 > 8: exception
      dump op, b0.Regs
      cpu.regs.R[b0] = not cpu.regs.R[b0]
    of NOTL:
      # Bitwise not on value in a register (lower bytes) and save result in this register
      cpu.read(b0)
      if b0 > 8: exception
      #cast[ptr BYTE](addr cpu.regs.R[b0])[] = not cpu.regs.R[b0]
    of ADVRD:
      # Add double word value to register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(d0)
      dump op, b0.Regs, d0
      d1 = cpu.regs.R[b0] + d0
      cpu.regs.ZF = if d1 == 0: 1 else: 0
      cpu.regs.CF = if d1 < cpu.regs.R[b0]: 1 else: 0
      cpu.regs.R[b0] = d1
    of SUBVRD:
      # Substract double word value from register
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(d0)
      dump op, b0.Regs, d0
      d1 = cpu.regs.R[b0] - d0
      cpu.regs.ZF = if d1 == 0: 1 else: 0
      cpu.regs.CF = if d1 > cpu.regs.R[b0]: 1 else: 0
      cpu.regs.R[b0] = d1
    of CMP:
      # compare two registers
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      d0 = cpu.regs.R[b0]
      d1 = cpu.regs.R[b1]
      cpu.regs.ZF = if d1 == d0: 1 else: 0
      cpu.regs.CF = if d1 > d0: 1 else: 0
    of CMPL:
      # compare two registers (lower byte)
      cpu.read(b0)
      if b0 > 8: exception
      cpu.read(b1)
      if b1 > 8: exception
      dump op, b0.Regs, b1.Regs
      b0  = LOW(cpu.regs.R[b0])
      b1  = LOW(cpu.regs.R[b1])
      cpu.regs.ZF = if b0 == b0: 1 else: 0
      cpu.regs.CF = if b1 > b0: 1 else: 0
    of INC:
      cpu.read(b0)
      dump op, b0.Regs
      if b0 > 8: exception
      inc(cpu.regs.R[b0])
    of DEC:
      cpu.read(b0)
      dump op, b0.Regs
      if b0 > 8: exception
      dec(cpu.regs.R[b0])
    of CALL:
      cpu.read(w0)
      dump op, w0
      cpu.push(w0)
    of RET:
      dump op
      cpu.regs.PC = cpu.pop()
    of PUSH:
      cpu.read(b0)
      if b0 > 8: exception
      dump op, b0.Regs
      cpu.push(cpu.regs.R[b0])
    of POP:
      cpu.read(b0)
      dump op, b0.Regs
      if b0 > 8: exception
      if cpu.stack[cpu.regs.SP] == STACK_SIZE:
        error "stack underflow"
      cpu.regs.R[b0] = cpu.pop()
    of PRNT:
      dump op
      if cpu.regs.SP == STACK_SIZE: exception
      echo cpu.code[cpu.stack[cpu.regs.SP]].char
      inc(cpu.regs.SP)
    of PRNTX:
      dump op
      if cpu.regs.SP == STACK_SIZE:
        error "stack underflow"
      echo "0x", cpu.code[cpu.stack[cpu.regs.SP]].toHex()
      inc(cpu.regs.SP)
    of PRNTS:
      dump op
      if cpu.regs.SP == STACK_SIZE:
        error "stack underflow"
      echo cast[cstring](addr cpu.code[cpu.stack[cpu.regs.SP]])
      inc(cpu.regs.SP)