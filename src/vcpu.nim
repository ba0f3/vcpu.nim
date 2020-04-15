import with, streams, macros, strutils

const
  DATA_SIZE = 64
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
    MOVMRB, MOVMRW, MOVMD, MOVD, MOVDM, MOVMRD
    JMP = 0x20
    JZ, JNZ, JAE, JBE, JB, JA
    ADVR, ADRR, ADRRL, SUBVR, SUBRR, SUBRRL
    XOR, XORL
    NOT, NOTL
    ADVRD, SUBVRD
    CMP = 0x50, CMPL
    PUSH = 0x90, POP
    PRNT = 0xb0, PRNTX, PRNTS
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
  when not defined(release):
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
  when not defined(release):
    echo "load code len: ", codeLen
  copyMem(addr cpu.code, code, codeLen)
  cpu.codeLen = codeLen.DWORD
  return true

proc loadCode*(cpu: var VCPU, code: openArray[byte]): bool {.inline.} =
  cpu.loadCode(unsafeAddr code, code.len)

proc loadCode*(cpu: var VCPU, stream: Stream): bool {.inline.} =
  let code = stream.readAll()
  cpu.loadCode(code.cstring, code.len)

proc addCode*(cpu: var VCPU, code: pointer, codeLen: DWORD): DWORD =
  when not defined(release):
    echo "add code len: ", codeLen
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

proc run*(cpu: var VCPU) =
  var
    opcode: OpCode
    b0, b1, b2: BYTE
    w0, w1: WORD
    d0, d1: DWORD

  with cpu:
    while true:
      if cpu.regs.PC >= codeLen: exception
      cpu.read(opcode)
      #when defined(x):
      #  echo "OPCODE: ", opcode
      case opcode
      of NOP:
        dump opcode
        #opcode = OpCode(opcode.int + 20)
        discard
      of HALT:
        dump opcode
        break
      of MOV:
        cpu.read(b0)
        cpu.read(b1)
        dump opcode, b0.Regs, b1.Regs
        if (b0 >= 0 and b0 <= 7) and (b1 >= 0 and b1 <= 7):
          regs.R[b0] = regs.R[b1]
        else:
          error "invalid register"
      of MOVMB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        dump opcode, b0.Regs, w0
        cpu.read(w1, w0)
        regs.R[b0] = w1
      of MOVMW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        dump opcode, b0.Regs, w0
        cpu.read(w1, w0)
        regs.R[b0] = w1
      of MOVB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        dump opcode, b0.Regs, b1
        regs.R[b0] = b1
      of MOVW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump MOVW, b0.Regs, w0
        regs.R[b0] = w0
      of MOVBM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump opcode, b0.Regs, w0
        if w0 >= codeLen:
          exception
        cpu.write(byte(regs.R[b0]), w0)
      of MOVWM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump opcode, b0.Regs, w0
        if w0 >= codeLen: exception
        cpu.write(regs.R[b0], w0)
      of MOVMRB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        dump opcode, b0.Regs, b1.Regs
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        cpu.read(b2, regs.R[b1])
        regs.R[b0] = b2
      of MOVMRW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        dump opcode, b0.Regs, b1.Regs
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        cpu.read(w0, regs.R[b1])
        regs.R[b0] = w0
      of MOVMD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        dump opcode, b0.Regs, d0
        if d0 >= codeLen: exception
        regs.R[b0] = d0
      of MOVD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        dump opcode, b0.Regs, d0
        if w0 >= codeLen: exception
        regs.R[b0] = d0
      of MOVDM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump opcode, b0.Regs, w0
        if w0 >= codeLen: exception
        cpu.write(regs.R[b0], w0)
      of MOVMRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        if regs.R[b1] >= codeLen: exception
        cpu.read(d0, regs.R[b1])
        regs.R[b0] = d0
      of JMP:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        regs.PC = w0
      of JZ:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 1:
          regs.PC = w0
      of JNZ:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 1:
          regs.PC = w0
      of JAE:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 0:
          regs.PC = w0
      of JBE:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 1:
          regs.PC = w0
      of JB:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 1:
          regs.PC = w0
      of JA:
        cpu.read(w0)
        dump opcode, w0
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 0:
          regs.PC = w0
      of ADVR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump opcode, b0.Regs, w0
        w1 = regs.R[b0].WORD + w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of ADRR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d1 += d0
        regs.R[b0] = d1
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 < d0: 1 else: 0
      of ADRRL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b1 = b1 + b2
        regs.R[b0] = b1
        regs.ZF = if b1 == 0: 1 else: 0
        regs.CF = if b1 < b2: 1 else: 0
      of SUBVR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        dump opcode, b0.Regs, w0
        w1 = regs.R[b0].WORD - w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of SUBRR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d1 -= d0
        regs.R[b0] = d1
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 > d0: 1 else: 0
      of SUBRRL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b1 = b1 - b2
        regs.R[b0] = b1
        regs.ZF = if b1 == 0: 1 else: 0
        regs.CF = if d1 > b2: 1 else: 0
      of XOR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d0 = d0 xor d1
        regs.ZF = if d0 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = d0
      of XORL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b2 = b2 xor b1
        regs.ZF = if b2 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = b2
      of NOT:
        cpu.read(b0)
        if b0 > 8: exception
        dump opcode, b0.Regs
        regs.R[b0] = not regs.R[b0]
      of NOTL:
        cpu.read(b0)
        if b0 > 8: exception
        #cast[ptr BYTE](addr regs.R[b0])[] = not regs.R[b0]
      of ADVRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        dump opcode, b0.Regs, d0
        d1 = regs.R[b0] + d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of SUBVRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        dump opcode, b0.Regs, d0
        d1 = regs.R[b0] - d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of CMP:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        regs.ZF = if d1 == d0: 1 else: 0
        regs.CF = if d1 > d0: 1 else: 0
      of CMPL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        dump opcode, b0.Regs, b1.Regs
        b0  = LOW(regs.R[b0])
        b1  = LOW(regs.R[b1])
        regs.ZF = if b0 == b0: 1 else: 0
        regs.CF = if b1 > b0: 1 else: 0
      of PUSH:
        cpu.read(b0)
        if b0 > 8: exception
        dump opcode, b0.Regs
        dec(regs.SP)
        if regs.SP == 0xffffffff.DWORD:
          error "stack overflow"
        stack[regs.SP] = regs.R[b0]
      of POP:
        cpu.read(b0)
        dump opcode, b0.Regs
        if b0 > 8: exception
        if stack[regs.SP] == STACK_SIZE:
          error "stack underflow"
        regs.R[b0] = stack[regs.SP]
        inc(regs.SP)
      of PRNT:
        dump opcode
        if regs.SP == STACK_SIZE: exception
        echo code[stack[regs.SP]]
        inc(regs.SP)
      of PRNTX:
        dump opcode
        if regs.SP == STACK_SIZE:
          error "stack underflow"
        echo "0x", code[stack[regs.SP]].toHex()
        inc(regs.SP)
      of PRNTS:
        dump opcode
        if regs.SP == STACK_SIZE:
          error "stack underflow"
        #cpu.read(b0, stack[regs.SP])
        echo cast[cstring](addr code[stack[regs.SP]])
        inc(regs.SP)