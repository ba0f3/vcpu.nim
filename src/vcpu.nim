import with, streams

type
  BYTE* = byte
  WORD* = uint16
  DWORD* = uint32

  REGISTERS = object
    R: array[8, DWORD]
    ZF {.bitsize:1.}: uint8
    CF {.bitsize:1.}: uint8
    PC: DWORD
    SP: DWORD

  VCPU = object
    code: array[21, BYTE]
    codeLen: DWORD
    stack: array[256, DWORD]
    regs: REGISTERS

  Regs* {.size: 1.} = enum
    R0, R1, R2, R3, R4, R5, R6, R7

  OpCode* = enum
    NOP = 0x0, HALT
    MOV, MOVMB, MOVMW, MOVB, MOVW, MOVBM, MOVWM, MOVMRB, MOVMRW, MOVMD, MOVD, MOVDM, MOVMRD
    JMP = 0x20
    JZ, JNZ, JAE, JBE, JB, JA
    ADVR, ADRR, ADRRL, SUBVR, SUBRR, SUBRRL
    XOR, XORL
    NOT, NOTL
    ADVRD
    SUBVRD
    CMP = 0x50
    CMPL
    PUSH = 0x90
    POP
    POC = 0xa0
    POCN

converter opcode2byte*(o: OpCode): BYTE = o.BYTE
converter regs2byte*(o: Regs): BYTE = o.BYTE

template error(s: string) =
  echo "ERR: ", s
  break

template exception() = error("VCPU CRASH!!")
template LOW(x: auto): BYTE = cast[ptr BYTE](x.unsafeAddr)[]

proc read[T](cpu: var VCPU, output: var T) =
  #echo "PC: ", cpu.regs.PC, " op: ", cpu.code[cpu.regs.PC]
  copyMem(addr output, addr cpu.code[cpu.regs.PC], sizeof(T))
  inc(cpu.regs.PC, sizeof(T))

proc readAt[T](cpu: var VCPU, pos: WORD|DWORD, output: var T) =
  copyMem(addr output, addr cpu.code[pos], sizeof(T))

proc writeAt[T](cpu: var VCPU, pos: WORD|DWORD, input: T) =
  copyMem(addr cpu.code[pos], input.unsafeAddr, sizeof(T))

proc reset*(cpu: var VCPU) =
  for i in 0..<8:
    cpu.regs.R[i] = 0
  cpu.regs.PC = 0
  cpu.regs.SP = DWORD(sizeof(cpu.stack) / sizeof(DWORD))
  cpu.regs.ZF = 0
  cpu.regs.CF = 0
  zeroMem(addr cpu.code, sizeof(cpu.code))
  zeroMem(addr cpu.stack, sizeof(cpu.stack))

proc initVCPU*(): VCPU =
  result.reset()

proc loadCode*(cpu: var VCPU, code: pointer, codeLen: int): bool =
  copyMem(addr cpu.code, code, codeLen)
  cpu.codeLen = codeLen.DWORD
  return true

proc loadCode*(cpu: var VCPU, code: openArray[byte]): bool =
  cpu.codeLen = sizeof(code).DWORD
  copyMem(addr cpu.code, unsafeAddr code, cpu.codeLen)
  return true

proc loadCode*(cpu: var VCPU, stream: Stream): bool =
  let code = stream.readAll()
  cpu.loadCode(code.cstring, code.len)

proc run*(cpu: var VCPU) =
  var
    opcode: OpCode
    b0, b1, b2: BYTE
    w0, w1, w2: WORD
    d0, d1, d2: DWORD

  with cpu:
    while true:
      cpu.read(opcode)
      echo "OPCODE: ", opcode
      case opcode
      of NOP:
        #opcode = OpCode(opcode.int + 20)
        discard
      of HALT:
        break
      of MOV:
        cpu.read(b0)
        cpu.read(b1)
        if (b0 >= 0 and b0 <= 7) and (b1 >= 0 and b1 <= 7):
          regs.R[b0] = regs.R[b1]
        else:
          error "invalid register"
      of MOVMB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        cpu.readAt(w0, w1)
        regs.R[b0] = w1
      of MOVMW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        cpu.readAt(w0, w1)
        regs.R[b0] = w1
      of MOVB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        regs.R[b0] = b1
      of MOVW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(regs.R[b0])
      of MOVBM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen:
          exception
        cpu.writeAt(w0, byte(regs.R[b0]))
      of MOVWM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        cpu.writeAt(w0, regs.R[b0])
      of MOVMRB:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        cpu.readAt(regs.R[b1], b2)
        regs.R[b0] = b2
      of MOVMRW:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        cpu.readAt(regs.R[b1], w0)
        regs.R[b0] = w0
      of MOVMD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        if d0 >= codeLen: exception
        regs.R[b0] = d0
      of MOVD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        regs.R[b0] = w0
      of MOVDM:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        if w0 >= codeLen: exception
        cpu.writeAt(w0, regs.R[b0])
      of MOVMRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        cpu.readAt(regs.R[b1], d0)
        regs.R[b0] = d0
      of JMP:
        cpu.read(w0)
        if w0 > codeLen: exception
        regs.PC = w0
      of JZ:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1:
          regs.PC = w0
      of JNZ:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1:
          regs.PC = w0
      of JAE:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 0:
          regs.PC = w0
      of JBE:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 1:
          regs.PC = w0
      of JB:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 1:
          regs.PC = w0
      of JA:
        cpu.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 0:
          regs.PC = w0
      of ADVR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(w0)
        w1 = regs.R[b0].WORD + w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of ADRR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 + d1
        regs.R[b0] = d2
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = if d2 < d0: 1 else: 0
      of ADRRL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
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
        w1 = regs.R[b0].WORD - w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of SUBRR:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 - d1
        regs.R[b0] = d2
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = if d2 > d0: 1 else: 0
      of SUBRRL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
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
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 xor d1
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = d2
      of XORL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b2 = b2 xor b1
        regs.ZF = if b2 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = b2
      of NOT:
        cpu.read(b0)
        if b0 > 8: exception
        regs.R[b0] = not regs.R[b0]
      of NOTL:
        cpu.read(b0)
        if b0 > 8: exception
        #cast[ptr BYTE](addr regs.R[b0])[] = not regs.R[b0]
      of ADVRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        d1 = regs.R[b0] + d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of SUBVRD:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(d0)
        d1 = regs.R[b0] - d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of CMP:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        regs.ZF = if d1 == d0: 1 else: 0
        regs.CF = if d1 > d0: 1 else: 0
      of CMPL:
        cpu.read(b0)
        if b0 > 8: exception
        cpu.read(b1)
        if b1 > 8: exception
        b0  = LOW(regs.R[b0])
        b1  = LOW(regs.R[b1])
        regs.ZF = if b0 == b0: 1 else: 0
        regs.CF = if b1 > b0: 1 else: 0
      of PUSH:
        cpu.read(b0)
        if b0 > 8: exception
        dec(regs.SP)
        if regs.SP == 0xffffffff.DWORD:
          error "stack overflow"
        stack[regs.SP] = regs.R[b0]
      of POP:
        cpu.read(b0)
        if b0 > 8: exception
        if stack[regs.SP] == DWORD(sizeof(stack)/sizeof(DWORD)):
          error "stack underflow"
        regs.R[b0] = stack[regs.SP]
        inc(regs.SP)
      of POC:
        if regs.SP == DWORD(sizeof(stack)/sizeof(DWORD)): exception
        cpu.readAt(stack[regs.SP], b0)
        discard writeBuffer(stdout, addr b0, sizeof(b0))
      of POCN:
        if regs.SP == DWORD(sizeof(stack)/sizeof(DWORD)):
          error "stack underflow"
        cpu.readAt(stack[regs.SP], b0)
        echo $b0
      #else:
      #  discard
