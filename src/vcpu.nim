import with, streams

type
  BYTE* = byte
  WORD* = uint16
  DWORD* = uint32

  REGISTERS = object
    R: array[8, DWORD]
    ZF {.bitsize:1.}: uint8
    CF {.bitsize:1.}: uint8
    #PC: DWORD
    SP: DWORD

  VCPU = object
    code: Stream
    codeLen: DWORD
    stack: array[256, DWORD]
    regs: REGISTERS

  OpCode* = enum
    NOP = 0x0
    HALT, MOV, MOVMB, MOVMW, MOVB, MOVW, MOVBM, MOVWM, MOVMRB, MOVMRW, MOVMD, MOVD, MOVDM, MOVMRD
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

template error(s: string) =
  echo "ERR: ", s
  break

template exception() = error("VCPU CRASH!!")

#proc `+`(dw: var DWORD): DWORD =
#  result = dw
#  inc(dw)

proc readAt[T](s: Stream, pos: WORD|DWORD, data: var T) =
  let curPos = s.getPosition()
  s.setPosition(pos.int)
  s.peek(data)
  s.setPosition(curPos)

proc writeAt[T](s: Stream, pos: WORD|DWORD, data: T) =
  let curPos = s.getPosition()
  s.setPosition(pos.int)
  s.write(data)
  s.setPosition(curPos)

template LOW(x: auto): BYTE = cast[ptr BYTE](x.unsafeAddr)[]


proc reset*(cpu: var VCPU) =
  for i in 0..<8:
    cpu.regs.R[i] = 0
  cpu.code = newStringStream()
  #cpu.regs.R[0] =

proc initVCPU*(): VCPU =
  result.code = newStringStream()
  result.reset()


proc loadCode*(cpu: var VCPU, code: pointer, codeLen: int): bool =
  cpu.code.writeData(code, codeLen)
  cpu.code.setPosition(0)
  cpu.codeLen = codeLen.DWORD
  return true

proc loadCode*(cpu: var VCPU, code: openArray[byte]): bool =
  cpu.code.write(code)
  cpu.code.setPosition(0)
  cpu.codeLen = sizeof(code).DWORD
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
      code.read(opcode)
      echo "OPCODE: ", opcode
      case opcode
      of NOP:
        opcode = OpCode(opcode.int + 20)
      of HALT:
        break
      of MOV:
        code.read(b0)
        code.read(b1)
        if (b0 >= 0 and b0 <= 7) and (b1 >= 0 and b1 <= 7):
          regs.R[b0] = regs.R[b1]
        else:
          error "invalid register"
      of MOVMB:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= sizeof(code).WORD: exception
        code.readAt(w0, w1)
        regs.R[b0] = w1
      of MOVMW:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= sizeof(code).WORD: exception
        code.readAt(w0, w1)
        regs.R[b0] = w1
      of MOVB:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        regs.R[b0] = b1
      of MOVW:
        code.read(b0)
        if b0 > 8: exception
        code.read(regs.R[b0])
      of MOVBM:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= sizeof(code).WORD:
          exception
        code.writeAt(w0, byte(regs.R[b0]))
      of MOVWM:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= sizeof(code).WORD: exception
        code.write(w0, regs.R[b0])
      of MOVMRB:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        code.readAt(regs.R[b1], b2)
        regs.R[b0] = b2
      of MOVMRW:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        code.readAt(regs.R[b1], w0)
        regs.R[b0] = w0
      of MOVMD:
        code.read(b0)
        if b0 > 8: exception
        code.read(d0)
        if d0 >= codeLen: exception
        regs.R[b0] = d0
      of MOVD:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= codeLen: exception
        regs.R[b0] = w0
      of MOVDM:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        if w0 >= codeLen: exception
        code.writeAt(w0, regs.R[b0])
      of MOVMRD:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        if regs.R[b1] >= codeLen: exception
        code.readAt(regs.R[b1], d0)
        regs.R[b0] = d0
      of JMP:
        code.read(w0)
        if w0 > codeLen: exception
        code.setPosition(w0.int)
      of JZ:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1:
          code.setPosition(w0.int)
      of JNZ:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1:
          code.setPosition(w0.int)
      of JAE:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 0:
          code.setPosition(w0.int)
      of JBE:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 1 or regs.CF == 1:
          code.setPosition(w0.int)
      of JB:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 1:
          code.setPosition(w0.int)
      of JA:
        code.read(w0)
        if w0 > codeLen: exception
        if regs.ZF == 0 and regs.CF == 0:
          code.setPosition(w0.int)
      of ADVR:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        w1 = regs.R[b0].WORD + w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of ADRR:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 + d1
        regs.R[b0] = d2
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = if d2 < d0: 1 else: 0
      of ADRRL:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b1 = b1 + b2
        regs.R[b0] = b1
        regs.ZF = if b1 == 0: 1 else: 0
        regs.CF = if b1 < b2: 1 else: 0
      of SUBVR:
        code.read(b0)
        if b0 > 8: exception
        code.read(w0)
        w1 = regs.R[b0].WORD - w0
        regs.ZF = if w1 == 0: 1 else: 0
        regs.CF = if w1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = w1
      of SUBRR:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 - d1
        regs.R[b0] = d2
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = if d2 > d0: 1 else: 0
      of SUBRRL:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b1 = b1 - b2
        regs.R[b0] = b1
        regs.ZF = if b1 == 0: 1 else: 0
        regs.CF = if d1 > b2: 1 else: 0
      of XOR:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        d2 = d0 xor d1
        regs.ZF = if d2 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = d2
      of XORL:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        b2 = regs.R[b0].BYTE
        b1 = regs.R[b1].BYTE
        b2 = b2 xor b1
        regs.ZF = if b2 == 0: 1 else: 0
        regs.CF = 0
        regs.R[b0] = b2
      of NOT:
        code.read(b0)
        if b0 > 8: exception
        regs.R[b0] = not regs.R[b0]
      of NOTL:
        code.read(b0)
        if b0 > 8: exception
        #cast[ptr BYTE](addr regs.R[b0])[] = not regs.R[b0]
      of ADVRD:
        code.read(b0)
        if b0 > 8: exception
        code.read(d0)
        d1 = regs.R[b0] + d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 < regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of SUBVRD:
        code.read(b0)
        if b0 > 8: exception
        code.read(d0)
        d1 = regs.R[b0] - d0
        regs.ZF = if d1 == 0: 1 else: 0
        regs.CF = if d1 > regs.R[b0]: 1 else: 0
        regs.R[b0] = d1
      of CMP:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        d0 = regs.R[b0]
        d1 = regs.R[b1]
        regs.ZF = if d1 == d0: 1 else: 0
        regs.CF = if d1 > d0: 1 else: 0
      of CMPL:
        code.read(b0)
        if b0 > 8: exception
        code.read(b1)
        if b1 > 8: exception
        b0  = LOW(regs.R[b0])
        b1  = LOW(regs.R[b1])
        regs.ZF = if b0 == b0: 1 else: 0
        regs.CF = if b1 > b0: 1 else: 0
      of PUSH:
        code.read(b0)
        if b0 > 8: exception
        dec(regs.SP)
        if regs.SP == 0xffffffff.DWORD:
          error "stack overflow"
        stack[regs.SP] = regs.R[b0]
      of POP:
        code.read(b0)
        if b0 > 8: exception
        if stack[regs.SP] == DWORD(sizeof(stack)/sizeof(DWORD)):
          error "stack underflow"
        regs.R[b0] = stack[regs.SP]
        inc(regs.SP)
      of POC:
        if stack[regs.SP] == stack[int(sizeof(stack)/sizeof(DWORD))]: exception
        code.readAt(regs.SP, b0)
        discard writeBuffer(stdout, addr b0, sizeof(b0))
      of POCN:
        if stack[regs.SP] == stack[int(sizeof(stack)/sizeof(DWORD))]:
          error "stack underflow"
        code.readAt(regs.SP, b0)
        echo $b0
      #else:
      #  discard
