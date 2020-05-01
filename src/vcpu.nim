import streams, macros, strutils, locks

import private/[common, helpers]
export common

const
  VCPU_DATA_SIZE {.intdefine.} = 10240
  VCPU_STACK_SIZE {.intdefine.} = 32

type

  REGISTERS = object
    R: array[8, IMM]
    ZF {.bitsize:1.}: uint8
    CF {.bitsize:1.}: uint8
    PC, SP, BP: DWORD

  VCPU* = ref object of RootObj
    code: array[VCPU_DATA_SIZE, BYTE]
    codeLen: DWORD
    stack: array[VCPU_STACK_SIZE, DWORD]
    regs: REGISTERS
    lock: Lock

  BufferOverflowException* = object of OSError
  StackOverflowException* = object of OSError
  StackUnderflowException* = object of OSError

proc err(t: typedesc, msg: string) {.inline.} = raise newException(t, msg)

template invalid() = err(IOError, "invalid register")
template bof() = err(BufferOverflowException, "buffer overflow")

proc `[]`(R: ptr array[6, DWORD], idx: BYTE): DWORD = R[idx mod 6]
#template R(idx: int): DWORD = cpu.regs.R[idx]

macro debug(args: varargs[untyped]): untyped =
  result = newStmtList()
  when not defined(release):
    for n in args:
      if n.kind != nnkStrLit:
        result.add newCall("write", newIdentNode("stdout"), newLit(n.repr))
        result.add newCall("write", newIdentNode("stdout"), newLit(": "))
      result.add newCall("write", newIdentNode("stdout"), n)
      result.add newCall("write", newIdentNode("stdout"), newLit(" "))
    result.add newCall("writeLine", newIdentNode("stdout"), newLit(""))
    result.add newCall("flushFile", newIdentNode("stdout"))

macro trace(args: varargs[untyped]): untyped =
  result = newStmtList()
  when defined(trace):
    result.add newCall("write", newIdentNode("stdout"), newStrLitNode("[VCPU] "))
    for n in args:
      result.add newCall("write", newIdentNode("stdout"), n)
      result.add newCall("write", newIdentNode("stdout"), newStrLitNode(" "))
    result.add newCall("write", newIdentNode("stdout"), newStrLitNode("\n"))

proc read[T](cpu: VCPU, output: var T) =
  copyMem(addr output, addr cpu.code[cpu.regs.PC], sizeof(T))
  inc(cpu.regs.PC, sizeof(T))

proc read*[T](cpu: VCPU, output: var T, pos: WORD|DWORD) =
  ## Read data directly from VCPU memory
  copyMem(addr output, addr cpu.code[pos], sizeof(T))

proc write*[T](cpu: VCPU, input: T, pos: WORD|DWORD) =
  ## Write data directly into VCPU memory
  if pos.int + sizeof(T) >= VCPU_DATA_SIZE:
    err(BufferOverflowException, "not enough free space")
  var tmpLen: int
  when T is string:
    var input = input
    if input[input.len - 1] != '\0':
      input.add('\0')
    copyMem(addr cpu.code[pos], input.cstring, input.len)
    tmpLen = pos.int + input.len
  else:
    copyMem(addr cpu.code[pos], input.unsafeAddr, sizeof(T))
    tmpLen = pos.int + sizeof(T)
  if tmpLen > cpu.codeLen.int:
    cpu.codeLen = tmpLen.DWORD

proc setReg*(cpu: VCPU, reg: Regs, value: DWORD) {.inline.} = cpu.regs.R[reg.ord mod 8].d = value
proc setReg*(cpu: VCPU, r1: Regs, r2: Regs) {.inline.} = cpu.setReg(r1, cpu.regs.R[r2.ord mod 8].d)
proc getReg*(cpu: VCPU, reg: Regs): DWORD {.inline.} = cpu.regs.R[reg.ord mod 8].d

proc getZF*(cpu: VCPU): uint8 {.inline.} = cpu.regs.ZF
proc getCF*(cpu: VCPU): uint8 {.inline.} = cpu.regs.CF


proc reset*(cpu: VCPU) =
  zeroMem(addr cpu.regs, sizeof(REGISTERS))
  zeroMem(addr cpu.code, VCPU_DATA_SIZE)
  cpu.regs.SP = VCPU_STACK_SIZE
  cpu.regs.BP = VCPU_STACK_SIZE
  cpu.codeLen = 0

proc newVCPU*(): VCPU =
  result = new VCPU
  initLock(result.lock)
  result.reset()

proc loadCode*(cpu: VCPU, code: pointer, codeLen: int): bool =
  debug "loading code", codeLen
  copyMem(addr cpu.code, code, codeLen)
  cpu.codeLen = codeLen.DWORD
  return true

proc loadCode*(cpu: VCPU, code: openArray[byte]): bool {.inline.} =
  cpu.loadCode(unsafeAddr code, code.len)

proc loadCode*(cpu: VCPU, stream: Stream): bool {.inline.} =
  let code = stream.readAll()
  cpu.loadCode(code.cstring, code.len)

proc addCode*(cpu: VCPU, code: pointer, codeLen: DWORD): DWORD =
  debug "adding code", codeLen
  if cpu.codeLen + codeLen >= VCPU_DATA_SIZE.DWORD: bof
  result = cpu.codeLen
  copyMem(addr cpu.code[result], code, codeLen)
  cpu.codeLen += codeLen

proc addCode*(cpu: VCPU, code: openArray[byte]): DWORD {.inline.} =
  cpu.addCode(unsafeAddr code, code.len.DWORD)

proc addCode*(cpu: VCPU, stream: Stream): DWORD {.inline.} =
  let code = stream.readAll()
  cpu.addCode(code.cstring, code.len.DWORD)

proc addInput*[T: BYTE|WORD|DWORD|string](cpu: VCPU, input: T): DWORD {.inline.} =
  ## Add user input value, returns its address in memory
  when T is string:
    var input = input
    if input[input.len - 1] != '\0':
      input.add('\0')
    result = cpu.addCode(input.cstring, input.len.DWORD)
  else:
    cpu.addCode(input.unsafeAddr, sizeof(T).DWORD)

proc push*(cpu: VCPU, value: DWORD) =
  dec(cpu.regs.SP)
  if cpu.regs.SP == 0xffffffff.DWORD:
    err(StackOverflowException, "stack overflow")
  cpu.stack[cpu.regs.SP] = value

proc pop(cpu: VCPU): DWORD {.inline.} =
  if cpu.stack[cpu.regs.SP] == VCPU_STACK_SIZE:
    err(StackUnderflowException, "stack underflow")
  result = cpu.stack[cpu.regs.SP]
  cpu.stack[cpu.regs.SP] = 0
  inc(cpu.regs.SP)

proc dump*(cpu: VCPU): DWORD =
  # dump loaded code to asm
  discard

proc run*(cpu: VCPU): DWORD {.discardable.} =
  var
    ins: Instruction
    op: OpCode
    b0, b1, b2: BYTE
    w0, w1: WORD
    d0, d1: DWORD
    R = addr cpu.regs.R
  cpu.lock.acquire()
  while true:
    if cpu.regs.PC >= cpu.codeLen: bof
    cpu.read(ins)
    #echo ins
    op = ins.op
    case op
    of NOP:
      trace op
    of DUMP:
      when not defined(release):
        hexdump(cast[cstring](addr cpu.regs), sizeof(REGISTERS))
    of HALT:
      trace op, "\t; 🚫"
      break
    of CALL:
      cpu.read(w0)
      trace op, w0, "; PC =", cpu.regs.PC
      cpu.push(cpu.regs.PC)
      cpu.regs.PC = w0
    of RET:
      cpu.regs.PC = cpu.pop()
      trace op, "; PC =", cpu.regs.PC
    of MOV:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d0)
        if ins.fp and ins.lp:
          trace op, "[" & $b0.Regs & "]" , "[" & $d0 & "]"
          raise newException(ValueError, "invalid combination of opcode and operands")
        elif ins.fp:
          trace op, "[" & $b0.Regs & "]" , d0
          cpu.code[R{b0}.d] = cast[ptr BYTE](addr d0)[]
        elif ins.lp:
          trace op, b0.Regs, "[" & $d0 & "]"
          R{b0} = cpu.code[d0]
        else:
          trace op, b0.Regs, d0
          R{b0} = d0
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        if ins.fp and ins.lp:
          trace op, "[" & $b0.Regs & "]" , "[" & $b1.Regs & "]"
          raise newException(ValueError, "invalid combination of opcode and operands")
        elif ins.fp:
          trace op, "[" & $b0.Regs & "]" , b1.Regs
          cpu.code[R{b0}.d] = cast[ptr BYTE](addr R{b1}.d)[]
        elif ins.lp:
          trace op, b0.Regs, "[" & $b1.Regs & "]"
          R{b0} = cpu.code[R{b1}.d]
        else:
          trace op, b0.Regs, b1.Regs
          R{b0} = R{b1}.d
    of JMP:
      # unconditional jump
      cpu.read(d0)
      trace op, d0
      if d0 > cpu.codeLen: bof
      cpu.regs.PC = d0
    of JZ:
      # jump if equal
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of JNZ:
      # jump if not equal
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of JGE:
      # jump if greater or equal
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1 or cpu.regs.CF == 0:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of JLE:
      # jump if less than or equal
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1 or cpu.regs.CF == 1:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of JL:
      # jump if less than
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0 and cpu.regs.CF == 1:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of JG:
      # jump if greater
      cpu.read(d0)
      if d0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0 and cpu.regs.CF == 0:
        trace op, d0, "\t; ✔️"
        cpu.regs.PC = d0
      else:
        trace op, d0, "\t; ❌"
    of XOR:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d1)
        trace op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace op, b0.Regs, b1.Regs
        d1 = R{b1}.d
      d0 = R{b0}.d
      d0 = d0 xor d1
      cpu.regs.ZF = if d0 == 0: 1 else: 0
      cpu.regs.CF = 0
      R{b0} = d0
    of NOT:
      # Bitwise not on value in a register and save result in this register
      cpu.read(b0)
      if b0 > Regs.high: invalid
      trace op, b0.Regs
      R{b0} = not R{b0}.d
    of CMP:
      # compare two registers
      cpu.read(b0)
      if b0 > Regs.high: invalid
      d0 = R{b0}.d
      if ins.im:
        cpu.read(d1)
        if ins.fp and ins.lp:
          trace op, "[" & $b0.Regs & "]" , "[" & $d1 & "]"
          raise newException(ValueError, "invalid combination of opcode and operands")
        elif ins.fp:
          trace op, "[" & $b0.Regs & "]" , d1
          d0 = cpu.code[R{b0}.d]
        elif ins.lp:
          trace op, b0.Regs, "[" & $d1 & "]"
          d1 = cpu.code[d1]
        else:
          trace op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        if ins.fp and ins.lp:
          trace op, "[" & $b0.Regs & "]" , "[" & $b1.Regs & "]"
          raise newException(ValueError, "invalid combination of opcode and operands")
        elif ins.fp:
          trace op, "[" & $b0.Regs & "]" , b1.Regs
          d0 = cpu.code[R{b0}.d]
          d1 = R{b1}.d
        elif ins.lp:
          trace op, b0.Regs, "[" & $b1.Regs & "]"
          d1 = cpu.code[R{b1}.d]
        else:
          trace op, b0.Regs, b1.Regs
          d1 = R{b1}.d
      cpu.regs.ZF = if d1 == d0: 1 else: 0
      cpu.regs.CF = if d1 > d0: 1 else: 0
    of INC:
      cpu.read(b0)
      trace op, b0.Regs, "\t; ➕"
      if b0 > Regs.high: invalid
      inc(R{b0}.d)
    of DEC:
      cpu.read(b0)
      trace op, b0.Regs, "\t; ➖"
      if b0 > Regs.high: invalid
      dec(R{b0}.d)
    of SHL:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(b2)
        trace op, b0.Regs, b2
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace op, b0.Regs, b1.Regs
        b2 = R{b1}.d.BYTE
      R{b0} = R{b0}.d shl b2
    of SHR:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(b2)
        trace op, b0.Regs, b2
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        b2 = R{b1}.d.BYTE
        trace op, b0.Regs, b1.Regs
      R{b0} = R{b0}.d shr b2
    of MOD:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      cpu.read(b1)
      if ins.im:
        cpu.read(d0)
        trace op, b0.Regs, d0
      else:
        if b1 > Regs.high: invalid
        trace op, b0.Regs, b1.Regs
        d0 = R{b1}.d
      R{b0} = R{b0}.d mod d0
    of PUSH:
      if ins.im:
        cpu.read(d0)
        trace op, d0
      else:
        cpu.read(b0)
        if b0 > Regs.high: invalid
        trace op, b0.Regs
        d0 = R{b0}.d
      cpu.push(d0)
    of POP:
      cpu.read(b0)
      trace op, b0.Regs
      if b0 > Regs.high: invalid
      R{b0} = cpu.pop()
    of PRNT:
      trace op
      let idx = cpu.pop()
      echo cpu.code[idx].char
    of PRNTX:
      trace op
      let idx = cpu.pop()
      echo ($cast[cstring](addr cpu.code[idx])).toHex()
    of PRNTS:
      trace op
      let idx = cpu.pop()
      echo cast[cstring](addr cpu.code[idx])
    else:
      echo "Opcode is not implement yet: ", $op
  result = R{0}.d
  cpu.lock.release()

proc jmp*(cpu: VCPU, `addr`: DWORD): bool {.discardable.} =
  ## Jump to specified location in CPU memory
  cpu.lock.acquire()
  if `addr` < cpu.codeLen:
    cpu.regs.PC = `addr`
    result = true
  cpu.lock.release()

proc call*(cpu: VCPU, `addr`: DWORD): DWORD =
  if cpu.jmp(`addr`):
    result = cpu.run()