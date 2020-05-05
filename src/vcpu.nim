import streams, macros, strutils, locks, tables

import private/[common, helpers, functions]
export common, functions

template invalid() = err(IOError, "invalid register")
template bof() = err(BufferOverflowException, "buffer overflow")

#proc `[]`(R: ptr array[6, DWORD], idx: BYTE): DWORD = R[idx mod 6]
#template R(idx: int): DWORD = cpu.regs.R[idx]

proc read[T](cpu: VCPU, output: var T) =
  copyMem(addr output, addr cpu.code[cpu.regs.PC], sizeof(T))
  inc(cpu.regs.PC, sizeof(T))

proc read[T](cpu: VCPU, output: var T, reg: Regs) =
  let size = size(reg)
  if size > sizeof(T): bof
  copyMem(addr output, addr cpu.code[cpu.regs.PC], size)
  inc(cpu.regs.PC, size)


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



proc dump*(cpu: VCPU): DWORD =
  # dump loaded code to asm
  discard

proc run*(cpu: VCPU): DWORD {.discardable.} =
  ## Execute loaded instructions, return R0's value when finish
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
    when not defined(release):
      var pc = toHex(cpu.regs.PC.WORD) & ":"
    cpu.read(ins)
    #echo ins
    op = ins.op
    case op
    of NOP:
      trace pc, op
    of CALL:
      if ins.fp: # address is a pointer to external function
        var add: int
        cpu.read(add)
        trace pc, op, "[" & functions_name[add] & "]", "\t; ☎️"
        getFunc(add)(cpu)
      else:
        cpu.read(w0)
        trace pc, op, toHex(w0), "\t; ☎️"
        cpu.push(cpu.regs.PC)
        cpu.regs.PC = w0
    of RET:
      cpu.regs.PC = cpu.pop()
      trace pc, op, "\t: 🔙"
    of MOV:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d0, b0.Regs)
        if ins.fp:
          trace pc, op, "[" & $b0.Regs & "]" , d0
          cpu.code[R{b0}.d] = cast[ptr BYTE](addr d0)[]
        elif ins.lp:
          trace pc, op, b0.Regs, "[" & $d0 & "]"
          R{b0} = cpu.code[d0]
        else:
          trace pc, op, b0.Regs, d0
          R{b0} = d0
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        if ins.fp:
          trace pc, op, "[" & $b0.Regs & "]" , b1.Regs
          cpu.code[R{b0}.d] = cast[ptr BYTE](addr R{b1}.d)[]
        elif ins.lp:
          trace pc, op, b0.Regs, "[" & $b1.Regs & "]"
          R{b0} = cpu.code[R{b1}.d]
        else:
          trace pc, op, b0.Regs, b1.Regs
          R{b0} = R{b1}.d
    of JMP:
      # unconditional jump
      cpu.read(w0)
      trace pc, op, toHex(w0), "\t; ↩️"
      if w0 > cpu.codeLen: bof
      cpu.regs.PC = w0
    of JE:
      # jump if equal
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, toHex(w0), "\t; ❌"
    of JNE:
      # jump if not equal
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, toHex(w0), "\t; ❌"
    of JGE:
      # jump if greater or equal
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1 or cpu.regs.CF == 0:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, toHex(w0), "\t; ❌"
    of JLE:
      # jump if less than or equal
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 1 or cpu.regs.CF == 1:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, d0, "\t; ❌"
    #[
    of JL:
      # jump if less than
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0 and cpu.regs.CF == 1:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, toHex(w0), "\t; ❌"
    of JG:
      # jump if greater
      cpu.read(w0)
      if w0 > cpu.codeLen: bof
      if cpu.regs.ZF == 0 and cpu.regs.CF == 0:
        trace pc, op, toHex(w0), "\t; ✔️"
        cpu.regs.PC = w0
      else:
        trace pc, op, toHex(w0), "\t; ❌"
    ]#
    of ADD:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d0, b0.Regs)
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        d0 = R{b1}.d
      if ins.fp:
        trace pc, op, "[" & $b0.Regs & "]" , d0
        if R{b0}.d > cpu.codeLen: bof
        cpu.read(d1, R{b0}.d)
        d1 += d0
        cpu.write(d1, R{b0}.d)
      else:
        trace pc, op, b0.Regs, d0
        R{b0} = R{b0}.d + d0
    of SUB:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d0)
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        d0 = R{b1}.d
      if ins.fp:
        trace pc, op, "[" & $b0.Regs & "]" , d0
        if R{b0}.d > cpu.codeLen: bof
        cpu.read(d1, R{b0}.d)
        d1 -= d0
        cpu.write(d1, R{b0}.d)
      else:
        trace pc, op, b0.Regs, d0
        R{b0} = R{b0}.d - d0
    of MUL:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d0, b0.Regs)
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        d0 = R{b1}.d
      if ins.fp:
        trace pc, op, "[" & $b0.Regs & "]" , d0
        if R{b0}.d > cpu.codeLen: bof
        cpu.read(d1, R{b0}.d)
        d1 *= d0
        cpu.write(d1, R{b0}.d)
      else:
        trace pc, op, b0.Regs, d0
        R{b0} = R{b0}.d * d0
    of DIV:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      d0 = R{R0}.d
      d1 = R{b0}.d
      R{R0} = (d0.int / d1.int).DWORD
      R{R3} = d0 mod d1
    of INC:
      cpu.read(b0)
      trace pc, op, b0.Regs
      if b0 > Regs.high: invalid
      d0 = R{b0}.d
      inc(d0)
      R{b0} = d0
    of DEC:
      cpu.read(b0)
      trace pc, op, b0.Regs
      if b0 > Regs.high: invalid
      d0 = R{b0}.d
      dec(d0)
      R{b0} = d0
    of SHL:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(b2)
        trace pc, op, b0.Regs, b2
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace pc, op, b0.Regs, b1.Regs
        b2 = R{b1}.d.BYTE
      R{b0} = R{b0}.d shl b2
    of SHR:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(b2)
        trace pc, op, b0.Regs, b2
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        b2 = R{b1}.d.BYTE
        trace pc, op, b0.Regs, b1.Regs
      R{b0} = R{b0}.d shr b2
    of MOD:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      cpu.read(b1)
      if ins.im:
        cpu.read(d0, b0.Regs)
        trace pc, op, b0.Regs, d0
      else:
        if b1 > Regs.high: invalid
        trace pc, op, b0.Regs, b1.Regs
        d0 = R{b1}.d
      R{b0} = R{b0}.d mod d0
    of XOR:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d1, b0.Regs)
        trace pc, op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace pc, op, b0.Regs, b1.Regs
        d1 = R{b1}.d
      d0 = R{b0}.d
      d0 = d0 xor d1
      cpu.regs.ZF = if d0 == 0: 1 else: 0
      cpu.regs.CF = 0
      R{b0} = d0
    of OR:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d1, b0.Regs)
        trace pc, op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace pc, op, b0.Regs, b1.Regs
        d1 = R{b1}.d
      d0 = R{b0}.d
      d0 = d0 or d1
      cpu.regs.ZF = if d0 == 0: 1 else: 0
      cpu.regs.CF = 0
      R{b0} = d0
    of AND:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.im:
        cpu.read(d1)
        trace pc, op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        trace pc, op, b0.Regs, b1.Regs
        d1 = R{b1}.d
      d0 = R{b0}.d
      d0 = d0 and d1
      cpu.regs.ZF = if d0 == 0: 1 else: 0
      cpu.regs.CF = 0
      R{b0} = d0
    of NOT:
      # Bitwise not on value in a register and save result in this register
      cpu.read(b0)
      if b0 > Regs.high: invalid
      trace pc, op, b0.Regs
      R{b0} = not R{b0}.d
    of CMP:
      # compare two registers
      cpu.read(b0)
      if b0 > Regs.high: invalid
      d0 = R{b0}.d
      if ins.im:
        cpu.read(d1, b0.Regs)
        if ins.fp:
          trace pc, op, "[" & $b0.Regs & "]" , d1
          d0 = cpu.code[R{b0}.d]
        elif ins.lp:
          trace pc, op, b0.Regs, "[" & $d1 & "]"
          d1 = cpu.code[d1]
        else:
          trace pc, op, b0.Regs, d1
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        if ins.fp:
          trace pc, op, "[" & $b0.Regs & "]" , b1.Regs
          d0 = cpu.code[R{b0}.d]
          d1 = R{b1}.d
        elif ins.lp:
          trace pc, op, b0.Regs, "[" & $b1.Regs & "]"
          d1 = cpu.code[R{b1}.d]
        else:
          trace pc, op, b0.Regs, b1.Regs
          d1 = R{b1}.d
      cpu.regs.ZF = if d1 == d0: 1 else: 0
      cpu.regs.CF = if d1 > d0: 1 else: 0
    of PUSH:
      if ins.im:
        cpu.read(d0)
        trace pc, op, d0
      else:
        cpu.read(b0)
        if b0 > Regs.high: invalid
        if ins.fp:
          trace pc, op, "[" & $b0.Regs  & "]"
          d0 = cpu.code[R{b0}.d]
        else:
          trace pc, op, b0.Regs
          if b0.Regs == SP:
            d0 = cpu.regs.SP
          elif b0.Regs == BP:
            d0 = cpu.regs.BP
          else:
            d0 = R{b0}.d
      cpu.push(d0)
    of POP:
      cpu.read(b0)
      trace pc, op, b0.Regs
      if b0 > Regs.high: invalid
      if b0.Regs == SP:
        cpu.regs.SP = cpu.pop()
      elif b0.Regs == BP:
        cpu.regs.BP = cpu.pop()
      else:
        R{b0} = cpu.pop()
    #of PRNT:
    #  trace pc, op
    #  let idx = cpu.pop()
    #  echo cpu.code[idx].char
    of PRNTX:
      trace pc, op
      let idx = cpu.pop()
      echo ($cast[cstring](addr cpu.code[idx])).toHex()
    of PRNTS:
      trace pc, op
      let idx = cpu.pop()
      echo cast[cstring](addr cpu.code[idx])
    of DUMP:
      when not defined(release):
        hexdump(cast[cstring](addr cpu.regs), sizeof(REGISTERS))
        hexdump(cast[cstring](addr cpu.code), cpu.codeLen.int)
    of ASSRT:
      cpu.read(b0)
      if b0 > Regs.high: invalid
      if ins.fp:
        d0 = cpu.code[R{b0}.d]
      else:
        d0 = R{b0}.d
      if ins.im:
        cpu.read(d1, b0.Regs)
        if ins.lp:
          d1 = cpu.code[d1]
      else:
        cpu.read(b1)
        if b1 > Regs.high: invalid
        if ins.lp:
          d1 = cpu.code[d1]
        else:
          d1 = R{b1}.d
      assert d0 == d1
    of HALT:
      trace pc, op, "\t; ⛔"
      break
    of DB, DW, DD:
      # special instructions, should never go here
      discard

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