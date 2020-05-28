import tables, macros, locks

macro opcodes*(body: untyped): untyped =
  result = newStmtList()
  var
    fields: seq[NimNode]
    varSection = newNimNode(nnkVarSection)

  varSection.add(
    newIdentDefs(
      postfix(ident"OpOptions", "*"),
      newNimNode(nnkBracketExpr).add(
        ident"Table",
        ident"BYTE",
        ident"WORD"
      ),
      newNimNode(nnkCall).add(
        newNimNode(nnkBracketExpr).add(
          ident"initTable",
          ident"BYTE",
          ident"WORD"
        )
      )
    )
  )
  result.add(varSection)

  var
    option: uint16
    c = 0x0'u8
  for n in body:
    var op = n[0]
    if n.len == 4:
      c = n[3].intVal.uint8

    option = (n[1].intVal.uint16 shl 8) + n[2].intVal.uint16
    #echo op, "\t", c, "\t", option
    fields.add(newNimNode(nnkEnumFieldDef).add(
      op,
      newIntLitNode(c.int)
    ))
    result.add(
      newAssignment(
        newNimNode(nnkBracketExpr).add(
          ident"OpOptions",
          newIntLitNode(c.int)
        ),
        newIntLitNode(option.int)
      )
    )

    inc(c)
  result.add(newEnum(ident"OpCode", fields, true, false))


type
  BYTE* = uint8
  WORD* = uint16
  DWORD* = uint32

  IMM* {.union.} = object
    d*: DWORD
    w*: array[2, WORD]
    b*: array[4, BYTE]


  Regs* {.size: 1.} = enum
    #0  1  2  3  4  5  6  7
    L0 H0 L1 H1 L2 H2 L3 H3
    W0 W1 W2 W3 W4 W5 W6 W7
    R0 R1 R2 R3 R4 R5 R6 R7
    SP BP


opcodes:
  # op: nargs, type: 1: reg, 2: reg/imm
  NOP   0, 0, 0x00
  CALL  1, 2
  RET   0, 0
  MOV   2, 2
  JMP   1, 2
  JE    1, 2
  JNE   1, 2
  JGE   1, 2
  JLE   1, 2
  ADD   2, 2
  SUB   2, 2 # 0x0A
  MUL   2, 2
  DIV   1, 1
  INC   1, 1
  DEC   1, 1
  SHL   2, 2
  SHR   2, 2
  MOD   2, 2
  XOR   2, 2
  OR    2, 2
  AND   2, 2 # 0x14
  NOT   1, 1
  CMP   2, 2
  PUSH  1, 2
  POP   1, 1
  PRNTX 0, 0
  PRNTS 0, 0
  DUMP  0, 0 # 0x1B
  ASSRT 2, 2
  HALT  0, 0, 0x1F

  # special op, resv, data size
  DB    0, 1
  DW    0, 2
  DD    0, 4

const
  VCPU_DATA_SIZE* {.intdefine.} = 2048
  VCPU_STACK_SIZE* {.intdefine.} = 32

type

  REGISTERS* = object
    R*: array[8, IMM]
    ZF* {.bitsize:1.}: uint8
    CF* {.bitsize:1.}: uint8
    PC*, SP*, BP*: DWORD

  VCPU* = ref object of RootObj
    code*: array[VCPU_DATA_SIZE, BYTE]
    codeLen*: DWORD
    stack*: array[VCPU_STACK_SIZE, DWORD]
    regs*: REGISTERS
    lock*: Lock

  BufferOverflowException* = object of OSError
  StackOverflowException* = object of OSError
  StackUnderflowException* = object of OSError

  Instruction* = object
    op* {.bitsize: 5.}: OpCode
    im* {.bitsize: 1.}: bool # register or immediate
    fp* {.bitsize: 1.}: bool # first arg is pointer
    lp* {.bitsize: 1.}: bool # second arg is pointer

proc setReg*(cpu: VCPU, reg: Regs, value: DWORD) {.inline.} = cpu.regs.R[reg.ord mod 8].d = value
proc setReg*(cpu: VCPU, r1: Regs, r2: Regs) {.inline.} = cpu.setReg(r1, cpu.regs.R[r2.ord mod 8].d)
proc getReg*(cpu: VCPU, reg: Regs): DWORD {.inline.} = cpu.regs.R[reg.ord mod 8].d

proc getZF*(cpu: VCPU): uint8 {.inline.} = cpu.regs.ZF
proc getCF*(cpu: VCPU): uint8 {.inline.} = cpu.regs.CF

proc err*(t: typedesc, msg: string) {.inline.} = raise newException(t, msg)

proc push*(cpu: VCPU, value: DWORD) =
  dec(cpu.regs.SP)
  if cpu.regs.SP == 0xffffffff.DWORD:
    err(StackOverflowException, "stack overflow")
  cpu.stack[cpu.regs.SP] = value

proc pop*(cpu: VCPU): DWORD {.inline.} =
  if cpu.stack[cpu.regs.SP] == VCPU_STACK_SIZE:
    err(StackUnderflowException, "stack underflow")
  result = cpu.stack[cpu.regs.SP]
  cpu.stack[cpu.regs.SP] = 0
  inc(cpu.regs.SP)