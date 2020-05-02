import tables, macros

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
  #PRNT  0, 0
  PRNTX 0, 0
  PRNTS 0, 0
  DUMP  0, 0 # 0x1B
  ASSRT 2, 2
  HALT  0, 0 # 0x1D

  # special op, resv, data size
  DB    0, 1
  DW    0, 2
  DD    0, 4

type
  Instruction* = object
    op* {.bitsize: 5.}: OpCode
    im* {.bitsize: 1.}: bool # register or immediate
    fp* {.bitsize: 1.}: bool # first arg is pointer
    lp* {.bitsize: 1.}: bool # second arg is pointer

