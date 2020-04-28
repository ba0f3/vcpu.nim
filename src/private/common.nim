type
  BYTE* = uint8
  WORD* = uint16
  DWORD* = uint32

  Regs* {.size: 1.} = enum
    AH AL BH BL CH CL DH DL
    RA RB RC RD RE RF FG RH
    R0 R1 R2 R3 R4 R5 R6 R7

  OpCode* = enum
    NOP = 0x00
    MOV, CMP
    JMP JZ JNZ JGE JLE JL JG
    ADD SUB INC DEC SHL SHR MOD
    XOR, OR, AND, NOT
    PUSH = 0x90, POP
    PRNT = 0xb0, PRNTX, PRNTS,
    HALT = 0xd0, CALL, RET, DUMP


  Instruction* = object
    op* {.bitsize: 5.}: OpCode
    im* {.bitsize: 1.}: bool # register or immediate
    fp* {.bitsize: 1.}: bool # first arg is pointer
    lp* {.bitsize: 1.}: bool # second arg is pointer
