import strutils, pegs, tables, streams, parseUtils
import common, helpers

# TODO: detect arg size by reg name, pointer address

let
  grammar = peg"""
  ASM   <- ^ ( ( \n / label  / code) ig )* $
  label <- \ident ':'
  code  <- ins (\n / \s+ arg (',' arg )* )?
  ins   <- \a+
  arg   <- ' '* (reg / imm / name / '"' str '"') ' '*
  reg   <- ('r' [0-7a-h]) / [a-d][hl]
  name  <- \ident
  str   <- ("\\" . / [^"])*
  imm   <- '0x' [0-9a-f]+ / [0-9a-f]+ 'h' / \d+
  ig    <- (\s / ';' @ \n)*
"""

type
  Assembler = ref object of RootObj
    code: StringStream
    labels: Table[string, int]
    placeHolders: Table[string, seq[int]]
    tokens: seq[Token]
    pos: int

  TokenKind = enum
    LABEL
    INS
    REG
    NAME
    IMM
    STR

  Token = object of RootObj
    case kind: TokenKind:
    of LABEL:
      l: string
    of INS:
      o: OpCode
    of REG:
      r: Regs
    of NAME:
      n: string
    of IMM:
      i: int
    of STR:
      s: string

proc newAssembler*(): Assembler =
  result = new(Assembler)
  result.code = newStringStream()
  result.labels = initTable[string, int]()
  result.placeHolders = initTable[string, seq[int]]()


proc lookupIns(ins: string): OpCode =
  var ins = ins.toUpperAscii()
  for i in OpCode.low .. OpCode.high:
    if ins == $i:
      result = i
      break

proc isReg(s: string, r: var Regs): bool =
  var s = s.toUpperAscii()
  for i in Regs.low .. Regs.high:
    if s == $i:
      r = i
      result = true
      break

proc labelToken(val: string): Token {.inline.} = Token(kind: LABEL, l: val)
proc insToken(val: OpCode): Token {.inline.} = Token(kind: INS, o: val)
proc regToken(val: Regs): Token {.inline.} = Token(kind: REG, r: val)
proc nameToken(val: string): Token {.inline.} = Token(kind: NAME, n: val)
proc immToken(val: int): Token {.inline.} = Token(kind: IMM, i: val)
proc strToken(val: string): Token {.inline.} = Token(kind: STR, s: val)

proc tokenizer(s: string): seq[Token] =
  var tokens: seq[Token]
  let parseArithExpr = grammar.eventParser:
    pkNonTerminal:
      leave:
        if p.nt.name != "ig" and length > 0:
          let matchStr = s.substr(start, start+length-1)
          #echo p.nt.name, " => ", matchStr
          case p.nt.name
          of "label":
            tokens.add(labelToken(matchStr[0..^2]))
          of "ins":
            var ins = lookupIns(matchStr)
            tokens.add(insToken(ins))
          of "reg":
            var reg: Regs
            if isReg(matchStr, reg):
              tokens.add(regToken(reg))
            else:
              raise newException(ValueError, "invalid register")
          of "name":
            tokens.add(nameToken(matchStr))
          of "str":
            tokens.add(strToken(matchStr))
          of "imm":
            var num: int
            discard parseHex(matchStr, num)
            tokens.add(immToken(num))

  discard parseArithExpr(s)
  result = tokens

proc writeArg(a: Assembler, t: Token) =
  case t.kind:
  of REG:
    a.code.write(t.r)
  of IMM:
    a.code.write(t.i.DWORD)
  of NAME:
    if not a.placeHolders.hasKey(t.n):
      a.placeHolders[t.n] = @[a.code.getPosition()]
    else:
      a.placeHolders[t.n].add(a.code.getPosition())
    a.code.write(0.DWORD)
  else:
    echo t.kind, " not supported yet"

proc parseData(a: Assembler, op: OpCode) =
  var
    size = LOBYTE(OpOptions[op]).int8
    next  = a.tokens[a.pos + 1]

  while next.kind in [IMM, STR]:
    if next.kind == STR:
      a.code.write(next.s)
      if next.s.len mod size > 0:
        let pad = size - (next.s.len mod size)
        a.code.write('\0'.repeat(pad))
    elif next.kind == IMM:
      a.code.writeData(next.i.unsafeAddr, size)
    inc(a.pos)
    if a.pos + 1 >= a.tokens.len:
      break
    next  = a.tokens[a.pos + 1]

proc parseIns(a: Assembler, op: OpCode) =
  let
    options = OpOptions[op]
    nargs = HIBYTE(options).int8
    kind = LOBYTE(options).int8

  if a.tokens.len < a.pos + nargs:
    raise newException(ValueError, "not enough token")

  var
    ins = Instruction(op: op)
    a1, a2: Token
  if nargs == 0:
    a.code.write(ins)
  elif nargs == 1:
    a1 = a.tokens[a.pos + 1]
    if kind == 2  and a1.kind != REG:
      ins.im = true
    #echo op, " ", ins
    a.code.write(ins)
    a.writeArg(a1)
  else:
    assert kind != 0
    a1 = a.tokens[a.pos + 1]
    a2 = a.tokens[a.pos + 2]
    if kind == 2 and a2.kind != REG:
      ins.im = true
    a.code.write(ins)
    a.writeArg(a1)
    a.writeArg(a2)
  inc(a.pos, nargs)

proc compileString*(a: Assembler, input: string): TaintedString =
  a.tokens = tokenizer(input)

  while a.pos < a.tokens.len:
    var token = a.tokens[a.pos]
    assert token.kind in [LABEL, INS]
    case token.kind
    of LABEL:
      a.labels[token.l] = a.code.getPosition()
      inc(a.pos)
    of INS:
      if token.o in [DB, DW, DD]:
        a.parseData(token.o)
      else:
        a.parseIns(token.o)
      inc(a.pos)
    else:
      raise newException(ValueError, "it should never go here")

  for label, pos in a.labels.pairs:
    if not a.placeHolders.hasKey(label):
      continue
    for p in a.placeHolders[label]:
      a.code.setPosition(p)
      a.code.write(pos.DWORD)
  a.code.setPosition(0)
  result = a.code.readAll()

proc compileFile*(a: Assembler, input: string, output: string) =
  var res = a.compileString(readFile(input))
  writeFile(output, res)

when isMainModule:
  proc check(input: string, output: string) =
    var a = newAssembler()
    var res = a.compileString(input).toHex()
    #echo input, " => ", res
    assert res == output

  check("db 0x55", "55")
  check("db 0x55,0x56,0x57", "555657")
  check("db \"a\",0x55", "6155")
  check("db \"hello\",13,10", "68656C6C6F1310")
  check("dw 0x1234", "3412")
  check("dw \"a\"", "6100")
  check("dw \"ab\"", "6162")
  check("dw \"abc\"", "61626300")
  check("dd 0x12345678", "78563412")

  var a = newAssembler()
  a.compileFile("examples/ex01.asm", "examples/ex01")