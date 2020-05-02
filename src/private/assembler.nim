import os, strutils, pegs, tables, strtabs, streams, parseUtils
import common, helpers

# TODO: fix data size for operations

let grammar = peg(readFile(joinPath(currentSourcePath.splitPath.head,  "grammar.peg")))

type
  Placeholder = object
    size: int
    pos: int

  Assembler = ref object of RootObj
    basePath: string
    includes: seq[string]
    code: StringStream
    labels: Table[string, int]
    placeholders: Table[string, seq[Placeholder]]
    inlineString: StringTableRef
    tokens: seq[Token]
    pos: int

  TokenKind = enum
    NONE
    LABEL
    INS
    REG
    NAME
    IMM
    STR

  Token = object of RootObj
    isptr: bool
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
    else:
      discard

proc newAssembler*(): Assembler =
  result = new(Assembler)
  result.basePath = getAppDir()
  result.code = newStringStream()
  result.labels = initTable[string, int]()
  result.placeholders = initTable[string, seq[Placeholder]]()
  result.inlineString = newStringTable(modeCaseInsensitive)


proc lookupIns(ins: string): OpCode =
  var ins = ins.toUpperAscii()
  for i in OpCode.low .. OpCode.high:
    if ins == $i:
      return i
  raise newException(ValueError, "unknow instruction: " & ins )

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

proc tokenizer(a: Assembler, t: Token)

proc tokenizer(a: Assembler, s: string) =
  var stack: seq[Token]

  let parseArithExpr = grammar.eventParser:
    pkNonTerminal:
      leave:
        if p.nt.name != "ig" and length > 0:
          let matchStr = s.substr(start, start+length-1)
          #echo p.nt.name, " => ", matchStr
          case p.nt.name
          of "inc":
            var t = stack.pop()
            assert t.kind == STR
            a.tokenizer(t)
          of "label":
            a.tokens.add(labelToken(matchStr[0..^2]))
          of "ins":
            var ins = lookupIns(matchStr)
            a.tokens.add(insToken(ins))
          of "reg":
            var reg: Regs
            if isReg(matchStr, reg):
              stack.add(regToken(reg))
            else:
              raise newException(ValueError, "invalid register")
          of "name":
            stack.add(nameToken(matchStr))
          of "addr":
            var token = stack.pop()
            token.isptr = true
            stack.add(token)
          of "str":
            stack.add(strToken(matchStr))
          of "imm":
            var num: int
            discard parseHex(matchStr, num)
            stack.add(immToken(num))
          of "arg":
            a.tokens.add(stack.pop())
  discard parseArithExpr(s)

  while a.includes.len > 0:
    let path = a.includes.pop()
    a.tokenizer(readFile(path))

proc tokenizer(a: Assembler, t: Token) =
  assert t.kind == STR
  let path = joinPath(a.basePath, t.s)
  if not path.fileExists:
    raise newException(IOError, "include file not found: " & path)
  if path notin a.includes:
    a.includes.add(path)


proc writeArg(a: Assembler, t: Token, prev = Token(kind: NONE)) =
  case t.kind:
  of REG:
    a.code.write(t.r)
  of IMM:
    if prev.kind == REG:
      case size(prev.r)
      of 1:
        a.code.write(t.i.BYTE)
      of 2:
        a.code.write(t.i.WORD)
      else:
        a.code.write(t.i.DWORD)
    else:
      a.code.write(t.i.DWORD)
  of NAME:
    var size = 2
    if prev.kind == REG:
      size = size(prev.r)
    var ph = Placeholder(size: size, pos: a.code.getPosition)
    if not a.placeholders.hasKey(t.n):
      a.placeholders[t.n] = @[ph]
    else:
      a.placeholders[t.n].add(ph)
    case size
    of 1:
      a.code.write(0.BYTE)
    of 2:
      a.code.write(0.WORD)
    else:
      a.code.write(0.DWORD)
  of STR:
    var size = 2
    if prev.kind == REG:
      size = size(prev.r)
    let name = "_str_" & $a.inlineString.len
    a.inlineString[name] = t.s
    a.placeholders[name] = @[Placeholder(size: size, pos: a.code.getPosition)]
    case size
    of 1:
      a.code.write(0.BYTE)
    of 2:
      a.code.write(0.WORD)
    else:
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
    if a1.isptr:
      ins.fp = true
    a.code.write(ins)
    a.writeArg(a1)
  else:
    assert kind != 0
    a1 = a.tokens[a.pos + 1]
    a2 = a.tokens[a.pos + 2]
    if kind == 2 and a2.kind != REG:
      ins.im = true
    if a1.isptr:
      ins.fp = true
    if a2.isptr:
      ins.lp = true
    if ins.fp and ins.lp:
      raise newException(ValueError, "invalid combination of opcode and operands")
    a.code.write(ins)
    a.writeArg(a1)
    a.writeArg(a2, a1)
  inc(a.pos, nargs)

proc compileString*(a: Assembler, input: string): TaintedString =
  a.tokenizer(input)

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

  for label, str in a.inlineString.pairs:
    let pos = a.code.getPosition()
    a.code.write(str)
    for p in a.placeholders[label]:
      a.code.setPosition(p.pos)
      case p.size
      of 1:
        a.code.write(pos.BYTE)
      of 2:
        a.code.write(pos.WORD)
      else:
        a.code.write(pos.DWORD)


  for label, pos in a.labels.pairs:
    if not a.placeholders.hasKey(label):
      continue
    for p in a.placeholders[label]:
      a.code.setPosition(p.pos)
      case p.size
      of 1:
        a.code.write(pos.BYTE)
      of 2:
        a.code.write(pos.WORD)
      else:
        a.code.write(pos.DWORD)
  a.code.setPosition(0)
  result = a.code.readAll()

proc compileFile*(a: Assembler, input: string, output: string) =
  a.basePath = expandFilename(input).splitPath.head
  var res = a.compileString(readFile(input))
  writeFile(joinPath(a.basePath, output), res)

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