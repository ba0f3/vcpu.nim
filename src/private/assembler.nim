import strutils, pegs, tables, streams, parseUtils
import common

let
  grammar = peg"""
  ASM   <- ^ ( ( \n / label  / code) ig )* $
  label <- \ident ':'
  code  <- ins (\n / \s+ arg (',' arg )* )?
  ins   <- \a+
  arg   <- ' '* (reg / num / name / '"' str '"') ' '*
  reg   <- ('r' [0-7a-h]) / [a-d][hl]
  name  <- \ident
  str   <- ("\\" . / [^"])*
  num   <- '0x' [0-9a-f]+ / [0-9a-f]+ 'h' / \d+
  #num   <- \d+
  ig    <- (\s / ';' @ \n)*
"""

type
  Assembler = ref object of RootObj
    labels: Table[string, int]
    code: StringStream

  TokenKind = enum
    LABEL
    INSTRUCTION
    REGISTER
    NAME
    NUMBER
    STRING

  Token = object of RootObj
    case kind: TokenKind:
    of LABEL:
      label: string
    of INSTRUCTION:
      ins: OpCode
    of REGISTER:
      reg: Regs
    of NAME:
      name: string
    of NUMBER:
      num: int
    of STRING:
      str: string

proc newAssembler(): Assembler =
  result = new(Assembler)
  result.labels = initTable[string, int]()
  result.code = newStringStream()

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
            tokens.add(Token(kind: LABEL, label: matchStr[0..^2]))
          of "ins":
            var ins = lookupIns(matchStr)
            tokens.add(Token(kind: INSTRUCTION, ins: ins))
          of "reg":
            var reg: Regs
            if isReg(matchStr, reg):
              tokens.add(Token(kind: REGISTER, reg: reg))
            else:
              raise newException(ValueError, "invalid register")

          of "name":
            tokens.add(Token(kind: NAME, name: matchStr))
          of "str":
            tokens.add(Token(kind: STRING, str: matchStr))
          of "num":
            var num: int
            discard parseHex(matchStr, num)
            tokens.add(Token(kind: NUMBER, num: num))

  discard parseArithExpr(s)
  result = tokens

proc compileFile(a: Assembler, path: string) =
  var
    op: string
    r1, r2: Token
    a1, a2: string
    d1, d2: DWORD
    tokens = tokenizer(readFile(path))
  for i in 0..<tokens.len:
    var token = tokens[i]
    case token.kind
    of LABEL:
      a.labels[token.label] = a.code.getPosition()
    of INSTRUCTION:
      var ins: Instruction
      ins.op = token.ins
      case ins.op:
      of MOV:
        r1 = tokens[i+1]
        r2 = tokens[i+2]
        case r2.kind
        of NUMBER:
          ins.im = true
          a.code.write(ins)
          a.code.write(r1.reg)
          a.code.write(r2.num.DWORD)
        of REGISTER:
          a.code.write(ins)
          a.code.write(r1.reg)
          a.code.write(r2.reg)
        else:
          discard
      else:
        discard
    else:
      discard

#[
proc compileFile(a: Assembler, path: string) =
  var
    op: string
    r1, r2: Regs
    a1, a2: string
    d1, d2: DWORD
  for line in path.lines:
    if line =~ grammar:
      echo matches
      op = matches[0]
      a1 = matches[1]
      a2 = matches[2]
      if a1 == ":":
        a.labels[op] = a.code.getPosition()
      elif op == "db":
        echo "db"
      else:
        var ins: Instruction
        ins.op = lookupIns(op)
        case ins.op
        of MOV:
          if not isReg(matches[1], r1):
            echo "first arg for MOV must be an register"
          if isReg(matches[2], r2):
            a.code.write(ins)
            a.code.write(r1)
            a.code.write(r2)
          else:
            ins.im = true
            a.code.write(ins)
            echo ins
            a.code.write(r1)
            # TODO check size, is pointer?
            try:
              discard parseHex(a2, d1)
              a.code.write(d1)
            except:
              # TODO insert label's position
              discard
        else:
          discard
]#
var a = newAssembler()
a.compileFile("examples/ex01.asm")
a.code.setPosition(0)
echo a.code.readAll().toHex()
echo $a[]