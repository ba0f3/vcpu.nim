import common, strutils, macros

template LOWORD*(d: DWORD): WORD = d.WORD
template HIWORD*(d: DWORD): WORD = (d shr 8).WORD
template LOBYTE*(w: WORD): BYTE = w.BYTE
template HIBYTE*(w: WORD): WORD = (w shr 8).BYTE

converter opcode2byte*(o: OpCode): BYTE = o.BYTE
converter regs2byte*(r: Regs): BYTE = r.BYTE
converter int2pointer*(x: int): pointer = cast[pointer](x)

template `+`*[T](p: ptr T, off: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[int](p) +% off.int)

template `+=`*[T](p: ptr T, off: SomeInteger) =
  p = p + off

template `-`*[T](p: ptr T, off: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[int](p) -% off.int)

template `-=`*[T](p: ptr T, off: SomeInteger) =
  p = p - off

template `+`*(p: pointer, off: SomeInteger): pointer =
  cast[pointer](cast[int](p) +% off.int)

template `+=`*(p: pointer, off: SomeInteger) =
  p = p + off

template `-`*(p: pointer, off: SomeInteger): pointer =
  cast[pointer](cast[int](p) -% off.int)

template `-=`*(p: pointer, off: SomeInteger) =
  p = p - off


macro debug*(args: varargs[untyped]): untyped =
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

macro trace*(args: varargs[untyped]): untyped =
  result = newStmtList()
  when defined(trace):
    for n in args:
      result.add newCall("write", newIdentNode("stdout"), n)
      result.add newCall("write", newIdentNode("stdout"), newStrLitNode(" "))
    result.add newCall("write", newIdentNode("stdout"), newStrLitNode("\n"))


proc index(idx: uint8): BYTE =
  if idx >= 8:
    return idx.uint8 mod 8
  return uint8(idx.int8 / 2)


proc size*(r: Regs): int =
  var tmp = (r.int8 / 8).int8
  if tmp > 0:
    return tmp * 2
  result = 1


proc `{}`*(r: ptr array[8, IMM], b: BYTE): ptr IMM =
  result = unsafeAddr r[index(b)]

proc `{}=`*(r: ptr array[8, IMM], b: BYTE, d: DWORD) =
  case size(b.Regs)
  of 4:
    r{b}.d = d
  of 2:
    r{b}.w[0] = d.WORD
  else:
    (unsafeAddr r[index(b)].b[b mod 2])[] = d.BYTE
    #tmp[] = d.BYTE


proc hexdump*(data: cstring, length: int) =
  var ascii: array[17, char]
  ascii[16] = '\0'
  for i in 0..15:
    stdout.write toHex(i.BYTE).toLowerAscii()
    stdout.write " "
  stdout.write "\n"
  for i in 0..<length:
    stdout.write toHex(data[i].BYTE)
    stdout.write " "
    if data[i] >= ' ' and data[i] <= '~':
      ascii[i mod 16] = data[i]
    else:
      ascii[i mod 16] = '.'
    if (i+1) mod 16 == 0:
      echo " | ", cast[cstring](addr ascii)
    elif (i+1) == length:
      ascii[(i+1) mod 16] = '\0'
      if (i+1) mod 16 <= 8:
        stdout.write " "

      var j = (i+1) mod 16
      while j < 16:
        stdout.write "   "
        inc(j)
      echo "| ", cast[cstring](addr ascii)
