
main:
  mov l0, 1
  mov w1, 2
  mov r2, sp
  ;add
  halt

add:
  push sp
  mov bp, sp
  pop sp
  ret

spell:
  db "a"