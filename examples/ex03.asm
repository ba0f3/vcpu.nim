
main:
  mov w0, tmp
  add [r0], 2
  assrt [r0], 2
  sub [r0], 2
  assrt [r0], 0
  mov r0, 0
  halt

tmp:
  db 0