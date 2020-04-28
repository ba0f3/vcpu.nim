main: ; entry point
  mov r1, r0 ;    hell yeah
  mov r2, 0x100
  mov r3, 100h
  mov r4, msg
  push r3
  prnts
  halt
msg:
  db "Hello World!", 0