main: ; entry point
  mov r1, r0 ;    hell yeah
  mov r2, ah
  mov r3, 100h
  mov r4, msg
  push r4
  prnts
  halt
msg:
  db "Hello World!", 0