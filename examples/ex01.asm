
main: ; entry point
  mov r0, 1 ;    hell yeah
  mov r1, 2
  mov r2, 3
  mov r3, 4
  mov r4, 5
  mov r5, 6
  mov r6, 7
  mov r7, msg
  dump
  push r7
  prnts
  mov r0, 0
  halt

msg:
  db "Hello World!", 0