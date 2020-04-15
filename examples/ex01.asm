%include "vm.inc"

main:
  mov r1, r0
  movd r3, msg
  push r3
  prnts
  halt

msg:
  db "Hello World!", 0