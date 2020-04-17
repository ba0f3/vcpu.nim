%include "vm.inc"

main:
  push r0
  push r1
loop:
  movmrb r4, r0
  movmrb r3, r1
  xor r4, r3
  movrmb r0, r4
  inc r0
  inc r1
  movmrb r2, r0
  cmpb r2, 0x00
  jz end
  movmrb r3, r1
  cmpb r3, 0
  jz reset_key
  jmp loop

reset_key:
  pop r1
  push r1
  jmp loop

end:
  ;pop r1
  ;prntx
  halt