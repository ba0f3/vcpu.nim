%include "vm.inc"
ma
in:
  movd r0, input
  movd r1, key
  jmp loop

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

end:
  movd r0, input
  push r0
  prnts
  halt

reset_key:
  movd r1, key
  jmp loop

key:
  db "s3cret", 0x00
input:
  db 0x1b, 0x56, 0x0f, 0x1e, 0x0a, 0x54, 0x04, 0x5c, 0x11, 0x1e, 0x01, 0x00


