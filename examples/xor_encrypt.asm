%include "vm.inc"

%define i 0

main:
  movd r0, input
  movd r1, key
  movd r7, 0x0
  jmp loop

end:
  movd r0, input
  push r0
  prnts
  halt

encode:
  ret

loop:
  movmrb r4, r0
  xor r4, r1
  movrmb r0, r4

  inc r0
  inc r1
  movmrb r2, r0
  cmp r2, r7
  jz end
  movmrb r3, r1
  cmp r3, r7
  jz reset_key
  jmp loop

reset_key:
  movd r1, key
  jmp loop

key:
  db "s3cret", 0
input:
  db 35, 41, 33, 34, 32, 112, 60, 35, 63, 34, 43, 0
