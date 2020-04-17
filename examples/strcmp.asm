%include "vm.inc"

; str1's address set to r0
; str2's address set to r1
; set r0 = 0 if to string equals
; r0 = -1 if the first character that does not match has a lower value in str1 than in str2
; r0 = 1 if the first character that does not match has a greater value in str1 than in str2
main:
  push r0
  prnts
  push r1
  prnts

strcmp:
  movmrb r2, r0
  movmrb r3, r1
  cmp r2, r3
  jz check_null
  jb lower
  ja greater

check_null:
  cmpb r2, 0
  ; null reached
  jz equal
  inc r0
  inc r1
  jmp strcmp

equal:
  movb r0, 0
  halt
lower:
  movb r0, 0xff
  halt
greater:
  movb r0, 1
  halt
