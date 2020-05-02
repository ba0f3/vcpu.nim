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
  mov r2, [r0]
  mov r3, [r1]
  cmp r2, r3
  je check_null
  jle lower
  jge greater

check_null:
  cmp r2, 0x00
  ; null reached
  je equal
  inc r0
  inc r1
  jmp strcmp

equal:
  mov r0, 0
  halt
lower:
  mov r0, 0xff
  halt
greater:
  mov r0, 0x01
  halt
