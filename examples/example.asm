; Usr input (addr) saved in R0
; Answer: vmobfuscationisthebest

%include "vm.inc"

main:
  mov r1, r0
  movd r3, 0x10
  movd r2, 0x0
  movd r4, 0x0
  movd r5, 0x0
  advrd r3, 0x5

JLL9:
  movmrb r2, r1
  adrr r4, r2
  advrd r4, 0x5
  advrd r1, 0x1
  movmrb r2, r1
  cmp r2, r5
  jz JLL8

  adrr r4, r2
  advrd r4, 0xa
  advrd r1, 0x1
  movmrb r2, r1
  cmp r2, r5
  jz JLL8

  adrr r4, r2
  subvrd r4, 0x6
  advrd r1, 0x1
  movmrb r2, r1
  cmp r2, r5
  jz JLL8

  adrr r4, r2
  subvrd r4, 0xc
  advrd r1, 0x1
  movmrb r2, r1
  cmp r2, r5
  jz JLL8

  adrr r4, r2
  advrd r4, 0xf
  advrd r1, 0x1
  movmrb r2, r1
  cmp r2, r5
  jz JLL8

  jnz JLL9

JLL8:
  advrd r3, 0x5
  movd r5, 0x980
  xor r4, r5
  cmp r4, r3
  jz JLL6
  jnz JLL7

JLL6:
; PRINT "PASS"
  jmp JLL5

JLL7:
; PRINT "FAILED"
  jmp JLL4

JLL5:
  movd r1, data1
  jmp JLL3

JLL4:
  movd r1, data2
  jmp JLL3

JLL3:
  mov r2, r1
  advrd r2, 0x1
  movd r3, 0x0
  movd r4, 0x0

JLL1:
  mov r3, r1
  push r3
  prnts
  halt

data1:
  db "PASS", 0
data2:
  db "FAILED", 0