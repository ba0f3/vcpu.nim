main:
  push r0
  push r1
loop:
  mov r4, [r0]
  mov r3, [r1]
  xor r4, r3
  mov [r0], r4
  inc r0
  inc r1
  mov r2, [r0]
  cmp r2, 0x00
  jz end
  mov r3, [r1]
  cmp cl, 0
  jz reset_key
  jmp loop

reset_key:
  pop r1
  push r1
  jmp loop

end:
  pop r1
  prntx
  halt