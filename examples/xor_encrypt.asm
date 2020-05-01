main:
  push r0
  push r1
loop:
  mov l3, [l0]      ; copy input to r4
  mov l2, [l1]      ; copy key to r3
  xor l3, l2        ; xor input and key
  mov [l0], l3      ; move back result to r1
  inc l0            ; move r0 and r1 one byte forward
  inc l1
  cmp [l0], 0x00    ; check if input is end (null byte)
  jz end            ; jump to `end` if null
  cmp [l1], 0       ; check if key is end
  jz reset_key      ; jump to `reset_key` if null
  jmp loop          ; continue

reset_key:
  pop l1
  push l1
  jmp loop

end:
  pop l1
  prntx
  mov r0, 0
  halt