main:
  push r0
  push r1
loop:
  mov r4, [r0]      ; copy input to r4
  mov r3, [r1]      ; copy key to r3
  xor r4, r3        ; xor input and key
  mov [l0], r4      ; move back result to r1
  inc r0            ; move r0 and r1 one byte forward
  inc r1
  cmp [l0], 0x00    ; check if input is end (null byte)
  jz end            ; jump to `end` if null
  cmp [l1], 0       ; check if key is end
  jz reset_key      ; jump to `reset_key` if null
  jmp loop          ; continue

reset_key:
  pop r1
  push r1
  jmp loop

end:
  pop r1
  prntx
  halt