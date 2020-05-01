
main:
  push 65
  push 1
  call add
  pop r2
  dump
  halt

add:
  pop bp
  pop r0
  pop r1
  add r0, r1
  push r0
  push bp
  ret
