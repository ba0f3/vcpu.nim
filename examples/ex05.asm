
main: ; entry point
  push 3
  push 4
  call [add]
  pop l0
  assrt l0, 7
  halt
