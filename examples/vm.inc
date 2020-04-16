[org 0x0]

; Registers.
%define r0 0
%define r1 1
%define r2 2
%define r3 3
%define r4 4
%define r5 5
%define r6 6
%define r7 7

%macro nop 0
db 0x00
%endmacro

%macro halt 0
db 0xde
%endmacro

%macro mov 2
db 0x01, %1, %2
%endmacro

%macro movmb 2
db 0x02, %1
dd %2
%endmacro

%macro movmw 2
db 0x03, %1
dd %2
%endmacro

%macro movb 2
db 0x04, %1, %2
%endmacro

%macro movw 2
db 0x05, %1
dw %2
%endmacro

%macro movbm 2
db 0x06
dw %1
db %2
%endmacro

%macro movwm 2
db 0x07
dw %1
db %2
%endmacro

%macro movmrb 2
db 0x08, %1, %2
%endmacro

%macro movmrw 2
db 0x09, %1, %2
%endmacro

%macro movmd 2
db 0x0a, %1, %2
%endmacro

%macro movd 2
db 0x0b, %1
dd %2
%endmacro

%macro movdm 2
db 0x0c, %1, %2
%endmacro

%macro movmrd 2
db 0x0d, %1, %2
%endmacro

%macro movrmb 2
db 0x0e, %1, %2
%endmacro

%macro movrmw 2
db 0x0f, %1, %2
%endmacro

%macro movrmd 2
db 0x10, %1, %2
%endmacro

%macro jmp 1
db 0x20
dw %1
%endmacro

%macro jz 1
db 0x21
dw %1
%endmacro

%macro jnz 1
db 0x22
dw %1
%endmacro

%macro jae 1
db 0x23
dw %1
%endmacro

%macro jbe 1
db 0x24
dw %1
%endmacro

%macro jb 1
db 0x25
dw %1
%endmacro

%macro ja 1
db 0x26
dw %1
%endmacro

%macro advr 2
db 0x27, %1
dw %2
%endmacro

%macro adrr 2
db 0x28, %1, %2
%endmacro

%macro adrrl 2
db 0x29, %1, %2
%endmacro

%macro subvr 2
db 0x2a, %1
dw %2
%endmacro

%macro subrr 2
db 0x2b, %1, %2
%endmacro

%macro subrrl 2
db 0x2c, %1, %2
%endmacro

%macro xor 2
db 0x2d, %1, %2
%endmacro

%macro xorl 2
db 0x2e, %1, %2
%endmacro

%macro not 1
db 0x2f, %1
%endmacro

%macro notb 1
db 0x30, %1
%endmacro

%macro advrd 2
db 0x31, %1
dd %2
%endmacro

%macro subvrd 2
db 0x32, %1
dd %2
%endmacro

%macro cmp 2
db 0x50, %1, %2
%endmacro

%macro cmpl 2
db 0x51, %1, %2
%endmacro

%macro cmpb 2
db 0x52, %1, %2
%endmacro

%macro cmpw 2
db 0x53, %1
dw %2
%endmacro

%macro cmpd 2
db 0x54, %1
dd %2
%endmacro

%macro inc 1
db 0x60, %1
%endmacro

%macro dec 1
db 0x61, %1
%endmacro

%macro call 1
db 0x62
dw %1
%endmacro

%macro ret 0
db 0x63
%endmacro

%macro push 1
db 0x90, %1
%endmacro

%macro pop 1
db 0x91, %1
%endmacro

%macro prnt 0
db 0xb0
%endmacro

%macro prntx 0
db 0xb1
%endmacro

%macro prnts 0
db 0xb2
%endmacro