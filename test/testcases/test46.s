BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov r11, 0
  mov r10, 1
  cmp r11, 0
  je true1
false1:              
  mov r10, 0
true1:              
  mov rax, 1
  cmp r11, 0
  jne true
false:              
  mov rax, 0
true:              
  mov rsp, rbp
  pop rbp
  ret