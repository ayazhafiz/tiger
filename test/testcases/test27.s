BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov r10, 0
  mov rsi, 2
  mov rdi, rbp
  call g
  mov rsp, rbp
  pop rbp
  ret
g:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov rax, rsi
  mov rsp, rbp
  pop rbp
  ret