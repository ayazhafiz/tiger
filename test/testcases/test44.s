BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov r10, 0
  mov r10, 0
  mov rax, 0
  mov rsp, rbp
  pop rbp
  ret