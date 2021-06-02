BITS 64
section .text

global _start

str__:
  dq 1
  db " "
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov r10, 0
  lea r10, [rel str__]
  mov rax, 0
  mov rsp, rbp
  pop rbp
  ret