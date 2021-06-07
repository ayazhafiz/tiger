; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, 4                              ; var b := 4
  mov rax, 0                              ; return ()
  mov rsp, rbp
  pop rbp
  ret