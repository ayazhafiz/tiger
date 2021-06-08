; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rcx, 4                              ; var b := 4
  xor rax, rax                            ; return 0
  mov rsp, rbp
  pop rbp
  ret