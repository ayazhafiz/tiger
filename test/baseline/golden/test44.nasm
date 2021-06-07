; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, 0                              ; var b : rectype := nil
  mov r10, 0                              ; b := nil
  mov rax, 0                              ; return ()
  mov rsp, rbp
  pop rbp
  ret