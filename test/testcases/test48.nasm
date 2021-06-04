; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rax, 0                              ; return ()
  mov rsp, rbp
  pop rbp
  ret
g1:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rax, rsi                            ; a
  mov rsp, rbp
  pop rbp
  ret
g:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rax, rsi                            ; a
  mov rsp, rbp
  pop rbp
  ret