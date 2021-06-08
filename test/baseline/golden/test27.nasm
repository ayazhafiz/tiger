; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  xor r10, r10                            ; var a := 0
  mov rdi, rbp                            ; %arg(static_link):g
  mov rsi, 2                              ; %arg1:g
  call g
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