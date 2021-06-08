; syntax:nasm
BITS 64
section .text

global _start

str__:
  dq 1
  db ` `
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  xor rcx, rcx                            ; var a := 0
  lea rcx, [rel str__]                    
  xor rax, rax                            ; return 0
  mov rsp, rbp
  pop rbp
  ret