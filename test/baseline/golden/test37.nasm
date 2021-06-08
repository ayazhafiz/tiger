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
  xor r10, r10                            ; var a := 0
  lea r10, [rel str__]                    
  xor rax, rax                            ; return ()
  mov rsp, rbp
  pop rbp
  ret