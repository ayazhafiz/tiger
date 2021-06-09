; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

str__:
  dq 1
  db ` `
_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  xor rcx, rcx                            ; var a := 0
  lea rcx, [rel str__]                    
  xor rax, rax                            ; return 0
  mov rdi, rax
  call TTexit