; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov rcx, 4                              ; var b := 4
  xor rax, rax                            ; return 0
  mov rdi, rax
  call TTexit