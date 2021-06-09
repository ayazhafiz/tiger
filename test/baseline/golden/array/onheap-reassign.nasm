; syntax:nasm
BITS 64
section .text

extern TTexit
extern initArray

global _start

_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, 2                              
  imul rdi, rax, 8                        
  mov rsi, 5                              ; arg2:initArray
  call initArray                          
  mov rcx, 0                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; return b[0]
  mov rdi, rax
  call TTexit