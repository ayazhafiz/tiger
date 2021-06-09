; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 32                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, rbp                            
  sub rax, 24                             
  mov rcx, 5                              ; 5
  mov [rax + 0], rcx                      ; a[0] = 5
  mov [rax + 8], rcx                      ; a[1] = 5
  mov rcx, 0                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; return a[0]
  mov rdi, rax
  call TTexit