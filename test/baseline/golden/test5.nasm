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
  xor rcx, rcx                            ; 0
  mov [rax + 0], rcx                      ; lis[0] = 0
  mov [rax + 8], rcx                      ; lis[1] = 0
  mov rsi, 0                              
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .hd=0
  mov rsi, 0                              
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .tl=nil
  mov rcx, 0                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; return lis.hd
  mov rdi, rax
  call TTexit