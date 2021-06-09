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
  xor rax, rax                            ; var a := 0
  mov rdi, rbp                            ; %arg(static_link):g
  mov rsi, 2                              ; %arg1:g
  call g
  mov rdi, rax
  call TTexit
g:                                        
  push rbp                                
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, rsi                            ; a
  mov rsp, rbp
  pop rbp
  ret