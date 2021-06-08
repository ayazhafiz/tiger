; syntax:nasm
BITS 64
section .text

extern initArray

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, 10                             
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r10, 0                              
  imul r10, r10, 8                        
  add rax, r10                            
  mov rax, [rax]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret