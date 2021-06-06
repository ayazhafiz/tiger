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
  mov r10, 2                              
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov r8, 0                               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r8                     ; .hd=0
  mov r8, 0                               
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r8                     ; .tl=nil
  mov r10, 0                              
  imul r10, r10, 8                        
  add rax, r10                            
  mov rax, [rax]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret