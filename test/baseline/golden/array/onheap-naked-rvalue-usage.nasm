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
  mov rax, 2                              
  imul rdi, rax, 8                        
  mov rsi, 5                              ; arg2:initArray
  call initArray                          
  mov [rbp - 16], rax                     ; store spilled t17
  mov rax, 2                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  mov qword [rcx], rdx                    ; .arr1=a
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  mov qword [rcx], rdx                    ; .arr2=a
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, 0                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; b.arr1
  add rax, rdx                            
  mov rax, [rax]                          ; return b.arr1[0]
  mov rsp, rbp
  pop rbp
  ret