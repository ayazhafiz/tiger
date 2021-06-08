; syntax:nasm
BITS 64
section .text

extern initArray

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 8], rdi                      ; static link
  mov rax, 2                              
  imul rdi, rax, 8                        
  mov rsi, 5                              ; arg2:initArray
  call initArray                          
  mov rcx, rbp                            
  sub rcx, 24                             
  mov rdx, [rbp - 32]                     ; fetch spilled t20
  xor rdx, rdx                            ; 0
  mov [rbp - 32], rdx                     ; store spilled t20
  mov rdx, [rbp - 32]                     ; fetch spilled t20
  mov [rcx + 0], rdx                      ; b[0] = 0
  mov rdx, [rbp - 32]                     ; fetch spilled t20
  mov [rcx + 8], rdx                      ; b[1] = 0
  mov rdx, 0                              
  imul rsi, rdx, 8                        
  mov rdx, rcx                            
  add rdx, rsi                            
  mov qword [rdx], rax                    ; .arr1=a
  mov rdx, 1                              
  imul rsi, rdx, 8                        
  mov rdx, rcx                            
  add rdx, rsi                            
  mov qword [rdx], rax                    ; .arr2=a
  mov rax, 0                              
  imul rdx, rax, 8                        
  mov rax, 0                              
  imul rax, rax, 8                        
  add rcx, rax                            
  mov rax, [rcx]                          ; b.arr1
  add rax, rdx                            
  mov rax, [rax]                          ; return b.arr1[0]
  mov rsp, rbp
  pop rbp
  ret