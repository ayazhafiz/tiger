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
  mov rsi, 5                              ; arg2:initArray
  call initArray                          
  mov [rbp - 16], rax                     ; store spilled t17
  mov r10, 2                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov r11, [rbp - 16]                     ; fetch spilled t17
  mov qword [r10], r11                    ; .arr1=a
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov r11, [rbp - 16]                     ; fetch spilled t17
  mov qword [r10], r11                    ; .arr2=a
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, 0                              
  imul r10, r10, 8                        
  add rax, r10                            
  mov r10, [rax]                          ; b.arr1
  add r10, r11                            
  mov rax, [r10]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret