; syntax:nasm
BITS 64
section .text

extern initArray

global _start

str__Somebody:
  dq 8
  db `Somebody`
str__Nobody:
  dq 6
  db `Nobody`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, 2                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  lea r8, [rel str__Nobody]               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r8                     ; .name="Nobody"
  mov r8, 10                              
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r8                     ; .age=10
  lea r8, [rel str__Somebody]             
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r8                     ; rec1.name := "Somebody"
  mov r10, 1                              
  imul r10, r10, 8                        
  add rax, r10                            
  mov rax, [rax]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret