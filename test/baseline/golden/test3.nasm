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
  mov rax, 2                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  lea rsi, [rel str__Nobody]              
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .name="Nobody"
  mov rsi, 10                             
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .age=10
  lea rsi, [rel str__Somebody]            
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; rec1.name := "Somebody"
  mov rcx, 1                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret