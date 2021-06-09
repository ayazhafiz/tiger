; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

str__Somebody:
  dq 8
  db `Somebody`
str__Nobody:
  dq 6
  db `Nobody`
_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 32                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, rbp                            
  sub rax, 24                             
  xor rcx, rcx                            ; 0
  mov [rax + 0], rcx                      ; rec1[0] = 0
  mov [rax + 8], rcx                      ; rec1[1] = 0
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
  mov rax, [rax]                          ; return rec1.age
  mov rdi, rax
  call TTexit