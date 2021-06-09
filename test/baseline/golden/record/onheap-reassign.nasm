; syntax:nasm
BITS 64
section .text

extern TTexit
extern initArray
extern print

global _start

str__success:
  dq 7
  db `success`
_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 16], rbx                     ; store spilled t21
  mov [rbp - 8], rdi                      ; static link
  mov rax, 2                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov rdx, 5                              
  mov rbx, 0                              
  imul rcx, rbx, 8                        
  mov rbx, rax                            
  add rbx, rcx                            
  mov qword [rbx], rdx                    ; .a=5
  lea rdx, [rel str__success]             
  mov rbx, 1                              
  imul rcx, rbx, 8                        
  mov rbx, rax                            
  add rbx, rcx                            
  mov qword [rbx], rdx                    ; .b="success"
  mov rbx, rax                            ; var b := a
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rdi, [rax]                          ; %arg1:print
  call print                              
  mov rax, 0                              
  imul rax, rax, 8                        
  add rbx, rax                            
  mov rax, [rbx]                          ; return b.a
  mov rbx, [rbp - 16]                     ; fetch spilled t21
  mov rdi, rax
  call TTexit