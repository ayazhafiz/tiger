; syntax:nasm
BITS 64
section .text

extern initArray
extern print

global _start

str__success:
  dq 7
  db `success`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov [rbp - 40], rbx                     ; store spilled t24
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
  mov rbx, rbp                            
  sub rbx, 24                             
  mov rcx, [rbp - 32]                     ; fetch spilled t21
  xor rcx, rcx                            ; 0
  mov [rbp - 32], rcx                     ; store spilled t21
  mov rcx, [rbp - 32]                     ; fetch spilled t21
  mov [rbx + 0], rcx                      ; b[0] = 0
  mov rcx, [rbp - 32]                     ; fetch spilled t21
  mov [rbx + 8], rcx                      ; b[1] = 0
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rbx                            
  add rcx, rdx                            
  mov qword [rcx], rax                    ; .aa=a
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rbx                            
  add rcx, rdx                            
  mov qword [rcx], rax                    ; .bb=a
  mov rax, 1                              
  imul rdx, rax, 8                        
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rax, [rax]                          ; b.aa
  add rax, rdx                            
  mov rdi, [rax]                          ; %arg1:print
  call print                              
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, 0                              
  imul rax, rax, 8                        
  add rbx, rax                            
  mov rax, [rbx]                          ; b.aa
  add rax, rcx                            
  mov rax, [rax]                          ; return b.aa.a
  mov rbx, [rbp - 40]                     ; fetch spilled t24
  mov rsp, rbp
  pop rbp
  ret