; syntax:nasm
BITS 64
section .text

extern print

global _start

str__success:
  dq 7
  db `success`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 32], rbx                     ; store spilled t22
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rbp                            
  sub rbx, 24                             
  xor rax, rax                            ; 0
  mov [rbx + 0], rax                      ; a[0] = 0
  mov [rbx + 8], rax                      ; a[1] = 0
  mov rdx, 5                              
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .a=5
  lea rdx, [rel str__success]             
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .b="success"
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rdi, [rax]                          ; %arg1:print
  call print                              
  mov rax, 0                              
  imul rax, rax, 8                        
  add rbx, rax                            
  mov rax, [rbx]                          ; return a.a
  mov rbx, [rbp - 32]                     ; fetch spilled t22
  mov rsp, rbp
  pop rbp
  ret