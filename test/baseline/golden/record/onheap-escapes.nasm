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
  sub rsp, 32                             
  mov [rbp - 24], rbx                     ; store spilled t26
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rbp                            
  sub rbx, 16                             
  mov rax, 2                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov rsi, 5                              
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .a=5
  lea rsi, [rel str__success]             
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; .b="success"
  mov qword [rbx], rax                    ; var a := rcd { a=5, b="success" }
  mov rdi, rbp                            ; %arg(static_link):inner
  call inner                              
  mov rbx, [rbp - 24]                     ; fetch spilled t26
  mov rdi, rax
  call TTexit
inner:                                    
  push rbp                                
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; a
  add rax, rcx                            
  mov rdi, [rax]                          ; %arg1:print
  call print                              
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; a
  add rax, rcx                            
  mov rax, [rax]                          ; return a.a
  mov rsp, rbp
  pop rbp
  ret