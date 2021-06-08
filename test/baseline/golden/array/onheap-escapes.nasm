; syntax:nasm
BITS 64
section .text

extern initArray

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 24], rbx                     ; store spilled t24
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rbp                            
  sub rbx, 16                             
  mov rax, 2                              
  imul rdi, rax, 8                        
  mov rsi, 5                              ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var a := intarray[2] of 5
  mov rdi, rbp                            ; %arg(static_link):inner
  call inner                              
  mov rbx, [rbp - 24]                     ; fetch spilled t24
  mov rsp, rbp
  pop rbp
  ret
inner:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; a
  add rax, rcx                            
  mov rax, [rax]                          ; return (a[0])
  mov rsp, rbp
  pop rbp
  ret