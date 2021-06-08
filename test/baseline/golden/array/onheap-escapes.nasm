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
  mov r10, 2                              
  imul rdi, r10, 8                        
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
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; a
  add r10, r11                            
  mov rax, [r10]                          ; return (a[0])
  mov rsp, rbp
  pop rbp
  ret