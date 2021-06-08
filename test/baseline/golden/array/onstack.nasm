; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 8], rdi                      ; static link
  mov r10, rbp                            
  sub r10, 24                             
  mov r11, 5                              ; 5
  mov [r10 + 0], r11                      ; a[0] = 5
  mov [r10 + 8], r11                      ; a[1] = 5
  mov r11, 0                              
  imul r11, r11, 8                        
  add r10, r11                            
  mov rax, [r10]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret