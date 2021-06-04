; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r11, 0                              ; var b : rectype := nil
  mov r10, 1                              ; true
  cmp r11, 0                              ; b = nil
  je true1                                
false1:                                   
  mov r10, 0                              ; false
true1:                                    
  mov rax, 1                              ; true
  cmp r11, 0                              ; b <> nil
  jne true                                
false:                                    
  mov rax, 0                              ; false
true:              
  mov rsp, rbp
  pop rbp
  ret