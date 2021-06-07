; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rax, 0                              ; var a := 0
  mov r10, 0                              ; var i := 0
  mov r8, 100                             ; var limit := 100
test:                                     
  mov r11, 1                              ; true
  cmp r10, r8                             ; i <= limit
  jle true                                
false:                                    
  mov r11, 0                              ; false
true:                                     
  cmp r11, 0                              ; i <= limit
  je break
body:              
  inc rax
  inc r10
  jmp test
break:              
  mov rsp, rbp
  pop rbp
  ret