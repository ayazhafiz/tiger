BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov rax, 0
  mov r10, 0
  mov r8, 100
test:              
  mov r11, 1
  cmp r10, r8
  jle true
false:              
  mov r11, 0
true:              
  cmp r11, 0
  je break
body:              
  inc rax
  inc r10
  jmp test
break:              
  mov rsp, rbp
  pop rbp
  ret