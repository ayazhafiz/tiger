BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov r11, 1
  mov r10, 10
  cmp r10, 20
  jg true
false:              
  mov r11, 0
true:              
  cmp r11, 0
  jne true1
false1:              
  mov rax, 40
join:              
  jmp done
true1:              
  mov rax, 30
  jmp join
done:              
  mov rsp, rbp
  pop rbp
  ret