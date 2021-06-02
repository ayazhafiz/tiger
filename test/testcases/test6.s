BITS 64
section .text

global _start

str__str2:
  dq 4
  db "str2"
str__str:
  dq 3
  db "str"
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  call do_nothing1
  mov rax, 0
  mov rsp, rbp
  pop rbp
  ret
do_nothing2:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov rsi, r10
  call do_nothing1
  mov rsp, rbp
  pop rbp
  ret
do_nothing1:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov rsi, r10
  call do_nothing2
  mov rsp, rbp
  pop rbp
  ret