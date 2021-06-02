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
  mov rdi, rbp
  mov rsi, 0
  lea rdx, [rel str__str2]
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
  mov rdi, [rbp - 8]
  lea rdx, [rel str__str]
  call do_nothing1
  mov rsp, rbp
  pop rbp
  ret
do_nothing1:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi
  mov rdi, [rbp - 8]
  inc rsi
  call do_nothing2
  mov rsp, rbp
  pop rbp
  ret