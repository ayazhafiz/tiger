; syntax:nasm
BITS 64
section .text

global _start

str__str2:
  dq 4
  db `str2`
str__str:
  dq 3
  db `str`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov qword [rbp - 16], 0                 ; var cnt := 0
  mov rdi, rbp                            ; %arg(static_link):do_nothing1
  mov rsi, 0                              ; %arg1:do_nothing1
  lea rdx, [rel str__str2]                
  call do_nothing1                        
  mov rax, [rbp - 16]                     ; return ()
  mov rsp, rbp
  pop rbp
  ret
do_nothing2:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; cnt
  inc r10                                 
  mov r11, [rbp - 8]                      ; static link
  mov [r11 - 16], r10                     ; cnt := cnt + 1
  mov rdi, [rbp - 8]                      ; %arg(static_link):do_nothing1
  lea rdx, [rel str__str]
  call do_nothing1
  mov rsp, rbp
  pop rbp
  ret
do_nothing1:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; cnt
  cmp r10, 100                            ; cnt < 100
  jl true                                 
false:                                    
  jmp done2                               
true:                                     
  mov rdi, [rbp - 8]                      ; %arg(static_link):do_nothing2
  inc rsi
  call do_nothing2
  jmp false
done2:              
  mov rsp, rbp
  pop rbp
  ret