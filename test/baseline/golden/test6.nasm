; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

str__str2:
  dq 4
  db `str2`
str__str:
  dq 3
  db `str`
_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov qword [rbp - 16], 0                 ; var cnt := 0
  mov rdi, rbp                            ; %arg(static_link):do_nothing1
  xor rsi, rsi                            ; %arg1:do_nothing1
  lea rdx, [rel str__str2]                
  call do_nothing1                        
  mov rax, [rbp - 16]                     ; return cnt
  mov rdi, rax
  call TTexit
do_nothing2:                              
  push rbp                                
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; cnt
  inc rax                                 
  mov rcx, [rbp - 8]                      ; static link
  mov [rcx - 16], rax                     ; cnt := cnt + 1
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
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 16]                     ; cnt
  cmp rcx, 100                            ; cnt < 100
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