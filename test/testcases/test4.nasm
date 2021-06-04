; syntax:nasm
BITS 64
section .text

extern print
extern string_of_int

global _start

str__:
  dq 1
  db `\n`
str__10:
  dq 6
  db `10! = `
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  lea rdi, [rel str__10]                  
  call print                              
  mov rdi, rbp                            ; %arg(static_link):nfactor
  mov rsi, 10                             ; %arg1:nfactor
  call nfactor                            
  mov rdi, rax                            ; %arg1:string_of_int
  call string_of_int                      
  mov rdi, rax                            ; %arg1:print
  call print                              
  lea rdi, [rel str__]                    
  call print                              
  mov rax, 0                              ; return ()
  mov rsp, rbp
  pop rbp
  ret
nfactor:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 16], rbx                     ; store spilled t22
  mov [rbp - 8], rdi                      ; static link
  cmp rsi, 0                              ; n = 0
  je true                                 
false:                                    
  mov rbx, rsi                            ; push stmt up
  mov rdi, [rbp - 8]                      ; %arg(static_link):nfactor
  dec rsi                                 
  call nfactor                            
  imul rbx, rax                           
  mov rax, rbx                            ; else
join:                                     
  mov rbx, [rbp - 16]                     ; fetch spilled t22
  jmp done1                               
true:                                     
  mov rax, 1                              ; then
  jmp join
done1:              
  mov rsp, rbp
  pop rbp
  ret