; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov rcx, 1                              ; true
  mov rax, 10                             
  cmp rax, 20                             ; 10 > 20
  jg true                                 
false:                                    
  xor rcx, rcx                            ; false
true:                                     
  cmp rcx, 0                              
  jne true1                               
false1:                                   
  mov rax, 40                             ; else
join:                                     
  jmp done                                
true1:                                    
  mov rax, 30                             ; then
  jmp join
done:              
  mov rsp, rbp
  pop rbp
  ret