; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov r11, 1                              ; true
  mov r10, 10                             
  cmp r10, 20                             ; 10 > 20
  jg true                                 
false:                                    
  xor r11, r11                            ; false
true:                                     
  cmp r11, 0                              
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