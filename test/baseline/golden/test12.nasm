; syntax:nasm
BITS 64
section .text

extern TTexit

global _start

_start:                                   
  and rsp, 0xFFFFFFFFFFFFFFF0             ; 16-byte alignment
  mov rbp, rsp                            
  sub rsp, 16                             
  mov [rbp - 8], rdi                      ; static link
  xor rax, rax                            ; var a := 0
  xor rcx, rcx                            ; var i := 0
  mov rsi, 100                            ; var limit := 100
test:                                     
  mov rdx, 1                              ; true
  cmp rcx, rsi                            ; i <= limit
  jle true                                
false:                                    
  xor rdx, rdx                            ; false
true:                                     
  cmp rdx, 0                              ; i <= limit
  je break
body:              
  inc rax
  inc rcx
  jmp test
break:              
  mov rdi, rax
  call TTexit