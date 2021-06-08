; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  xor r11, r11                            ; var b : rectype := nil
  mov r10, 1                              ; true
  cmp r11, 0                              ; b = nil
  je true1                                
false1:                                   
  xor r10, r10                            ; false
true1:                                    
  mov rax, 1                              ; true
  cmp r11, 0                              ; b <> nil
  jne true                                
false:                                    
  xor rax, rax                            ; false
true:              
  mov rsp, rbp
  pop rbp
  ret