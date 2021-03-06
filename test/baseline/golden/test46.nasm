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
  xor rcx, rcx                            ; var b : rectype := nil
  mov rax, 1                              ; true
  cmp rcx, 0                              ; b = nil
  je true1                                
false1:                                   
  xor rax, rax                            ; false
true1:                                    
  mov rax, 1                              ; true
  cmp rcx, 0                              ; b <> nil
  jne true                                
false:                                    
  xor rax, rax                            ; false
true:              
  mov rdi, rax
  call TTexit