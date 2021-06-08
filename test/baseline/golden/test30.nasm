; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 96
  mov [rbp - 8], rdi                      ; static link
  mov r10, rbp                            
  sub r10, 88                             
  xor r11, r11                            ; 0
  mov [r10 + 0], r11                      ; arr1[0] = 0
  mov [r10 + 8], r11                      ; arr1[1] = 0
  mov [r10 + 16], r11                     ; arr1[2] = 0
  mov [r10 + 24], r11                     ; arr1[3] = 0
  mov [r10 + 32], r11                     ; arr1[4] = 0
  mov [r10 + 40], r11                     ; arr1[5] = 0
  mov [r10 + 48], r11                     ; arr1[6] = 0
  mov [r10 + 56], r11                     ; arr1[7] = 0
  mov [r10 + 64], r11                     ; arr1[8] = 0
  mov [r10 + 72], r11                     ; arr1[9] = 0
  mov r11, 2                              
  imul r11, r11, 8                        
  add r10, r11                            
  mov rax, [r10]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret