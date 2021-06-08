; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 96
  mov [rbp - 8], rdi                      ; static link
  mov rax, rbp                            
  sub rax, 88                             
  xor rcx, rcx                            ; 0
  mov [rax + 0], rcx                      ; arr1[0] = 0
  mov [rax + 8], rcx                      ; arr1[1] = 0
  mov [rax + 16], rcx                     ; arr1[2] = 0
  mov [rax + 24], rcx                     ; arr1[3] = 0
  mov [rax + 32], rcx                     ; arr1[4] = 0
  mov [rax + 40], rcx                     ; arr1[5] = 0
  mov [rax + 48], rcx                     ; arr1[6] = 0
  mov [rax + 56], rcx                     ; arr1[7] = 0
  mov [rax + 64], rcx                     ; arr1[8] = 0
  mov [rax + 72], rcx                     ; arr1[9] = 0
  mov rsi, 6                              
  mov rcx, 5                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; arr1[5] := 6
  mov rcx, 5                              
  imul rcx, rcx, 8                        
  add rax, rcx                            
  mov rax, [rax]                          ; return ()
  mov rsp, rbp
  pop rbp
  ret