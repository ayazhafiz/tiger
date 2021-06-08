; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 80
  mov [rbp - 8], rdi                      ; static link
  mov rdi, rbp                            ; %arg(static_link):sum15
  mov rsi, 1                              ; %arg1:sum15
  mov rdx, 2                              ; %arg2:sum15
  mov rcx, 3                              ; %arg3:sum15
  mov r8, 4                               ; %arg4:sum15
  mov r9, 5                               ; %arg5:sum15
  mov rax, 6                              ; %arg6:sum15
  mov [rbp - 16], rax                     ; store spilled t44
  mov rax, 7                              ; %arg7:sum15
  mov [rbp - 24], rax                     ; store spilled t45
  mov rax, 8                              ; %arg8:sum15
  mov [rbp - 32], rax                     ; store spilled t46
  mov rax, 9                              ; %arg9:sum15
  mov [rbp - 40], rax                     ; store spilled t47
  mov rax, 10                             ; %arg10:sum15
  mov [rbp - 48], rax                     ; store spilled t48
  mov rax, 11                             ; %arg11:sum15
  mov [rbp - 56], rax                     ; store spilled t49
  mov rax, 12                             ; %arg12:sum15
  mov [rbp - 64], rax                     ; store spilled t50
  mov rax, 13                             ; %arg13:sum15
  mov [rbp - 72], rax                     ; store spilled t51
  mov r11, 14                             ; %arg14:sum15
  mov r10, 15                             ; %arg15:sum15
  mov rax, 16                             ; %arg16:sum15
  sub rsp, 8                              ; Pad stack for alignment
  push rax                                ; arg17:sum15
  push r10                                ; arg16:sum15
  push r11                                ; arg15:sum15
  mov rax, [rbp - 72]                     ; fetch spilled t51
  push rax                                ; arg14:sum15
  mov rax, [rbp - 64]                     ; fetch spilled t50
  push rax                                ; arg13:sum15
  mov rax, [rbp - 56]                     ; fetch spilled t49
  push rax                                ; arg12:sum15
  mov rax, [rbp - 48]                     ; fetch spilled t48
  push rax                                ; arg11:sum15
  mov rax, [rbp - 40]                     ; fetch spilled t47
  push rax                                ; arg10:sum15
  mov rax, [rbp - 32]                     ; fetch spilled t46
  push rax                                ; arg9:sum15
  mov rax, [rbp - 24]                     ; fetch spilled t45
  push rax                                ; arg8:sum15
  mov rax, [rbp - 16]                     ; fetch spilled t44
  push rax                                ; arg7:sum15
  call sum15                              
  add rsp, 96                             ; Deallocate sum15 args
  mov rsp, rbp
  pop rbp
  ret
sum15:
  push rbp
  mov rbp, rsp
  sub rsp, 80
  mov [rbp - 8], rdi                      ; static link
  mov [rbp - 16], rsi                     ; store spilled t17
  mov [rbp - 24], rdx                     ; store spilled t18
  mov [rbp - 32], rcx                     ; store spilled t19
  mov [rbp - 40], r8                      ; store spilled t20
  mov [rbp - 48], r9                      ; store spilled t21
  mov rax, [rbp + 16]                     ; n6
  mov [rbp - 56], rax                     ; store spilled t22
  mov rax, [rbp + 24]                     ; n7
  mov [rbp - 64], rax                     ; store spilled t23
  mov rax, [rbp + 32]                     ; n8
  mov [rbp - 72], rax                     ; store spilled t24
  mov rax, [rbp + 40]                     ; n9
  mov [rbp - 80], rax                     ; store spilled t25
  mov r10, [rbp + 48]                     ; n10
  mov r9, [rbp + 56]                      ; n11
  mov r8, [rbp + 64]                      ; n12
  mov rdi, [rbp + 72]                     ; n13
  mov rsi, [rbp + 80]                     ; n14
  mov rdx, [rbp + 88]                     ; n15
  mov rcx, [rbp + 96]                     ; n16
  mov rax, [rbp - 16]                     ; fetch spilled t17
  mov r11, [rbp - 24]                     ; fetch spilled t18
  add rax, r11                            
  mov r11, [rbp - 32]                     ; fetch spilled t19
  add rax, r11                            
  mov r11, [rbp - 40]                     ; fetch spilled t20
  add rax, r11                            
  mov r11, [rbp - 48]                     ; fetch spilled t21
  add rax, r11                            
  mov r11, [rbp - 56]                     ; fetch spilled t22
  add rax, r11                            
  mov r11, [rbp - 64]                     ; fetch spilled t23
  add rax, r11                            
  mov r11, [rbp - 72]                     ; fetch spilled t24
  add rax, r11                            
  mov r11, [rbp - 80]                     ; fetch spilled t25
  add rax, r11
  add rax, r10
  add rax, r9
  add rax, r8
  add rax, rdi
  add rax, rsi
  add rax, rdx
  add rax, rcx
  mov rsp, rbp
  pop rbp
  ret