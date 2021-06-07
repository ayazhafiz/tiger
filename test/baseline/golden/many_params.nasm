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
  mov r10, 6                              ; %arg6:sum15
  mov [rbp - 16], r10                     ; store spilled t44
  mov r10, 7                              ; %arg7:sum15
  mov [rbp - 24], r10                     ; store spilled t45
  mov r10, 8                              ; %arg8:sum15
  mov [rbp - 32], r10                     ; store spilled t46
  mov r10, 9                              ; %arg9:sum15
  mov [rbp - 40], r10                     ; store spilled t47
  mov r10, 10                             ; %arg10:sum15
  mov [rbp - 48], r10                     ; store spilled t48
  mov r10, 11                             ; %arg11:sum15
  mov [rbp - 56], r10                     ; store spilled t49
  mov r10, 12                             ; %arg12:sum15
  mov [rbp - 64], r10                     ; store spilled t50
  mov r10, 13                             ; %arg13:sum15
  mov [rbp - 72], r10                     ; store spilled t51
  mov rax, 14                             ; %arg14:sum15
  mov r11, 15                             ; %arg15:sum15
  mov r10, 16                             ; %arg16:sum15
  sub rsp, 8                              ; Pad stack for alignment
  push r10                                ; arg17:sum15
  push r11                                ; arg16:sum15
  push rax                                ; arg15:sum15
  mov r10, [rbp - 72]                     ; fetch spilled t51
  push r10                                ; arg14:sum15
  mov r10, [rbp - 64]                     ; fetch spilled t50
  push r10                                ; arg13:sum15
  mov r10, [rbp - 56]                     ; fetch spilled t49
  push r10                                ; arg12:sum15
  mov r10, [rbp - 48]                     ; fetch spilled t48
  push r10                                ; arg11:sum15
  mov r10, [rbp - 40]                     ; fetch spilled t47
  push r10                                ; arg10:sum15
  mov r10, [rbp - 32]                     ; fetch spilled t46
  push r10                                ; arg9:sum15
  mov r10, [rbp - 24]                     ; fetch spilled t45
  push r10                                ; arg8:sum15
  mov r10, [rbp - 16]                     ; fetch spilled t44
  push r10                                ; arg7:sum15
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
  mov [rbp - 16], rdx                     ; store spilled t18
  mov [rbp - 24], rcx                     ; store spilled t19
  mov [rbp - 32], r8                      ; store spilled t20
  mov [rbp - 40], r9                      ; store spilled t21
  mov r10, [rbp + 16]                     ; n6
  mov [rbp - 48], r10                     ; store spilled t22
  mov r10, [rbp + 24]                     ; n7
  mov [rbp - 56], r10                     ; store spilled t23
  mov r10, [rbp + 32]                     ; n8
  mov [rbp - 64], r10                     ; store spilled t24
  mov r10, [rbp + 40]                     ; n9
  mov [rbp - 72], r10                     ; store spilled t25
  mov rdx, [rbp + 48]                     ; n10
  mov rdi, [rbp + 56]                     ; n11
  mov rcx, [rbp + 64]                     ; n12
  mov r9, [rbp + 72]                      ; n13
  mov r8, [rbp + 80]                      ; n14
  mov r11, [rbp + 88]                     ; n15
  mov r10, [rbp + 96]                     ; n16
  mov rax, rsi                            
  mov rsi, [rbp - 16]                     ; fetch spilled t18
  add rax, rsi                            
  mov rsi, [rbp - 24]                     ; fetch spilled t19
  add rax, rsi                            
  mov rsi, [rbp - 32]                     ; fetch spilled t20
  add rax, rsi                            
  mov rsi, [rbp - 40]                     ; fetch spilled t21
  add rax, rsi                            
  mov rsi, [rbp - 48]                     ; fetch spilled t22
  add rax, rsi                            
  mov rsi, [rbp - 56]                     ; fetch spilled t23
  add rax, rsi                            
  mov rsi, [rbp - 64]                     ; fetch spilled t24
  add rax, rsi                            
  mov rsi, [rbp - 72]                     ; fetch spilled t25
  add rax, rsi
  add rax, rdx
  add rax, rdi
  add rax, rcx
  add rax, r9
  add rax, r8
  add rax, r11
  add rax, r10
  mov rsp, rbp
  pop rbp
  ret