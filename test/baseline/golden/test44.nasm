; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  xor rcx, rcx                            ; var b : rectype := nil
  xor rcx, rcx                            ; b := nil
  xor rax, rax                            ; return ()
  mov rsp, rbp
  pop rbp
  ret