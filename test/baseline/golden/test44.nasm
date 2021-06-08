; syntax:nasm
BITS 64
section .text

global _start

_start:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  xor r10, r10                            ; var b : rectype := nil
  xor r10, r10                            ; b := nil
  xor rax, rax                            ; return ()
  mov rsp, rbp
  pop rbp
  ret