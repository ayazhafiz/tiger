; syntax:nasm
BITS 64
section .text

extern initArray
extern print

global _start

str__1:
  dq 1
  db `\n`
str__O:
  dq 2
  db ` O`
str__:
  dq 2
  db ` .`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov [rbp - 64], rbx                     ; store spilled t46
  mov [rbp - 8], rdi                      ; static link
  mov qword [rbp - 16], 8                 ; var N := 8
  mov rbx, rbp                            
  sub rbx, 24                             
  mov rax, [rbp - 16]                     ; N
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var row := intArray[N] of 0
  mov rbx, rbp                            
  sub rbx, 32                             
  mov rax, [rbp - 16]                     ; N
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var col := intArray[N] of 0
  mov rbx, rbp                            
  sub rbx, 40                             
  mov rcx, [rbp - 16]                     ; N
  mov rax, [rbp - 16]                     ; N
  add rax, rcx                            
  dec rax                                 
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var diag1 := intArray[N + N - 1] of 0
  mov rbx, rbp                            
  sub rbx, 48                             
  mov rcx, [rbp - 16]                     ; N
  mov rax, [rbp - 16]                     ; N
  add rax, rcx                            
  dec rax                                 
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var diag2 := intArray[N + N - 1] of 0
  mov rdi, rbp                            ; %arg(static_link):try
  mov rax, [rbp - 56]                     ; fetch spilled t45
  xor rax, rax                            ; %arg1:try
  mov [rbp - 56], rax                     ; store spilled t45
  mov rsi, [rbp - 56]                     ; fetch spilled t45
  call try                                
  xor rax, rax                            ; return ()
  mov rbx, [rbp - 64]                     ; fetch spilled t46
  mov rsp, rbp
  pop rbp
  ret
try:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 8], rdi                      ; static link
  mov [rbp - 16], rsi                     ; store spilled t17
  mov rcx, [rbp - 8]                      ; static link
  mov rdx, [rcx - 16]                     ; N
  mov rcx, [rbp - 16]                     ; fetch spilled t17
  cmp rcx, rdx                            ; c = N
  je true5                                
false5:                                   
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  xor rcx, rcx                            ; var r := 0
  mov [rbp - 24], rcx                     ; store spilled t33
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 16]                     ; N
  dec rcx                                 
  mov [rbp - 32], rcx                     ; store spilled t34
test2:                                    
  mov rsi, 1                              ; true
  mov rdx, [rbp - 24]                     ; fetch spilled t33
  mov rcx, [rbp - 32]                     ; fetch spilled t34
  cmp rdx, rcx                            ; r <= limit
  jle true4                               
false4:                                   
  xor rsi, rsi                            ; false
true4:                                    
  cmp rsi, 0                              ; r <= limit
  je break2                               
body2:                                    
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 24]                     ; row
  add rcx, rdx                            
  mov rcx, [rcx]                          ; row[r]
  cmp rcx, 0                              ; row[r] = 0
  je if_t1                                
if_f1:                                    
if_f:                                     
false3:                                   
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  inc rcx                                 
  mov [rbp - 24], rcx                     ; store spilled t33
  jmp test2                               
true5:                                    
  mov rdi, [rbp - 8]                      ; %arg(static_link):printboard
  call printboard                         
join1:                                    
  jmp done1                               
if_t1:                                    
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  add rcx, rdx                            
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 40]                     ; diag1
  add rcx, rdx                            
  mov rcx, [rcx]                          ; diag1[r + c]
  cmp rcx, 0                              ; !(diag1[r + c] = 0)
  jne if_f                                
if_t:                                     
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  add rcx, 7                              
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  sub rcx, rdx                            
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 48]                     ; diag2
  add rcx, rdx                            
  mov rcx, [rcx]                          ; diag2[r + 7 - c]
  cmp rcx, 0                              ; !(diag2[r + 7 - c] = 0)
  jne false3                              
true3:                                    
  mov rdx, 1                              
  mov rax, [rbp - 24]                     ; fetch spilled t33
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 24]                     ; row
  add rax, rcx                            
  mov qword [rax], rdx                    ; row[r] := 1
  mov rdx, 1                              
  mov rax, [rbp - 24]                     ; fetch spilled t33
  mov rcx, [rbp - 16]                     ; fetch spilled t17
  add rax, rcx                            
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 40]                     ; diag1
  add rax, rcx                            
  mov qword [rax], rdx                    ; diag1[r + c] := 1
  mov rdx, 1                              
  mov rax, [rbp - 24]                     ; fetch spilled t33
  add rax, 7                              
  mov rcx, [rbp - 16]                     ; fetch spilled t17
  sub rax, rcx                            
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 48]                     ; diag2
  add rax, rcx                            
  mov qword [rax], rdx                    ; diag2[r + 7 - c] := 1
  mov rax, [rbp - 16]                     ; fetch spilled t17
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 32]                     ; col
  add rax, rcx                            
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  mov qword [rax], rcx                    ; col[c] := r
  mov rdi, [rbp - 8]                      ; %arg(static_link):try
  mov rsi, [rbp - 16]                     ; fetch spilled t17
  inc rsi                                 
  call try                                
  mov rsi, 0                              
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 24]                     ; row
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; row[r] := 0
  mov rsi, 0                              
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  add rcx, rdx                            
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 40]                     ; diag1
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; diag1[r + c] := 0
  mov rsi, 0                              
  mov rcx, [rbp - 24]                     ; fetch spilled t33
  add rcx, 7                              
  mov rdx, [rbp - 16]                     ; fetch spilled t17
  sub rcx, rdx                            
  imul rdx, rcx, 8                        
  mov rcx, [rbp - 8]                      ; static link
  mov rcx, [rcx - 48]                     ; diag2
  add rcx, rdx                            
  mov qword [rcx], rsi                    ; diag2[r + 7 - c] := 0
  jmp false3
break2:              
  jmp join1
done1:              
  mov rsp, rbp
  pop rbp
  ret
printboard:
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov [rbp - 8], rdi                      ; static link
  mov rax, [rbp - 16]                     ; fetch spilled t18
  xor rax, rax                            ; var i := 0
  mov [rbp - 16], rax                     ; store spilled t18
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; N
  dec rax                                 
  mov [rbp - 24], rax                     ; store spilled t19
test1:                                    
  mov rdx, 1                              ; true
  mov rcx, [rbp - 16]                     ; fetch spilled t18
  mov rax, [rbp - 24]                     ; fetch spilled t19
  cmp rcx, rax                            ; i <= limit
  jle true2                               
false2:                                   
  xor rdx, rdx                            ; false
true2:                                    
  cmp rdx, 0                              ; i <= limit
  je break                                
body1:                                    
  mov rax, [rbp - 32]                     ; fetch spilled t20
  xor rax, rax                            ; var j := 0
  mov [rbp - 32], rax                     ; store spilled t20
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 16]                     ; N
  dec rax                                 
  mov [rbp - 40], rax                     ; store spilled t21
test:                                     
  mov rdx, 1                              ; true
  mov rcx, [rbp - 32]                     ; fetch spilled t20
  mov rax, [rbp - 40]                     ; fetch spilled t21
  cmp rcx, rax                            ; j <= limit
  jle true1                               
false1:                                   
  xor rdx, rdx                            ; false
true1:                                    
  cmp rdx, 0                              ; j <= limit
  je break1                               
body:                                     
  mov rax, [rbp - 16]                     ; fetch spilled t18
  imul rcx, rax, 8                        
  mov rax, [rbp - 8]                      ; static link
  mov rax, [rax - 32]                     ; col
  add rax, rcx                            
  mov rcx, [rax]                          ; col[i]
  mov rax, [rbp - 32]                     ; fetch spilled t20
  cmp rcx, rax                            ; col[i] = j
  je true                                 
false:                                    
  lea rdi, [rel str__]                    
join:                                     
  call print                              
  mov rax, [rbp - 32]                     ; fetch spilled t20
  inc rax                                 
  mov [rbp - 32], rax                     ; store spilled t20
  jmp test                                
true:                                     
  lea rdi, [rel str__O]                   
  jmp join                                
break1:                                   
  lea rdi, [rel str__1]                   
  call print                              
  mov rax, [rbp - 16]                     ; fetch spilled t18
  inc rax                                 
  mov [rbp - 16], rax                     ; store spilled t18
  jmp test1
break:              
  lea rdi, [rel str__1]
  call print
  mov rsp, rbp
  pop rbp
  ret