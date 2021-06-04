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
  mov [rbp - 56], rbx                     ; store spilled t46
  mov [rbp - 8], rdi                      ; static link
  mov qword [rbp - 16], 8                 ; var N := 8
  mov rbx, rbp                            
  sub rbx, 24                             
  mov r10, [rbp - 16]                     ; N
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var row := intArray[N] of 0
  mov rbx, rbp                            
  sub rbx, 32                             
  mov r10, [rbp - 16]                     ; N
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var col := intArray[N] of 0
  mov rbx, rbp                            
  sub rbx, 40                             
  mov r11, [rbp - 16]                     ; N
  mov r10, [rbp - 16]                     ; N
  add r10, r11                            
  dec r10                                 
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var diag1 := intArray[N + N - 1] of 0
  mov rbx, rbp                            
  sub rbx, 48                             
  mov r11, [rbp - 16]                     ; N
  mov r10, [rbp - 16]                     ; N
  add r10, r11                            
  dec r10                                 
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov qword [rbx], rax                    ; var diag2 := intArray[N + N - 1] of 0
  mov rdi, rbp                            ; %arg(static_link):try
  mov rsi, 0                              ; %arg1:try
  call try                                
  mov rax, 0                              ; return ()
  mov rbx, [rbp - 56]                     ; fetch spilled t46
  mov rsp, rbp
  pop rbp
  ret
try:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 8], rdi                      ; static link
  mov [rbp - 16], rsi                     ; store spilled t17
  mov r10, [rbp - 8]                      ; static link
  mov r11, [r10 - 16]                     ; N
  mov r10, [rbp - 16]                     ; fetch spilled t17
  cmp r10, r11                            ; c = N
  je true5                                
false5:                                   
  mov r10, 0                              ; var r := 0
  mov [rbp - 24], r10                     ; store spilled t33
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; N
  dec r10                                 
  mov [rbp - 32], r10                     ; store spilled t34
test2:                                    
  mov r8, 1                               ; true
  mov r11, [rbp - 24]                     ; fetch spilled t33
  mov r10, [rbp - 32]                     ; fetch spilled t34
  cmp r11, r10                            ; r <= limit
  jle true4                               
false4:                                   
  mov r8, 0                               ; false
true4:                                    
  cmp r8, 0                               ; r <= limit
  je break2                               
body2:                                    
  mov r10, [rbp - 24]                     ; fetch spilled t33
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 24]                     ; row
  add r10, r11                            
  mov r10, [r10]                          ; row[r]
  cmp r10, 0                              ; row[r] = 0
  je if_t1                                
if_f1:                                    
if_f:                                     
false3:                                   
  mov r10, [rbp - 24]                     ; fetch spilled t33
  inc r10                                 
  mov [rbp - 24], r10                     ; store spilled t33
  jmp test2                               
true5:                                    
  mov rdi, [rbp - 8]                      ; %arg(static_link):printboard
  call printboard                         
join1:                                    
  jmp done1                               
if_t1:                                    
  mov r10, [rbp - 24]                     ; fetch spilled t33
  mov r11, [rbp - 16]                     ; fetch spilled t17
  add r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 40]                     ; diag1
  add r10, r11                            
  mov r10, [r10]                          ; diag1[r + c]
  cmp r10, 0                              ; !(diag1[r + c] = 0)
  jne if_f                                
if_t:                                     
  mov r10, [rbp - 24]                     ; fetch spilled t33
  add r10, 7                              
  mov r11, [rbp - 16]                     ; fetch spilled t17
  sub r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 48]                     ; diag2
  add r10, r11                            
  mov r10, [r10]                          ; diag2[r + 7 - c]
  cmp r10, 0                              ; !(diag2[r + 7 - c] = 0)
  jne false3                              
true3:                                    
  mov r8, 1                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 24]                     ; row
  add r10, r11                            
  mov qword [r10], r8                     ; row[r] := 1
  mov r8, 1                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  mov r11, [rbp - 16]                     ; fetch spilled t17
  add r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 40]                     ; diag1
  add r10, r11                            
  mov qword [r10], r8                     ; diag1[r + c] := 1
  mov r8, 1                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  add r10, 7                              
  mov r11, [rbp - 16]                     ; fetch spilled t17
  sub r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 48]                     ; diag2
  add r10, r11                            
  mov qword [r10], r8                     ; diag2[r + 7 - c] := 1
  mov r10, [rbp - 16]                     ; fetch spilled t17
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 32]                     ; col
  add r10, r11                            
  mov r11, [rbp - 24]                     ; fetch spilled t33
  mov qword [r10], r11                    ; col[c] := r
  mov rdi, [rbp - 8]                      ; %arg(static_link):try
  mov rsi, [rbp - 16]                     ; fetch spilled t17
  inc rsi                                 
  call try                                
  mov r8, 0                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 24]                     ; row
  add r10, r11                            
  mov qword [r10], r8                     ; row[r] := 0
  mov r8, 0                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  mov r11, [rbp - 16]                     ; fetch spilled t17
  add r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 40]                     ; diag1
  add r10, r11                            
  mov qword [r10], r8                     ; diag1[r + c] := 0
  mov r8, 0                               
  mov r10, [rbp - 24]                     ; fetch spilled t33
  add r10, 7                              
  mov r11, [rbp - 16]                     ; fetch spilled t17
  sub r10, r11                            
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 48]                     ; diag2
  add r10, r11                            
  mov qword [r10], r8                     ; diag2[r + 7 - c] := 0
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
  mov r10, 0                              ; var i := 0
  mov [rbp - 16], r10                     ; store spilled t18
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; N
  dec r10                                 
  mov [rbp - 24], r10                     ; store spilled t19
test1:                                    
  mov r8, 1                               ; true
  mov r11, [rbp - 16]                     ; fetch spilled t18
  mov r10, [rbp - 24]                     ; fetch spilled t19
  cmp r11, r10                            ; i <= limit
  jle true2                               
false2:                                   
  mov r8, 0                               ; false
true2:                                    
  cmp r8, 0                               ; i <= limit
  je break                                
body1:                                    
  mov r10, 0                              ; var j := 0
  mov [rbp - 32], r10                     ; store spilled t20
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 16]                     ; N
  dec r10                                 
  mov [rbp - 40], r10                     ; store spilled t21
test:                                     
  mov r8, 1                               ; true
  mov r11, [rbp - 32]                     ; fetch spilled t20
  mov r10, [rbp - 40]                     ; fetch spilled t21
  cmp r11, r10                            ; j <= limit
  jle true1                               
false1:                                   
  mov r8, 0                               ; false
true1:                                    
  cmp r8, 0                               ; j <= limit
  je break1                               
body:                                     
  mov r10, [rbp - 16]                     ; fetch spilled t18
  imul r11, r10, 8                        
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 32]                     ; col
  add r10, r11                            
  mov r11, [r10]                          ; col[i]
  mov r10, [rbp - 32]                     ; fetch spilled t20
  cmp r11, r10                            ; col[i] = j
  je true                                 
false:                                    
  lea rdi, [rel str__]                    
join:                                     
  call print                              
  mov r10, [rbp - 32]                     ; fetch spilled t20
  inc r10                                 
  mov [rbp - 32], r10                     ; store spilled t20
  jmp test                                
true:                                     
  lea rdi, [rel str__O]                   
  jmp join                                
break1:                                   
  lea rdi, [rel str__1]                   
  call print                              
  mov r10, [rbp - 16]                     ; fetch spilled t18
  inc r10                                 
  mov [rbp - 16], r10                     ; store spilled t18
  jmp test1
break:              
  lea rdi, [rel str__1]
  call print
  mov rsp, rbp
  pop rbp
  ret