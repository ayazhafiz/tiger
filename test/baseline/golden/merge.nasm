; syntax:nasm
BITS 64
section .text

extern chr
extern get_char
extern initArray
extern ord
extern print
extern stringEqual

global _start

str__2:
  dq 1
  db `\n`
str__1:
  dq 1
  db `-`
str__:
  dq 1
  db ` `
str__0:
  dq 1
  db `0`
str__9:
  dq 1
  db `9`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 32], rbx                     ; store spilled t143
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rbp                            
  sub rbx, 16                             
  call get_char                           
  mov qword [rbx], rax                    ; var buffer := get_char()
  mov rdi, rbp                            ; %arg(static_link):readlist
  call readlist                           
  mov [rbp - 24], rax                     ; store spilled t130
  mov rbx, rbp                            
  sub rbx, 16                             
  call get_char                           
  mov qword [rbx], rax                    ; buffer := get_char()
  mov rdi, rbp                            ; %arg(static_link):readlist
  call readlist                           
  mov rbx, rbp                            ; %arg(static_link):printlist
  mov rdi, rbp                            ; %arg(static_link):merge
  mov rsi, [rbp - 24]                     ; fetch spilled t130
  mov rdx, rax                            ; %arg2:merge
  call merge                              
  mov rsi, rax                            ; %arg1:printlist
  mov rdi, rbx                            ; arg1:printlist
  call printlist                          
  mov rax, 0                              ; return ()
  mov rbx, [rbp - 32]                     ; fetch spilled t143
  mov rsp, rbp
  pop rbp
  ret
printlist:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 16], rbx                     ; store spilled t125
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rsi                            ; l
  cmp rbx, 0                              ; l = nil
  je true8                                
false8:                                   
  mov rdi, [rbp - 8]                      ; %arg(static_link):printint
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rbx                            
  add r10, r11                            
  mov rsi, [r10]                          ; %arg1:printint
  call printint                           
  lea rdi, [rel str__]                    
  call print                              
  mov rdi, [rbp - 8]                      ; %arg(static_link):printlist
  mov r10, 1                              
  imul r10, r10, 8                        
  add rbx, r10                            
  mov rsi, [rbx]                          ; %arg1:printlist
  call printlist                          
join6:                                    
  mov rbx, [rbp - 16]                     ; fetch spilled t125
  jmp done1
true8:              
  lea rdi, [rel str__2]
  call print
  jmp join6
done1:              
  mov rsp, rbp
  pop rbp
  ret
printint:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 8], rdi                      ; static link
  mov [rbp - 16], rsi                     ; store spilled t60
  mov r10, [rbp - 16]                     ; fetch spilled t60
  cmp r10, 0                              ; i < 0
  jl true7                                
false7:                                   
  mov r10, [rbp - 16]                     ; fetch spilled t60
  cmp r10, 0                              ; i > 0
  jg true6                                
false6:                                   
  lea rdi, [rel str__0]                   
  call print                              
join4:                                    
join5:                                    
  jmp done2                               
true7:                                    
  lea rdi, [rel str__1]                   
  call print                              
  mov rdi, rbp                            ; %arg(static_link):f
  mov rsi, 0                              
  mov r10, [rbp - 16]                     ; fetch spilled t60
  sub rsi, r10                            
  call f                                  
  jmp join5                               
true6:                                    
  mov rdi, rbp                            ; %arg(static_link):f
  mov rsi, [rbp - 16]                     ; fetch spilled t60
  call f
  jmp join4
done2:              
  mov rsp, rbp
  pop rbp
  ret
f:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 16], rbx                     ; store spilled t103
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rsi                            ; i
  cmp rbx, 0                              ; i > 0
  jg true5                                
false5:                                   
  mov rbx, [rbp - 16]                     ; fetch spilled t103
  jmp done3                               
true5:                                    
  mov rdi, [rbp - 8]                      ; %arg(static_link):f
  mov r10, 10                             
  mov rax, rbx                            
  cqo                                     
  idiv r10                                
  mov rsi, rax                            ; quotient
  call f                                  
  mov r10, 10                             
  mov rax, rbx                            
  cqo                                     
  idiv r10                                
  imul r10, rax, 10                       
  sub rbx, r10                            
  lea rdi, [rel str__0]                   
  call ord                                
  mov rdi, rbx                            
  add rdi, rax                            
  call chr                                
  mov rdi, rax                            ; %arg1:print
  call print
  jmp false5
done3:              
  mov rsp, rbp
  pop rbp
  ret
merge:
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov [rbp - 24], rbx                     ; store spilled t90
  mov [rbp - 32], r12                     ; store spilled t91
  mov [rbp - 40], r13                     ; store spilled t92
  mov [rbp - 48], r14                     ; store spilled t93
  mov [rbp - 8], rdi                      ; static link
  mov r13, rsi                            ; a
  mov r14, rdx                            ; b
  cmp r13, 0                              ; a = nil
  je true4                                
false4:                                   
  cmp r14, 0                              ; b = nil
  je true3                                
false3:                                   
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov r12, [r10]                          ; b.first
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r13                            
  add r10, r11                            
  mov r10, [r10]                          ; a.first
  cmp r10, r12                            ; a.first < b.first
  jl true2                                
false2:                                   
  mov r10, 2                              
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov r12, rax                            
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov r8, [r10]                           ; b.first
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r12                            
  add r10, r11                            
  mov qword [r10], r8                     ; .first=b.first
  mov r10, 1                              
  imul r10, r10, 8                        
  mov rbx, r12                            
  add rbx, r10                            
  mov rdi, [rbp - 8]                      ; %arg(static_link):merge
  mov rsi, r13                            ; %arg1:merge
  mov r10, 1                              
  imul r10, r10, 8                        
  add r14, r10                            
  mov rdx, [r14]                          ; %arg2:merge
  call merge                              
  mov qword [rbx], rax                    ; .rest=merge(a, b.rest)
  mov [rbp - 16], r12                     ; store spilled t87
join1:                                    
  mov r13, [rbp - 16]                     ; fetch spilled t87
join2:                                    
  mov rax, r13                            ; else
join3:                                    
  mov rbx, [rbp - 24]                     ; fetch spilled t90
  mov r12, [rbp - 32]                     ; fetch spilled t91
  mov r13, [rbp - 40]                     ; fetch spilled t92
  mov r14, [rbp - 48]                     ; fetch spilled t93
  jmp done4                               
true4:                                    
  mov rax, r14                            ; then
  jmp join3                               
true3:                                    
  jmp join2                               
true2:                                    
  mov r10, 2                              
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov r12, rax                            
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r13                            
  add r10, r11                            
  mov r8, [r10]                           ; a.first
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r12                            
  add r10, r11                            
  mov qword [r10], r8                     ; .first=a.first
  mov r10, 1                              
  imul r10, r10, 8                        
  mov rbx, r12                            
  add rbx, r10                            
  mov rdi, [rbp - 8]                      ; %arg(static_link):merge
  mov r10, 1                              
  imul r10, r10, 8                        
  add r13, r10                            
  mov rsi, [r13]                          ; %arg1:merge
  mov rdx, r14                            ; %arg2:merge
  call merge                              
  mov qword [rbx], rax                    ; .rest=merge(a.rest, b)
  mov [rbp - 16], r12                     ; store spilled t87
  jmp join1
done4:              
  mov rsp, rbp
  pop rbp
  ret
readlist:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 32], rbx                     ; store spilled t72
  mov [rbp - 8], rdi                      ; static link
  mov r10, 1                              
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov rbx, rax                            
  mov r8, 0                               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rbx                            
  add r10, r11                            
  mov qword [r10], r8                     ; .any=0
  mov rdi, [rbp - 8]                      ; %arg(static_link):readint
  mov rsi, rbx                            ; %arg1:readint
  call readint                            
  mov [rbp - 16], rax                     ; store spilled t64
  mov r10, 0                              
  imul r10, r10, 8                        
  add rbx, r10                            
  mov r10, [rbx]                          ; any.any
  cmp r10, 0                              ; any.any
  jne true1                               
false1:                                   
  mov rax, 0                              ; else
join:                                     
  mov rbx, [rbp - 32]                     ; fetch spilled t72
  jmp done5                               
true1:                                    
  mov r10, 2                              
  imul rdi, r10, 8                        
  mov rsi, 0                              ; arg2:initArray
  call initArray                          
  mov [rbp - 24], rax                     ; store spilled t70
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, [rbp - 24]                     ; fetch spilled t70
  add r10, r11                            
  mov r11, [rbp - 16]                     ; fetch spilled t64
  mov qword [r10], r11                    ; .first=i
  mov r10, 1                              
  imul r10, r10, 8                        
  mov rbx, [rbp - 24]                     ; fetch spilled t70
  add rbx, r10                            
  mov rdi, [rbp - 8]                      ; %arg(static_link):readlist
  call readlist                           
  mov qword [rbx], rax                    ; .rest=readlist()
  mov rax, [rbp - 24]                     ; fetch spilled t70
  jmp join
done5:              
  mov rsp, rbp
  pop rbp
  ret
readint:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 24], rbx                     ; store spilled t53
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rsi                            ; any
  mov r10, 0                              ; var i := 0
  mov [rbp - 16], r10                     ; store spilled t19
  mov rdi, rbp                            ; %arg(static_link):skipto
  call skipto                             
  mov r10, 0                              
  imul r10, r10, 8                        
  add rbx, r10                            
  mov rdi, rbp                            ; %arg(static_link):isdigit
  mov r10, [rbp - 8]                      ; static link
  mov rsi, [r10 - 16]                     ; %arg1:isdigit
  call isdigit                            
  mov qword [rbx], rax                    ; any.any := isdigit(buffer)
test1:                                    
  mov rdi, rbp                            ; %arg(static_link):isdigit
  mov r10, [rbp - 8]                      ; static link
  mov rsi, [r10 - 16]                     ; %arg1:isdigit
  call isdigit                            
  cmp rax, 0                              ; isdigit(buffer)
  je break1                               
body1:                                    
  mov r10, [rbp - 16]                     ; fetch spilled t19
  imul rbx, r10, 10                       
  mov r10, [rbp - 8]                      ; static link
  mov rdi, [r10 - 16]                     ; %arg1:ord
  call ord                                
  add rbx, rax                            
  lea rdi, [rel str__0]                   
  call ord                                
  sub rbx, rax                            
  mov [rbp - 16], rbx                     ; store spilled t19
  mov rbx, [rbp - 8]                      ; static link
  sub rbx, 16                             
  call get_char                           
  mov qword [rbx], rax                    ; buffer := get_char()
  jmp test1                               
break1:                                   
  mov rax, [rbp - 16]                     ; fetch spilled t19
  mov rbx, [rbp - 24]                     ; fetch spilled t53
  mov rsp, rbp
  pop rbp
  ret
skipto:
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov [rbp - 16], rbx                     ; store spilled t36
  mov [rbp - 8], rdi                      ; static link
test:                                     
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 8]                      ; static link
  mov rdi, [r10 - 16]                     ; arg1:stringEqual
  lea rsi, [rel str__]                    
  call stringEqual                        
  mov r10, rax                            ; push call up
  cmp r10, 0                              ; buffer = " "
  je break                                
body:                                     
  mov r10, [rbp - 8]                      ; static link
  mov rbx, [r10 - 8]                      ; static link
  sub rbx, 16                             
  call get_char                           
  mov qword [rbx], rax                    ; buffer := get_char()
  jmp test                                
break:                                    
  mov rbx, [rbp - 16]                     ; fetch spilled t36
  mov rsp, rbp
  pop rbp
  ret
isdigit:
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov [rbp - 24], rbx                     ; store spilled t30
  mov [rbp - 8], rdi                      ; static link
  mov r10, 1                              ; true
  mov [rbp - 16], r10                     ; store spilled t29
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 8]                      ; static link
  mov rdi, [r10 - 16]                     ; %arg1:ord
  call ord                                
  mov rbx, rax                            ; push stmt up
  lea rdi, [rel str__0]                   
  call ord                                
  cmp rbx, rax                            ; ord(buffer) >= ord("0")
  jge if_t                                
if_f:                                     
false:                                    
  mov r10, 0                              ; false
  mov [rbp - 16], r10                     ; store spilled t29
true:                                     
  mov rax, [rbp - 16]                     ; fetch spilled t29
  mov rbx, [rbp - 24]                     ; fetch spilled t30
  jmp done8                               
if_t:                                     
  mov r10, [rbp - 8]                      ; static link
  mov r10, [r10 - 8]                      ; static link
  mov rdi, [r10 - 16]                     ; %arg1:ord
  call ord                                
  mov rbx, rax                            ; push stmt up
  lea rdi, [rel str__9]
  call ord
  cmp rbx, rax
  jle true
false_bridge:              
  jmp false
done8:              
  mov rsp, rbp
  pop rbp
  ret