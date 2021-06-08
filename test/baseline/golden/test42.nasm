; syntax:nasm
BITS 64
section .text

extern concat
extern initArray
extern print
extern string_of_int

global _start

str__yearsold:
  dq 11
  db ` years old\n`
str__is:
  dq 4
  db ` is `
str__sdf:
  dq 3
  db `sdf`
str__sfd:
  dq 3
  db `sfd`
str__kati:
  dq 4
  db `kati`
str__Allos:
  dq 5
  db `Allos`
str__Kapou:
  dq 5
  db `Kapou`
str__Kapoios:
  dq 7
  db `Kapoios`
str__:
  dq 0
  db ``
str__somewhere:
  dq 9
  db `somewhere`
str__aname:
  dq 5
  db `aname`
_start:
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov [rbp - 16], rbx                     ; store spilled t35
  mov [rbp - 24], r12                     ; store spilled t36
  mov [rbp - 32], r13                     ; store spilled t37
  mov [rbp - 40], r14                     ; store spilled t38
  mov [rbp - 48], r15                     ; store spilled t39
  mov [rbp - 8], rdi                      ; static link
  mov r10, 10                             
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov rbx, rax                            ; var arr1 := arrtype1[10] of 0
  mov r10, 5                              
  imul r12, r10, 8                        
  mov r10, 4                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov rsi, rax                            
  lea r13, [rel str__aname]               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rsi                            
  add r10, r11                            
  mov qword [r10], r13                    ; .name="aname"
  lea r13, [rel str__somewhere]           
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, rsi                            
  add r10, r11                            
  mov qword [r10], r13                    ; .address="somewhere"
  mov r13, 0                              
  mov r10, 2                              
  imul r11, r10, 8                        
  mov r10, rsi                            
  add r10, r11                            
  mov qword [r10], r13                    ; .id=0
  mov r13, 0                              
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, rsi                            
  add r10, r11                            
  mov qword [r10], r13                    ; .age=0
  mov rdi, r12                            ; arg1:initArray
  call initArray                          
  mov r12, rax                            ; var arr2 := arrtype2[5] of rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov r10, 100                            
  imul rdi, r10, 8                        
  lea rsi, [rel str__]                    
  call initArray                          
  mov r15, rax                            ; var arr3 : arrtype3 := arrtype3[100] of ""
  mov r10, 4                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r14, rax                            
  lea r13, [rel str__Kapoios]             
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov qword [r10], r13                    ; .name="Kapoios"
  lea r13, [rel str__Kapou]               
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov qword [r10], r13                    ; .address="Kapou"
  mov r13, 2432                           
  mov r10, 2                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov qword [r10], r13                    ; .id=2432
  mov r13, 44                             
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, r14                            
  add r10, r11                            
  mov qword [r10], r13                    ; .age=44
  mov r10, 2                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r13, rax                            
  lea r8, [rel str__Allos]                
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, r13                            
  add r10, r11                            
  mov qword [r10], r8                     ; .name="Allos"
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, r13                            
  add r10, r11                            
  mov [rbp - 56], r10                     ; store spilled t41
  mov r10, 3                              
  imul rdi, r10, 8                        
  mov rsi, 1900                           ; arg2:initArray
  call initArray                          
  mov r10, [rbp - 56]                     ; fetch spilled t41
  mov qword [r10], rax                    ; .dates=arrtype1[3] of 1900
  mov r8, 1                               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rbx                            
  add r10, r11                            
  mov qword [r10], r8                     ; arr1[0] := 1
  mov r11, 3                              
  mov r10, 9                              
  imul r10, r10, 8                        
  add rbx, r10                            
  mov qword [rbx], r11                    ; arr1[9] := 3
  lea r9, [rel str__kati]                 
  mov r10, 0                              
  imul r8, r10, 8                         
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, r12                            
  add r10, r11                            
  mov r10, [r10]                          ; arr2[3]
  add r10, r8                             
  mov qword [r10], r9                     ; arr2[3].name := "kati"
  mov r9, 23                              
  mov r10, 3                              
  imul r8, r10, 8                         
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, r12                            
  add r10, r11                            
  mov r10, [r10]                          ; arr2[1]
  add r10, r8                             
  mov qword [r10], r9                     ; arr2[1].age := 23
  lea r11, [rel str__sfd]                 
  mov r10, 34                             
  imul r10, r10, 8                        
  add r15, r10                            
  mov qword [r15], r11                    ; arr3[34] := "sfd"
  lea r11, [rel str__sdf]                 
  mov r10, 0                              
  imul r10, r10, 8                        
  add r14, r10                            
  mov qword [r14], r11                    ; rec1.name := "sdf"
  mov r15, 2323                           
  mov r10, 0                              
  imul r14, r10, 8                        
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, r13                            
  add r10, r11                            
  mov r10, [r10]                          ; rec2.dates
  add r10, r14                            
  mov qword [r10], r15                    ; rec2.dates[0] := 2323
  mov r14, 2323                           
  mov r10, 2                              
  imul r11, r10, 8                        
  mov r10, 1                              
  imul r10, r10, 8                        
  add r13, r10                            
  mov r10, [r13]                          ; rec2.dates
  add r10, r11                            
  mov qword [r10], r14                    ; rec2.dates[2] := 2323
  mov r10, 0                              
  imul r13, r10, 8                        
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, r12                            
  add r10, r11                            
  mov r10, [r10]                          ; arr2[3]
  add r10, r13                            
  mov rdi, [r10]                          ; %arg1:concat
  lea rsi, [rel str__is]                  
  call concat                             
  mov r13, rax                            ; %arg1:concat
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, 3                              
  imul r10, r10, 8                        
  add r12, r10                            
  mov r10, [r12]                          ; arr2[3]
  add r10, r11                            
  mov rdi, [r10]                          ; %arg1:string_of_int
  call string_of_int                      
  mov rsi, rax                            ; %arg2:concat
  mov rdi, r13                            ; arg1:concat
  call concat                             
  mov rdi, rax                            ; %arg1:print
  call print                              
  lea rdi, [rel str__yearsold]            
  call print                              
  xor rax, rax                            ; return ()
  mov rbx, [rbp - 16]                     ; fetch spilled t35
  mov r12, [rbp - 24]                     ; fetch spilled t36
  mov r13, [rbp - 32]                     ; fetch spilled t37
  mov r14, [rbp - 40]                     ; fetch spilled t38
  mov r15, [rbp - 48]                     ; fetch spilled t39
  mov rsp, rbp
  pop rbp
  ret