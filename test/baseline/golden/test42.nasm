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
  sub rsp, 976
  mov [rbp - 936], rbx                    ; store spilled t41
  mov [rbp - 944], r12                    ; store spilled t42
  mov [rbp - 952], r13                    ; store spilled t43
  mov [rbp - 960], r14                    ; store spilled t44
  mov [rbp - 968], r15                    ; store spilled t45
  mov [rbp - 8], rdi                      ; static link
  mov rbx, rbp                            
  sub rbx, 88                             
  xor r10, r10                            ; 0
  mov [rbx + 0], r10                      ; arr1[0] = 0
  mov [rbx + 8], r10                      ; arr1[1] = 0
  mov [rbx + 16], r10                     ; arr1[2] = 0
  mov [rbx + 24], r10                     ; arr1[3] = 0
  mov [rbx + 32], r10                     ; arr1[4] = 0
  mov [rbx + 40], r10                     ; arr1[5] = 0
  mov [rbx + 48], r10                     ; arr1[6] = 0
  mov [rbx + 56], r10                     ; arr1[7] = 0
  mov [rbx + 64], r10                     ; arr1[8] = 0
  mov [rbx + 72], r10                     ; arr1[9] = 0
  mov r12, rbp                            
  sub r12, 128                            
  mov r10, 4                              
  imul rdi, r10, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  lea r13, [rel str__aname]               
  mov r10, 0                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r13                    ; .name="aname"
  lea r13, [rel str__somewhere]           
  mov r10, 1                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r13                    ; .address="somewhere"
  mov r13, 0                              
  mov r10, 2                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r13                    ; .id=0
  mov r13, 0                              
  mov r10, 3                              
  imul r11, r10, 8                        
  mov r10, rax                            
  add r10, r11                            
  mov qword [r10], r13                    ; .age=0
  mov [r12 + 0], rax                      ; arr2[0] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [r12 + 8], rax                      ; arr2[1] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [r12 + 16], rax                     ; arr2[2] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [r12 + 24], rax                     ; arr2[3] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [r12 + 32], rax                     ; arr2[4] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov r15, rbp                            
  sub r15, 928                            
  lea r10, [rel str__]                    
  mov [r15 + 0], r10                      ; arr3[0] = ""
  mov [r15 + 8], r10                      ; arr3[1] = ""
  mov [r15 + 16], r10                     ; arr3[2] = ""
  mov [r15 + 24], r10                     ; arr3[3] = ""
  mov [r15 + 32], r10                     ; arr3[4] = ""
  mov [r15 + 40], r10                     ; arr3[5] = ""
  mov [r15 + 48], r10                     ; arr3[6] = ""
  mov [r15 + 56], r10                     ; arr3[7] = ""
  mov [r15 + 64], r10                     ; arr3[8] = ""
  mov [r15 + 72], r10                     ; arr3[9] = ""
  mov [r15 + 80], r10                     ; arr3[10] = ""
  mov [r15 + 88], r10                     ; arr3[11] = ""
  mov [r15 + 96], r10                     ; arr3[12] = ""
  mov [r15 + 104], r10                    ; arr3[13] = ""
  mov [r15 + 112], r10                    ; arr3[14] = ""
  mov [r15 + 120], r10                    ; arr3[15] = ""
  mov [r15 + 128], r10                    ; arr3[16] = ""
  mov [r15 + 136], r10                    ; arr3[17] = ""
  mov [r15 + 144], r10                    ; arr3[18] = ""
  mov [r15 + 152], r10                    ; arr3[19] = ""
  mov [r15 + 160], r10                    ; arr3[20] = ""
  mov [r15 + 168], r10                    ; arr3[21] = ""
  mov [r15 + 176], r10                    ; arr3[22] = ""
  mov [r15 + 184], r10                    ; arr3[23] = ""
  mov [r15 + 192], r10                    ; arr3[24] = ""
  mov [r15 + 200], r10                    ; arr3[25] = ""
  mov [r15 + 208], r10                    ; arr3[26] = ""
  mov [r15 + 216], r10                    ; arr3[27] = ""
  mov [r15 + 224], r10                    ; arr3[28] = ""
  mov [r15 + 232], r10                    ; arr3[29] = ""
  mov [r15 + 240], r10                    ; arr3[30] = ""
  mov [r15 + 248], r10                    ; arr3[31] = ""
  mov [r15 + 256], r10                    ; arr3[32] = ""
  mov [r15 + 264], r10                    ; arr3[33] = ""
  mov [r15 + 272], r10                    ; arr3[34] = ""
  mov [r15 + 280], r10                    ; arr3[35] = ""
  mov [r15 + 288], r10                    ; arr3[36] = ""
  mov [r15 + 296], r10                    ; arr3[37] = ""
  mov [r15 + 304], r10                    ; arr3[38] = ""
  mov [r15 + 312], r10                    ; arr3[39] = ""
  mov [r15 + 320], r10                    ; arr3[40] = ""
  mov [r15 + 328], r10                    ; arr3[41] = ""
  mov [r15 + 336], r10                    ; arr3[42] = ""
  mov [r15 + 344], r10                    ; arr3[43] = ""
  mov [r15 + 352], r10                    ; arr3[44] = ""
  mov [r15 + 360], r10                    ; arr3[45] = ""
  mov [r15 + 368], r10                    ; arr3[46] = ""
  mov [r15 + 376], r10                    ; arr3[47] = ""
  mov [r15 + 384], r10                    ; arr3[48] = ""
  mov [r15 + 392], r10                    ; arr3[49] = ""
  mov [r15 + 400], r10                    ; arr3[50] = ""
  mov [r15 + 408], r10                    ; arr3[51] = ""
  mov [r15 + 416], r10                    ; arr3[52] = ""
  mov [r15 + 424], r10                    ; arr3[53] = ""
  mov [r15 + 432], r10                    ; arr3[54] = ""
  mov [r15 + 440], r10                    ; arr3[55] = ""
  mov [r15 + 448], r10                    ; arr3[56] = ""
  mov [r15 + 456], r10                    ; arr3[57] = ""
  mov [r15 + 464], r10                    ; arr3[58] = ""
  mov [r15 + 472], r10                    ; arr3[59] = ""
  mov [r15 + 480], r10                    ; arr3[60] = ""
  mov [r15 + 488], r10                    ; arr3[61] = ""
  mov [r15 + 496], r10                    ; arr3[62] = ""
  mov [r15 + 504], r10                    ; arr3[63] = ""
  mov [r15 + 512], r10                    ; arr3[64] = ""
  mov [r15 + 520], r10                    ; arr3[65] = ""
  mov [r15 + 528], r10                    ; arr3[66] = ""
  mov [r15 + 536], r10                    ; arr3[67] = ""
  mov [r15 + 544], r10                    ; arr3[68] = ""
  mov [r15 + 552], r10                    ; arr3[69] = ""
  mov [r15 + 560], r10                    ; arr3[70] = ""
  mov [r15 + 568], r10                    ; arr3[71] = ""
  mov [r15 + 576], r10                    ; arr3[72] = ""
  mov [r15 + 584], r10                    ; arr3[73] = ""
  mov [r15 + 592], r10                    ; arr3[74] = ""
  mov [r15 + 600], r10                    ; arr3[75] = ""
  mov [r15 + 608], r10                    ; arr3[76] = ""
  mov [r15 + 616], r10                    ; arr3[77] = ""
  mov [r15 + 624], r10                    ; arr3[78] = ""
  mov [r15 + 632], r10                    ; arr3[79] = ""
  mov [r15 + 640], r10                    ; arr3[80] = ""
  mov [r15 + 648], r10                    ; arr3[81] = ""
  mov [r15 + 656], r10                    ; arr3[82] = ""
  mov [r15 + 664], r10                    ; arr3[83] = ""
  mov [r15 + 672], r10                    ; arr3[84] = ""
  mov [r15 + 680], r10                    ; arr3[85] = ""
  mov [r15 + 688], r10                    ; arr3[86] = ""
  mov [r15 + 696], r10                    ; arr3[87] = ""
  mov [r15 + 704], r10                    ; arr3[88] = ""
  mov [r15 + 712], r10                    ; arr3[89] = ""
  mov [r15 + 720], r10                    ; arr3[90] = ""
  mov [r15 + 728], r10                    ; arr3[91] = ""
  mov [r15 + 736], r10                    ; arr3[92] = ""
  mov [r15 + 744], r10                    ; arr3[93] = ""
  mov [r15 + 752], r10                    ; arr3[94] = ""
  mov [r15 + 760], r10                    ; arr3[95] = ""
  mov [r15 + 768], r10                    ; arr3[96] = ""
  mov [r15 + 776], r10                    ; arr3[97] = ""
  mov [r15 + 784], r10                    ; arr3[98] = ""
  mov [r15 + 792], r10                    ; arr3[99] = ""
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
  mov [rbp - 976], r10                    ; store spilled t47
  mov r10, 3                              
  imul rdi, r10, 8                        
  mov rsi, 1900                           ; arg2:initArray
  call initArray                          
  mov r10, [rbp - 976]                    ; fetch spilled t47
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
  mov rbx, [rbp - 936]                    ; fetch spilled t41
  mov r12, [rbp - 944]                    ; fetch spilled t42
  mov r13, [rbp - 952]                    ; fetch spilled t43
  mov r14, [rbp - 960]                    ; fetch spilled t44
  mov r15, [rbp - 968]                    ; fetch spilled t45
  mov rsp, rbp
  pop rbp
  ret