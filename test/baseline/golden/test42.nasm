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
  mov r15, rbp                            
  sub r15, 88                             
  xor rax, rax                            ; 0
  mov [r15 + 0], rax                      ; arr1[0] = 0
  mov [r15 + 8], rax                      ; arr1[1] = 0
  mov [r15 + 16], rax                     ; arr1[2] = 0
  mov [r15 + 24], rax                     ; arr1[3] = 0
  mov [r15 + 32], rax                     ; arr1[4] = 0
  mov [r15 + 40], rax                     ; arr1[5] = 0
  mov [r15 + 48], rax                     ; arr1[6] = 0
  mov [r15 + 56], rax                     ; arr1[7] = 0
  mov [r15 + 64], rax                     ; arr1[8] = 0
  mov [r15 + 72], rax                     ; arr1[9] = 0
  mov rbx, rbp                            
  sub rbx, 128                            
  mov rax, 4                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  lea rdi, [rel str__aname]               
  mov rcx, 0                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rdi                    ; .name="aname"
  lea rdi, [rel str__somewhere]           
  mov rcx, 1                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rdi                    ; .address="somewhere"
  mov rdi, 0                              
  mov rcx, 2                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rdi                    ; .id=0
  mov rdi, 0                              
  mov rcx, 3                              
  imul rdx, rcx, 8                        
  mov rcx, rax                            
  add rcx, rdx                            
  mov qword [rcx], rdi                    ; .age=0
  mov [rbx + 0], rax                      ; arr2[0] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [rbx + 8], rax                      ; arr2[1] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [rbx + 16], rax                     ; arr2[2] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [rbx + 24], rax                     ; arr2[3] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov [rbx + 32], rax                     ; arr2[4] = rectype1 {
                                          ;   name="aname",
                                          ;   address="somewhere",
                                          ;   id=0,
                                          ;   age=0
                                          ; }
  mov r14, rbp                            
  sub r14, 928                            
  lea rax, [rel str__]                    
  mov [r14 + 0], rax                      ; arr3[0] = ""
  mov [r14 + 8], rax                      ; arr3[1] = ""
  mov [r14 + 16], rax                     ; arr3[2] = ""
  mov [r14 + 24], rax                     ; arr3[3] = ""
  mov [r14 + 32], rax                     ; arr3[4] = ""
  mov [r14 + 40], rax                     ; arr3[5] = ""
  mov [r14 + 48], rax                     ; arr3[6] = ""
  mov [r14 + 56], rax                     ; arr3[7] = ""
  mov [r14 + 64], rax                     ; arr3[8] = ""
  mov [r14 + 72], rax                     ; arr3[9] = ""
  mov [r14 + 80], rax                     ; arr3[10] = ""
  mov [r14 + 88], rax                     ; arr3[11] = ""
  mov [r14 + 96], rax                     ; arr3[12] = ""
  mov [r14 + 104], rax                    ; arr3[13] = ""
  mov [r14 + 112], rax                    ; arr3[14] = ""
  mov [r14 + 120], rax                    ; arr3[15] = ""
  mov [r14 + 128], rax                    ; arr3[16] = ""
  mov [r14 + 136], rax                    ; arr3[17] = ""
  mov [r14 + 144], rax                    ; arr3[18] = ""
  mov [r14 + 152], rax                    ; arr3[19] = ""
  mov [r14 + 160], rax                    ; arr3[20] = ""
  mov [r14 + 168], rax                    ; arr3[21] = ""
  mov [r14 + 176], rax                    ; arr3[22] = ""
  mov [r14 + 184], rax                    ; arr3[23] = ""
  mov [r14 + 192], rax                    ; arr3[24] = ""
  mov [r14 + 200], rax                    ; arr3[25] = ""
  mov [r14 + 208], rax                    ; arr3[26] = ""
  mov [r14 + 216], rax                    ; arr3[27] = ""
  mov [r14 + 224], rax                    ; arr3[28] = ""
  mov [r14 + 232], rax                    ; arr3[29] = ""
  mov [r14 + 240], rax                    ; arr3[30] = ""
  mov [r14 + 248], rax                    ; arr3[31] = ""
  mov [r14 + 256], rax                    ; arr3[32] = ""
  mov [r14 + 264], rax                    ; arr3[33] = ""
  mov [r14 + 272], rax                    ; arr3[34] = ""
  mov [r14 + 280], rax                    ; arr3[35] = ""
  mov [r14 + 288], rax                    ; arr3[36] = ""
  mov [r14 + 296], rax                    ; arr3[37] = ""
  mov [r14 + 304], rax                    ; arr3[38] = ""
  mov [r14 + 312], rax                    ; arr3[39] = ""
  mov [r14 + 320], rax                    ; arr3[40] = ""
  mov [r14 + 328], rax                    ; arr3[41] = ""
  mov [r14 + 336], rax                    ; arr3[42] = ""
  mov [r14 + 344], rax                    ; arr3[43] = ""
  mov [r14 + 352], rax                    ; arr3[44] = ""
  mov [r14 + 360], rax                    ; arr3[45] = ""
  mov [r14 + 368], rax                    ; arr3[46] = ""
  mov [r14 + 376], rax                    ; arr3[47] = ""
  mov [r14 + 384], rax                    ; arr3[48] = ""
  mov [r14 + 392], rax                    ; arr3[49] = ""
  mov [r14 + 400], rax                    ; arr3[50] = ""
  mov [r14 + 408], rax                    ; arr3[51] = ""
  mov [r14 + 416], rax                    ; arr3[52] = ""
  mov [r14 + 424], rax                    ; arr3[53] = ""
  mov [r14 + 432], rax                    ; arr3[54] = ""
  mov [r14 + 440], rax                    ; arr3[55] = ""
  mov [r14 + 448], rax                    ; arr3[56] = ""
  mov [r14 + 456], rax                    ; arr3[57] = ""
  mov [r14 + 464], rax                    ; arr3[58] = ""
  mov [r14 + 472], rax                    ; arr3[59] = ""
  mov [r14 + 480], rax                    ; arr3[60] = ""
  mov [r14 + 488], rax                    ; arr3[61] = ""
  mov [r14 + 496], rax                    ; arr3[62] = ""
  mov [r14 + 504], rax                    ; arr3[63] = ""
  mov [r14 + 512], rax                    ; arr3[64] = ""
  mov [r14 + 520], rax                    ; arr3[65] = ""
  mov [r14 + 528], rax                    ; arr3[66] = ""
  mov [r14 + 536], rax                    ; arr3[67] = ""
  mov [r14 + 544], rax                    ; arr3[68] = ""
  mov [r14 + 552], rax                    ; arr3[69] = ""
  mov [r14 + 560], rax                    ; arr3[70] = ""
  mov [r14 + 568], rax                    ; arr3[71] = ""
  mov [r14 + 576], rax                    ; arr3[72] = ""
  mov [r14 + 584], rax                    ; arr3[73] = ""
  mov [r14 + 592], rax                    ; arr3[74] = ""
  mov [r14 + 600], rax                    ; arr3[75] = ""
  mov [r14 + 608], rax                    ; arr3[76] = ""
  mov [r14 + 616], rax                    ; arr3[77] = ""
  mov [r14 + 624], rax                    ; arr3[78] = ""
  mov [r14 + 632], rax                    ; arr3[79] = ""
  mov [r14 + 640], rax                    ; arr3[80] = ""
  mov [r14 + 648], rax                    ; arr3[81] = ""
  mov [r14 + 656], rax                    ; arr3[82] = ""
  mov [r14 + 664], rax                    ; arr3[83] = ""
  mov [r14 + 672], rax                    ; arr3[84] = ""
  mov [r14 + 680], rax                    ; arr3[85] = ""
  mov [r14 + 688], rax                    ; arr3[86] = ""
  mov [r14 + 696], rax                    ; arr3[87] = ""
  mov [r14 + 704], rax                    ; arr3[88] = ""
  mov [r14 + 712], rax                    ; arr3[89] = ""
  mov [r14 + 720], rax                    ; arr3[90] = ""
  mov [r14 + 728], rax                    ; arr3[91] = ""
  mov [r14 + 736], rax                    ; arr3[92] = ""
  mov [r14 + 744], rax                    ; arr3[93] = ""
  mov [r14 + 752], rax                    ; arr3[94] = ""
  mov [r14 + 760], rax                    ; arr3[95] = ""
  mov [r14 + 768], rax                    ; arr3[96] = ""
  mov [r14 + 776], rax                    ; arr3[97] = ""
  mov [r14 + 784], rax                    ; arr3[98] = ""
  mov [r14 + 792], rax                    ; arr3[99] = ""
  mov rax, 4                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r13, rax                            
  lea rdx, [rel str__Kapoios]             
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, r13                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .name="Kapoios"
  lea rdx, [rel str__Kapou]               
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, r13                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .address="Kapou"
  mov rdx, 2432                           
  mov rax, 2                              
  imul rcx, rax, 8                        
  mov rax, r13                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .id=2432
  mov rdx, 44                             
  mov rax, 3                              
  imul rcx, rax, 8                        
  mov rax, r13                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .age=44
  mov rax, 2                              
  imul rdi, rax, 8                        
  xor rsi, rsi                            ; arg2:initArray
  call initArray                          
  mov r12, rax                            
  lea rdx, [rel str__Allos]               
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, r12                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; .name="Allos"
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, r12                            
  add rax, rcx                            
  mov [rbp - 976], rax                    ; store spilled t47
  mov rax, 3                              
  imul rdi, rax, 8                        
  mov rsi, 1900                           ; arg2:initArray
  call initArray                          
  mov rcx, [rbp - 976]                    ; fetch spilled t47
  mov qword [rcx], rax                    ; .dates=arrtype1[3] of 1900
  mov rdx, 1                              
  mov rax, 0                              
  imul rcx, rax, 8                        
  mov rax, r15                            
  add rax, rcx                            
  mov qword [rax], rdx                    ; arr1[0] := 1
  mov rcx, 3                              
  mov rax, 9                              
  imul rax, rax, 8                        
  add r15, rax                            
  mov qword [r15], rcx                    ; arr1[9] := 3
  lea rsi, [rel str__kati]                
  mov rax, 0                              
  imul rdx, rax, 8                        
  mov rax, 3                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rax, [rax]                          ; arr2[3]
  add rax, rdx                            
  mov qword [rax], rsi                    ; arr2[3].name := "kati"
  mov rsi, 23                             
  mov rax, 3                              
  imul rdx, rax, 8                        
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rax, [rax]                          ; arr2[1]
  add rax, rdx                            
  mov qword [rax], rsi                    ; arr2[1].age := 23
  lea rcx, [rel str__sfd]                 
  mov rax, 34                             
  imul rax, rax, 8                        
  add r14, rax                            
  mov qword [r14], rcx                    ; arr3[34] := "sfd"
  lea rcx, [rel str__sdf]                 
  mov rax, 0                              
  imul rax, rax, 8                        
  add r13, rax                            
  mov qword [r13], rcx                    ; rec1.name := "sdf"
  mov rsi, 2323                           
  mov rax, 0                              
  imul rdx, rax, 8                        
  mov rax, 1                              
  imul rcx, rax, 8                        
  mov rax, r12                            
  add rax, rcx                            
  mov rax, [rax]                          ; rec2.dates
  add rax, rdx                            
  mov qword [rax], rsi                    ; rec2.dates[0] := 2323
  mov rdx, 2323                           
  mov rax, 2                              
  imul rcx, rax, 8                        
  mov rax, 1                              
  imul rax, rax, 8                        
  add r12, rax                            
  mov rax, [r12]                          ; rec2.dates
  add rax, rcx                            
  mov qword [rax], rdx                    ; rec2.dates[2] := 2323
  mov rax, 0                              
  imul rdx, rax, 8                        
  mov rax, 3                              
  imul rcx, rax, 8                        
  mov rax, rbx                            
  add rax, rcx                            
  mov rax, [rax]                          ; arr2[3]
  add rax, rdx                            
  mov rdi, [rax]                          ; %arg1:concat
  lea rsi, [rel str__is]                  
  call concat                             
  mov r12, rax                            ; %arg1:concat
  mov rax, 3                              
  imul rcx, rax, 8                        
  mov rax, 3                              
  imul rax, rax, 8                        
  add rbx, rax                            
  mov rax, [rbx]                          ; arr2[3]
  add rax, rcx                            
  mov rdi, [rax]                          ; %arg1:string_of_int
  call string_of_int                      
  mov rsi, rax                            ; %arg2:concat
  mov rdi, r12                            ; arg1:concat
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