global so_emul

%ifndef CORES
%define CORES 4
%endif

section .bss

; [rbp] - current instruction
; r10b - arg1
; r11b - arg2
; bl - imm8

state:          resq CORES

section .text

; modifies rbp, r13, r11, r0 and rbx
; saves value of arg1 in r10b, arg2 in r1bb and imm8 in bl
; if current instruction doesn't use some or any of these arguments
; they simply won't be looked at
decode_parameters:
                                ; [rbp] will mean effectively [rdi + PC], that is the current instruction
    mov bpl, [r14 + 4]          ; rbp = PC
    dec bpl                     ; becasue we increment it earlier for the next instruction
    movzx rbp, bpl
    shl rbp, 1                  ; rbp = 2 * PC
    add rbp, rdi                ; rbp = rdi + 2 * PC

    mov r13w, word [rbp]        ; get entire instruction value

    mov bl, r13b                ; save imm8 value
    shr r13w, 3                 ; move bitwise to obtain correct bytes to decode
    shr r13b, 5
    mov r10b, r13b              ; save arg1 value
    shr r13w, 3
    shr r13b, 5
    mov r11b, r13b              ; save arg2 value
    ret


; modifies r13 and r9
; sets r13b as value of memory address pointed by arg2
; [r14 + r9] is also the value in memory
set_arg2_to_memory_address:
    cmp r13b, 4                     ; 4 means access X
    jne .check_for_5
    mov r13b, [r14 + 2]             ; get X from state
    movzx r9, r13b                  ; r9 is now an address, rsi + r9 means [X]
    mov r13b, [rsi + r9]            ; save value under r13b
    jmp .finish
.check_for_5:
    cmp r13b, 5                     ; 5 means access Y
    jne .check_for_6
    mov r13b, [r14 + 3]             ; get Y from state
    movzx r9, r13b                  ; r9 is now an address, rsi + r9 means [Y]
    mov r13b, [rsi + r9]            ; save value under r13b
    jmp .finish
.check_for_6:
    cmp r13b, 6                     ; 6 means access X + D
    jne .check_for_7
    mov r13b, [r14 + 2]             ; get X from state
    add r13b, [r14 + 1]             ; add D, sum is X + D
    movzx r9, r13b                  ; r9 is now an address, rsi + r9 means [X + D]
    mov r13b, [rsi + r9]            ; save value under r13b
    jmp .finish
.check_for_7:
                                    ; 7 means access Y + D
    mov r13b, [r14 + 3]             ; get Y from state
    add r13b, [r14 + 1]             ; add D, sum is Y + D
    movzx r9, r13b                  ; r9 is now an address, rsi + r9 means [Y + D]
    mov r13b, [rsi + r9]            ; save value under r13b
.finish:
    ret

; modifies r9 and r13
; sets r13b as value pointed to by arg2
; [r14 + r9] is now a value of register specified by arg2
set_arg2_to_register_value:
    movzx r9, r13b                  ; get arg2 value as 64-bits
    mov r13b, [r14 + r9]            ; set value of arg2 with correct register
    ret


; calls correct function to set r11b according to arg2, 
; either as a register or a memory address
set_arg2:
    cmp r13b, 3                     ; check arg2
    jle .arg2_is_a_register         ; arg2 is a register 
    call set_arg2_to_memory_address ; arg2 is a memory address, set r13b accordingly
    ret
.arg2_is_a_register:
    call set_arg2_to_register_value ; arg2 is a register
    ret


; modifies r15 and r9
; sets [rsi + r9] as correct value that decoded arg2 points to
set_arg_1_to_memory_address:
    cmp r15b, 4                     ; 4 means access X
    jne .check_for_5
    mov r15b, [r14 + 2]             ; get X from state
    movzx r9, r15b                  ; r9 is now an address, rsi + r9 means [X]
    jmp .finish
.check_for_5:
    cmp r15b, 5                     ; 5 means access Y
    jne .check_for_6
    mov r15b, [r14 + 3]             ; get Y from state
    movzx r9, r15b                  ; r9 is now an address, rsi + r9 means [Y]
    jmp .finish
.check_for_6:
    cmp r15b, 6                     ; 6 means access X + D
    jne .check_for_7
    mov r15b, [r14 + 2]             ; get X from state
    add r15b, [r14 + 1]             ; add D, sum is X + D
    movzx r9, r15b                  ; r9 is now an address, rsi + r9 means [X + D]
    jmp .finish
.check_for_7:
                                    ; 7 means access Y + D
    mov r15b, [r14 + 3]             ; get Y from state
    add r15b, [r14 + 1]             ; add D, sum is Y + D
    movzx r9, r15b                  ; r9 is now an address, rsi + r9 means [Y + D]
.finish:
    ret  

; modifies al and [r14 + 7]
; sets Z register according to al, which is a value of latest operation
set_Z_register:
    cmp al, 0
    jne .ZF_is_not_zero
    inc al                          ; al = 1
    mov [r14 + 7], al
    ret
.ZF_is_not_zero:
    mov al, 0
    mov [r14 + 7], al
    ret

; modifies r15, r13, rax, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is or [rsi + r9], r13b
; if arg1 is a register, then the result is or [r14 + r15], r13b
; sets Z register
OR:
    mov r15b, r10b
    mov r13b, r11b
    call set_arg2                   ; sets r13b to a correct arg2 value
    cmp r15b, 3
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address
    or [rsi + r9], r13b
    mov al, [rsi + r9]              ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movzx r15, r15b
    or [r14 + r15], r13b
    mov al, [r14 + r15]             ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, r12, r9, [rsi] and [r14]
; if arg1 points to memory and arg2 is a register
; then this instruction is atomic
; otherwise it's not
; swaps the values of memory or registers pointed to by arg1 and arg2
XCHG:
    mov r15b, r10b
    mov r13b, r11b

    cmp r13b, 3                     ; check if arg2 is a register
    jle .may_be_atomic_arg2_reg
                                    ; arg2 is a memory address
.cant_be_atomic_arg2_mem:
    cmp r15b, 3                     ; check if arg1 is a register
    ja .is_not_atomic_arg1_mem_arg2_mem
                                    ; arg1 is a register
                                    ; arg2 is a memory address
    jmp .is_not_atomic_arg1_reg_arg2_mem


.may_be_atomic_arg2_reg:
                                    ; now check if arg1 is a memory address
    cmp r15b, 3                     ; check if arg1 is a memory address
    ja .is_atomic_arg1_mem_arg2_reg ; is > 3, so it's a memory address
    
                                    ; arg1 is a register
                                    ; arg2 is a register
                                    ; xchg is not atomic
.is_not_atomic_arg1_reg_arg2_reg:
                                    ; swap([r14 + r15], [r14 + r13])
    movzx r15, r15b
    movzx r13, r13b
                                    ; instruction is known, r12 can be overwritten
    mov r12b, byte [r14 + r15]
                                    ; r9 is not used, can be overwritten
    mov r9b, byte [r14 + r13]
    mov byte [r14 + r15], r9b
    mov byte [r14 + r13], r12b
    jmp decode_and_perform_instruction.finish

.is_not_atomic_arg1_reg_arg2_mem:
    movzx r15, r15b                 ; [r14 + r15] is arg1 value, a register
                                    ; now make [rsi + r9] be a value of arg2
    call set_arg2_to_memory_address
                                    ; swap([r14 + r15], [rsi + r9])
                                    ; r12 and r13 can now be overwritten
                                    ; as instructions and parameters are set
    mov r12b, byte [r14 + r15]
                                    ; r13b already contains value of [rsi + r9]
                                    ; from set_arg2_to_memory_address
    mov byte [r14 + r15], r13b
    mov byte [rsi + r9], r12b
    jmp decode_and_perform_instruction.finish
.is_not_atomic_arg1_mem_arg2_mem:
    call set_arg_1_to_memory_address
                                    ; [rsi + r9] points to correct arg1 address
    mov r12, r9                     ; set r12 as address in memory array
                                    ; now [rsi + r12] points to arg1 address             
                                    ; now make [rsi + r9] point to arg2 address
    call set_arg2_to_memory_address ; [rsi + r9] points to arg2 address
                                    ; swap([rsi + r9], [rsi + r12])
                                    ; r13 and r15 can now be overwritten
                                    ; as instructions and parameters are set
    mov r15b, byte [rsi + r12]
                                    ; r13b already contains value of [rsi + r9]
                                    ; from set_arg2_to_memory_address
    mov byte [rsi + r12], r13b
    mov byte [rsi + r9], r15b
    jmp decode_and_perform_instruction.finish
.is_atomic_arg1_mem_arg2_reg:
    movzx r13, r13b
                                    ; [r14 + r13] is arg2's value in register
                                    ; r12 can now be overwritten
    mov r12b, [r14 + r13]           ; arg2 value 

    call set_arg_1_to_memory_address
                                    ; [rsi + r9] is arg1's memory address value
    xchg byte [rsi + r9], r12b
    mov byte [r14 + r13], r12b      ; [r14 + r13] := [rsi + r9]
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, rax, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then [rsi + r9] += r13b
; if arg1 is a register, then [r14 + r15] += r13b
; sets Z register
ADD:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address
                                    ; rsi + r9 is a correct address to write onto
    add [rsi + r9], r13b
    mov al, [rsi + r9]
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    add [r14 + r15], r13b
    mov al, [r14 + r15]
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r13, r15, rax, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is [rsi + r9] += (r13b + [r14 + 6])
; if arg1 is a register, then the result is [r14 + r15] += (r13b + [r14 + 6])
; sets C and Z registers
ADC:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    mov al, [r14 + 6]               ; get C value
    mov [r14 + 6], byte 0           ; reset C register

    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address 
                                    ; rsi + r9 is a correct address to write onto
    add [rsi + r9], r13b
    adc [r14 + 6], byte 0           ; if CF is set, set C register               
    add [rsi + r9], al              ; add C value
    adc [r14 + 6], byte 0           ; if CF is set, set C register.
    
    mov al, [rsi + r9]              ; to set Z 
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    add [r14 + r15], r13b
    adc [r14 + 6], byte 0           ; if CF is set, set C register
    add [r14 + r15], al
    adc [r14 + 6], byte 0           ; if CF is set, set C register

    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9], [r14 + r15] and rax
; if arg1 is a memory address, then the result is [rsi + r9] -= r13b
; if arg1 is a register, then the result is [r14 + r15] -= r13b
; sets Z register
SUB:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2                   
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address 
                                    ; rsi + r9 is a correct address to write onto
    sub [rsi + r9], r13b
    mov al, [rsi + r9]              ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    sub [r14 + r15], r13b
    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r13, r15, rax, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is [rsi + r9] -= (r13b + [r14 + 6])
; if arg1 is a register, then the result is [r14 + r15] -= (r13b + [r14 + 6])
; sets C and Z registers
SBB:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2        
    mov al, [r14 + 6]               ; save C value
    mov [r14 + 6], byte 0           ; reset C register

    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address
                                    ; rsi + r9 is a correct address to write onto
    sub [rsi + r9], r13b            
    adc [r14 + 6], byte 0           ; if CF is set, set C register
    sub [rsi + r9], al
    adc [r14 + 6], byte 0           ; if CF is set, set C register

    mov al, [rsi + r9]              ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    sub [r14 + r15], r13b
    adc [r14 + 6], byte 0           ; if CF is set, set C register
    sub [r14 + r15], al
    adc [r14 + 6], byte 0           ; if CF is set, set C register

    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is [rsi + r9] = r13b
; if arg1 is a register, then the result is [r14 + r15] = r13b
MOV:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2
    cmp r15b, 3
    jle .arg1_is_a_register

    call set_arg_1_to_memory_address
    mov [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    mov [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is [rsi + r9] = r13b
; if arg1 is a register, then the result is [r14 + r15] = r13b
; where r13b is an immediate
MOVI:
    mov r15b, r10b
    mov r13b, bl                    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register

    call set_arg_1_to_memory_address
    mov [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    mov [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish


; modifies rdx
; stops all operations as a consequence
BRK:
    mov rdx, 1
    jmp decode_and_perform_instruction.finish


; modifies [r14 + 6]
; sets C register to 0
CLC: 
    mov [r14 + 6], byte 0
    jmp decode_and_perform_instruction.finish


; modifies [r14 + 6]
; sets C register to 1
STC:
    mov [r14 + 6], byte 1
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is or [rsi + r9],  r13b
; if arg1 is a register, then the result is or [r14 + r15], r13b
; where r13b is an immediate
; modifies Z register
XORI:
    mov r15b, r10b
    mov r13b, bl                    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register

    call set_arg_1_to_memory_address
    xor [rsi + r9], r13b
    mov al, [rsi + r9]              ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    xor [r14 + r15], r13b
    mov al, [r14 + r15]             ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is [rsi + r9] += (r13b + [r14 + 6])
; if arg1 is a register, then the result is [r14 + r15] += (r13b + [r14 + 6])
; where r13b is an immediate
; modifies Z register
ADDI:
    mov r15b, r10b
    mov r13b, bl                        ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                                        ; arg1 is a memory address
    call set_arg_1_to_memory_address
    add [rsi + r9], r13b                ; add imm8 to given memory address
    mov al, [rsi + r9]                  ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movzx r15, r15b
    add [r14 + r15], r13b               ; add imm8 to given register
    mov al, [r14 + r15]                 ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is cmp [rsi + r9], r13b
; if arg1 is a register, then the result is cmp [r14 + r15], r13b
; modifies C and Z registers
CMPI:
    mov r15b, r10b
    mov r13b, bl                        ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                                        ; arg1 is a memory address
    call set_arg_1_to_memory_address
    mov [r14 + 7], byte 0               ; Z register's value will be now set to 1 if comparison resulted in 0
    mov [r14 + 6], byte 0               ; C register can now be forgotten and will be set now

    cmp [rsi + r9], r13b
    jnz .set_C
    inc byte [r14 + 7]                  ; will set Z to 1 if ZF is set, stay 0 otherwise
.arg1_is_a_register:
    movzx r15, r15b
    mov [r14 + 7], byte 0               ; Z register's value will be now set to 1 if comparison resultet in 0
    mov [r14 + 6], byte 0               ; C register can now be forgotten and will be set now
    cmp [r14 + r15], r13b
    jnz .set_C
                                        ; result is 0, set Z register
    inc byte [r14 + 7]                  ; set Z to 1
.set_C:
                                        ; Z is set already, now set C correctly 
    adc [r14 + 6], byte 0               ; will set C to 1 if CF is set, stay 0 otherwise
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [r14 + r15] and [rsi + r9]
; if arg1 is a memory address, then the result is rcr [rsi + r9], 1
; if arg1 is a register, then the results is rcr [r14 + r15], 1
; modifies C register
RCR:
    mov r15b, r10b                      ; arg1 is the value that will be rotated alongside C register
    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address

    xor r13b, r13b                      ; r13b = 0
    cmp [r14 + 6], byte 1               ; check if C register is set
    jne .CF_done_arg1_is_a_memory_address

    inc r13b                            ; r13b = 1, representing that CF flag is set
.CF_done_arg1_is_a_memory_address:
                                        ; cf set <=> (r13b == 1)
    cmp r13b, 1
    jne .CF_is_not_set_arg1_memory
    
    stc 
    jmp .finish_arg1_memory
.CF_is_not_set_arg1_memory:
    clc
    jmp .finish_arg1_memory
.finish_arg1_memory:
                                        ; CF value is set correctly
    mov [r14 + 6], byte 0               ; old value of C register can be forgotten, will be set quickly
                                        ; CF is ready, now perform rcr      
    rcr byte [rsi + r9], 1
                                        ; r14 + r15 is a register correctly rotated with CF
    setc [r14 + 6]                      ; set C register if CF is set after rcr instruction
    ret
.arg1_is_a_register:
    movzx r15, r15b
    xor r13b, r13b                      ; r13b = 0
    cmp [r14 + 6], byte 1               ; check if C register is set
    jne .CF_done_arg1_is_a_register
                                        ; C register is set, set CF flag as well
    inc r13b                            ; r13b = 1, representing that CF flag is set
.CF_done_arg1_is_a_register:
                                        ; CF set <=> (r13b == 1)
    cmp r13b, 1
    jne .CF_is_not_set
                                        ; CF is set
    stc
    jmp .finish_arg1_register
.CF_is_not_set:
    clc
.finish_arg1_register:
                                        ; CF value is set correctly
    mov [r14 + 6], byte 0               ; old value of C register can be forgotten, will be set quickly
                                        ; CF is ready, now perform rcr

    rcr byte [r14 + r15], 1             ; rotate with carry
                                        ; r14 + r15 is a register correctly rotated with CF
    setc [r14 + 6]                      ; set C register if CF is set after rcr instruction
    ret

; modifies r15, r13 and [r14 + 4]
; makes an unconditional jump
; ([r14 + 4] += r13b) mod 256
; where r13b is an immediate 
JMP:
                                        ; since instruction is known, r12 can be overwritten  
    mov r15b, [r14 + 4]                 ; save old PC value
    mov r13b, bl                        ; get jump value
                                        ; if PC = 13 and jump_value = -7 so actually 249
                                        ; then PC = 262, but overflows and actually contains 6
    add [r14 + 4], r13b
    jmp decode_and_perform_instruction.finish


; makes a conditional jump if Z register is set to 1
JZ:
    cmp [r14 + 7], byte 1
    jne decode_and_perform_instruction.finish
                                        ; Z is set
    jmp JMP


; makes a conditional jump if Z register is set to 0
JNZ:
    cmp [r14 + 7], byte 0
    jne decode_and_perform_instruction.finish
                                        ; Z is not set
    jmp JMP           

; makes a conditional jump if C register is set to 1
JC:
    cmp [r14 + 6], byte 1
    jne decode_and_perform_instruction.finish
                                        ; C is set
    jmp JMP


; makes a conditional jump if C register is set to 0
JNC:
    cmp [r14 + 6], byte 0
    jne decode_and_perform_instruction.finish
                                        ; C is not set
    jmp JMP


; modifies r12
; calls a decoded instructions
decode_and_perform_instruction:
    mov r12w, word [rbp]                ; get the entire instruction value
    shr r12w, 14                        ; shift to get oldest byte
    cmp word r12w, 0                    ; check if it starts with 0
    jne .other_instruction              ; instruction does not use arg1 and arg2
.classic_instruction:
    mov r12b, byte [rbp]
    cmp r12b, 4                         ; 4 means ADD
    je ADD
    cmp r12b, 5                         ; 5 means SUB
    je SUB
    cmp r12b, 0                         ; 0 means MOV
    je MOV
    cmp r12b, 2                         ; 2 means OR
    je OR
    cmp r12b, 6                         ; 6 means ADC
    je ADC
    cmp r12b, 7                         ; 7 means SBB
    je SBB
    cmp r12b, 8                         ; 8 means XCHG
    je XCHG
    jmp .finish                         ; incorrect instruction
.other_instruction:
    mov r12w, word [rbp]
    shr r12w, 12                        ; get oldest byte
    cmp r12w, 4                         ; 4 means MOVI
    je MOVI
    cmp r12w, 15                        ; 15 means BRK
    je BRK
    cmp r12w, 5                         ; 5 means XORI
    je XORI
    cmp r12w, 7                         ; 7 means RCR
    je RCR
    cmp r12w, 6                         ; 6 means either ADDI or CMPI                         
    je .ADDI_OR_CMPI
    cmp r12w, 8                         ; 8 means either CLC or STC
    je .CLC_OR_STC
    cmp r12w, 12                        ; 12 means a kind of jump instruction 
    je .jump_instruction
    jmp .finish                         ; incorrect instruction
.ADDI_OR_CMPI:
    cmp [rbp], word 0x6800
    jl ADDI                             ; instruction value is below 0x6800, so it must be ADDI
    jmp CMPI                            ; instruction is 0x6XXX and is greater or equal to 0x6800, must be CMPI
.CLC_OR_STC:
    cmp [rbp], word 0x8000
    je CLC                              ; instruction is CLC    
    jmp STC                             ; instruction is 0x8XXX and is not 0x8000, so it must be STC
.jump_instruction:
    cmp [rbp], word 0xC500              ; compare with minimal value of JZ instruction
    jnb JZ
    cmp [rbp], word 0xC400              ; compare with minimal value of JNZ instruction
    jnb JNZ
    cmp [rbp], word 0xC300              ; compare with minimal value of JC instruction
    jnb JC
    cmp [rbp], word 0xC200              ; compare with minimal value of JNC instruction
    jnb JNC
                                        ; it's a jump instruction, bute none of the above
                                        ; so it must be simply a JMP instruction
    jmp JMP
.finish:
    ret


; rdi - uint16_t const *code, instructions to perform
; rsi - uint8_t *data, 256 bytes of memory
; rdx - size_t steps, how many instructions to perform
; rcx - size_t cores, for concurrent calculations
; performs 'steps' number of instructions or less, if BRK instruction is called
so_emul:
    push r12
    push r13
    push r14
    push r15
    push rbp
    push rbx

    lea r14, [rel state]                ; get the state variable
    lea r14, [r14 + rcx * 8]            ; get this core's state
    cmp rdx, 0                          ; check if 'steps' count is 0
    je .finish                          ; finish if there are no instructions to perform
.instructions_loop:
    inc byte [r14 + 4]       
    call decode_parameters              
    call decode_and_perform_instruction
    dec rdx
    cmp rdx, 0
    ja .instructions_loop
.finish:
    mov rax, [r14]                      ; return this core's state
    pop rbx
    pop rbp
    pop r15
    pop r14
    pop r13
    pop r12
    ret
