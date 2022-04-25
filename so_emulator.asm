global so_emul

section .bss

; [rbp] - current instruction
; r10b - arg1
; r11b - arg2
; bl - imm8

state:          resq CORES              ; 8 bytes for each core, 1 byte for each register + 1 unused

section .text

; modifies rbp, r13, r11, r0 and rbx
; saves value of arg1 in r10b, arg2 in r1bb and imm8 in bl
; if current instruction doesn't use some or any of these arguments
; they simply won't be looked at
decode_parameters:
                                        ; [rbp] will mean effectively [rdi + PC], the current instruction
    mov bpl, byte [r14 + 4]             ; rbp = PC
    dec bpl                             ; becasue we increment it earlier for the next instruction
    movzx rbp, bpl
    shl rbp, 1                          ; rbp = 2 * PC
    add rbp, rdi                        ; rbp = rdi + 2 * PC

    mov r13w, word [rbp]                ; get entire instruction value

    mov bl, r13b                        ; save imm8 value
    shr r13w, 3                         ; move bitwise to obtain correct bytes to decode
    shr r13b, 5
    mov r10b, r13b                      ; save arg1 value
    shr r13w, 3
    shr r13b, 5
    mov r11b, r13b                      ; save arg2 value
    ret


; modifies r13 and r9
; sets r13b as value of memory address pointed by arg2
; [r14 + r9] is also the value in memory
set_arg2_to_memory_address:
    cmp r13b, 4                         ; 4 means access X
    jne .check_for_5
    mov r13b, byte [r14 + 2]            ; get X from state
    jmp .finish
.check_for_5:
    cmp r13b, 5                         ; 5 means access Y
    jne .check_for_6
    mov r13b, byte [r14 + 3]            ; get Y from state
    jmp .finish
.check_for_6:
    cmp r13b, 6                         ; 6 means access X + D
    jne .check_for_7
    mov r13b, byte [r14 + 2]            ; get X from state
    add r13b, byte [r14 + 1]            ; add D, sum is X + D
    jmp .finish
.check_for_7:
                                        ; 7 means access Y + D
    mov r13b, byte [r14 + 3]            ; get Y from state
    add r13b, byte [r14 + 1]            ; add D, sum is Y + D
.finish:
    movzx r9, r13b
    mov r13b, byte [rsi + r9]           ; save value under r13b
    ret


; modifies r9 and r13
; sets r11b according to arg2, 
; either as a register or a memory address
set_arg2:
    cmp r13b, 3                         ; check arg2
    jbe .arg2_is_a_register             ; arg2 is a register 
    call set_arg2_to_memory_address     ; arg2 is a memory address, set r13b accordingly
    ret
.arg2_is_a_register:
    movzx r9, r13b                      ; get arg2 value as 64-bits
    mov r13b, byte [r14 + r9]           ; set value of arg2 with correct register
    ret


; modifies r15 and r9
; sets [rsi + r9] as correct value that decoded arg2 points to
set_arg_1_to_memory_address:
    cmp r15b, 4                         ; 4 means access X
    jne .check_for_5
    mov r15b, byte [r14 + 2]            ; get X from state
    jmp .finish
.check_for_5:
    cmp r15b, 5                         ; 5 means access Y
    jne .check_for_6
    mov r15b, byte [r14 + 3]            ; get Y from state
    jmp .finish
.check_for_6:
    cmp r15b, 6                         ; 6 means access X + D
    jne .check_for_7
    mov r15b, byte [r14 + 2]            ; get X from state
    add r15b, byte [r14 + 1]            ; add D, sum is X + D
    jmp .finish
.check_for_7:
                                        ; 7 means access Y + D
    mov r15b, byte [r14 + 3]            ; get Y from state
    add r15b, byte [r14 + 1]            ; add D, sum is Y + D
.finish:
    movzx r9, r15b                      ; now rsi + r9 points to arg1's value
    ret


; modifies rcx
; after this function is called, rcx points to arg1's value
; meaning [rcx] is either a value in memory, or value of a register
get_pointer_to_arg1:
    cmp r15b, 3                         ; check if arg1 means a register
    jbe .arg1_is_a_register
    
    call set_arg_1_to_memory_address
    mov rcx, rsi
    add rcx, r9
                                        ; [rcx] = [rsi + r9]
    ret
.arg1_is_a_register:
    movzx r15, r15b
    mov rcx, r14
    add rcx, r15                        ; [rcx] = [r14 + r15]
    ret


; modifies r15 and r13
; sets r13b as arg2's value
; calls get_pointer_to_arg1 to make [rcx] be arg1's value
prepare_arg1_arg2:
    mov r15b, r10b
    mov r13b, r11b
    call get_pointer_to_arg1
    call set_arg2                       ; sets r13b to a correct arg2 value
    ret


; modifies r15 and r13
; sets r13b as imm8's value
; calls get_pointer_to_arg1 to make [rcx] be arg1's value
prepare_arg1_imm8:
    mov r15b, r10b
    mov r13b, bl                        ; r13b is an immediate
    call get_pointer_to_arg1
    ret  

; modifies [r14 + 7]
; sets Z register according to [rcx], which is a value of latest operation
set_Z_register_and_finish:
    mov byte [r14 + 7], 0               ; old Z value can be forgotten
    cmp byte [rcx], 0
    jne decode_and_perform_instruction.finish
    mov byte [r14 + 7], 1
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, rax, [rsi + r9] and [r14 + r15]
; if arg1 is a memory address, then the result is or [rsi + r9], r13b
; if arg1 is a register, then the result is or [r14 + r15], r13b
; sets Z register
OR:
    call prepare_arg1_arg2
    or byte [rcx], r13b
    jmp set_Z_register_and_finish


; modifies r15, r13, rcx, r9, [rsi] and [r14]
; if arg1 points to memory and arg2 is a register
; then this instruction is atomic
; otherwise it's not
; swaps the values of memory or registers pointed to by arg1 and arg2
XCHG:
    call prepare_arg1_arg2              ; [rsi] is arg1's value, r13b is arg2's value
    cmp r11b, 3                         ; check if arg2 is a register
    ja .not_atomic
    cmp r10b, 3                         ; check if arg1 is memory
    jbe .not_atomic
                                        ; is atomic
    mov r11, r14                        ; r11 can be overwritten
    add r11, r9                         ; r11 points to arg2
    xchg byte [rcx], r13b
    mov byte [r11], r13b
    jmp decode_and_perform_instruction.finish
.not_atomic:
    cmp r11b, 3
    ja .arg2_mem 
    mov r11, r14                        ; arg2 is reg
    jmp .finish
.arg2_mem:
    mov r11, rsi                        ; arg2 is in memory
.finish:
    add r11, r9                         ; r11 now points to arg2
                                        ; before add r11 was equal to rsi or r14
    mov r10b, byte [rcx]                ; get old arg1's value
    mov byte [rcx], r13b                ; arg1 = old arg2
    mov byte [r11], r10b                ; arg2 = old arg1
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, rax, [[rcx]
; [rcx] += r13b
; sets Z register
ADD:
    call prepare_arg1_arg2
    add byte [rcx], r13b
    jmp set_Z_register_and_finish


; modifies r13, r15, rax, [rcx]
; [rcx] += (r13b + C)
; sets C and Z registers
ADC:
    mov al, byte [r14 + 6]              ; get C value
    mov byte [r14 + 6], 0               ; reset C register
    call prepare_arg1_arg2
    add byte [rcx], r13b
    adc byte [r14 + 6], 0               ; if CF is set, set C register               
    add byte [rcx], al                  ; add C value
    adc byte [r14 + 6], 0               ; if CF is set, set C register.
    jmp set_Z_register_and_finish


; modifies r15, r13, rax, [[rcx]
; [rcx] -= r13b
; sets Z register
SUB:
    call prepare_arg1_arg2
    sub byte [rcx], r13b
    jmp set_Z_register_and_finish

; modifies r13, r15, rax, [rcx]
; [rcx] -= (r13b + [r14 + 6])
; sets C and Z registers
SBB:
    mov al, byte [r14 + 6]              ; save C value
    mov byte [r14 + 6], 0               ; reset C register
    call prepare_arg1_arg2
    sub byte [rcx], r13b            
    adc byte [r14 + 6], 0               ; if CF is set, set C register
    sub byte [rcx], al
    adc byte [r14 + 6], 0               ; if CF is set, set C register

    jmp set_Z_register_and_finish


; modifies r15, r13, [rcx]
; [rcx] = r13b
MOV:
    call prepare_arg1_arg2              
    mov byte [rcx], r13b
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rcx]
; [rcx] = r13b
; where r13b is an immediate
MOVI:
    call prepare_arg1_imm8
    mov byte [rcx], r13b
    jmp decode_and_perform_instruction.finish


; modifies rdx
; stops all operations as a consequence
BRK:
    mov rdx, 1
    jmp decode_and_perform_instruction.finish


; modifies [r14 + 6]
; sets C register to 0
CLC: 
    mov byte [r14 + 6], 0
    jmp decode_and_perform_instruction.finish


; modifies [r14 + 6]
; sets C register to 1
STC:
    mov byte [r14 + 6], 1
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rcx]
; or [rcx], r13b
; where r13b is an immediate
; modifies Z register
XORI:
    call prepare_arg1_imm8
    xor byte [rcx], r13b
    jmp set_Z_register_and_finish


; modifies r15, r13, [rcx]
; [rcx] += (r13b + C)
; where r13b is an immediate
; modifies Z register
ADDI:
    call prepare_arg1_imm8
    add byte [rcx], r13b
    jmp set_Z_register_and_finish

; modifies r15, r13, [rcx]
; cmp [rcx], r13b
; modifies C and Z registers
CMPI:
    mov byte [r14 + 7], 0               ; Z register's value will be now set to 1 if comparison resulted in 0
    mov byte [r14 + 6], 0               ; C register can now be forgotten and will be set now
    call prepare_arg1_imm8

    cmp byte [rcx], r13b
    jnz .set_C
    inc byte [r14 + 7]                  ; will set Z to 1 if ZF is set, stay 0 otherwise
.set_C:
                                        ; Z is set already, now set C correctly 
    setc byte [r14 + 6]
    jmp decode_and_perform_instruction.finish


; modifies r15, r13, [rcx]
; rcr [rcx], 1
; modifies C register
RCR:
    mov r15b, r10b
    call get_pointer_to_arg1
                                        ; rcx points to arg1's value
    xor r13b, r13b
    cmp byte [r14 + 6], 1               ; check if C register is set
    jne .C_register_remembered
    inc r13b                            ; r13b remembers C register's value
.C_register_remembered:
    mov byte [r14 + 6], 0               ; old value of C register can be forgotten
    cmp r13b, 1
    je .set_CF
    clc                                 ; C wasn't set, clear CF
    jmp .finish
.set_CF:
    stc                                 ; CF set <=> (C is set)
.finish:
    rcr byte [rcx], 1                   ; rotate with CF equal to C register
    setc byte [r14 + 6]                 ; set C register if CF is set after rotation
    jmp decode_and_perform_instruction.finish


; modifies r15, r13 and [r14 + 4]
; makes an unconditional jump
; ([r14 + 4] += r13b) mod 256
; where r13b is an immediate 
JMP:
                                        ; since instruction is known, rcx can be overwritten  
    mov r15b, byte [r14 + 4]            ; save old PC value
    mov r13b, bl                        ; get jump value
                                        ; if PC = 13 and jump_value = -7 so actually 249
                                        ; then PC = 262, but overflows and actually contains 6
    add byte [r14 + 4], r13b
    jmp decode_and_perform_instruction.finish


; makes a conditional jump if Z register is set to 1
JZ:
    cmp byte [r14 + 7], 1
    jne decode_and_perform_instruction.finish
                                        ; Z is set
    jmp JMP


; makes a conditional jump if Z register is set to 0
JNZ:
    cmp byte [r14 + 7], 0
    jne decode_and_perform_instruction.finish
                                        ; Z is not set
    jmp JMP           

; makes a conditional jump if C register is set to 1
JC:
    cmp byte [r14 + 6], 1
    jne decode_and_perform_instruction.finish
                                        ; C is set
    jmp JMP


; makes a conditional jump if C register is set to 0
JNC:
    cmp byte [r14 + 6], 0
    jne decode_and_perform_instruction.finish
                                        ; C is not set
    jmp JMP


; modifies rcx
; calls a decoded instructions
decode_and_perform_instruction:
    mov cx, word [rbp]                  ; get the entire instruction value
    shr cx, 14                          ; shift to get oldest byte
    cmp cx, 0                           ; check if it starts with 0
    jne .other_instruction              ; instruction does not use arg1 and arg2
.classic_instruction:
    mov cl, byte [rbp]
    cmp cl, 4                           ; 4 means ADD
    je ADD
    cmp cl, 5                           ; 5 means SUB
    je SUB
    cmp cl, 0                           ; 0 means MOV
    je MOV
    cmp cl, 2                           ; 2 means OR
    je OR
    cmp cl, 6                           ; 6 means ADC
    je ADC
    cmp cl, 7                           ; 7 means SBB
    je SBB
    cmp cl, 8                           ; 8 means XCHG
    je XCHG
    jmp .finish                         ; incorrect instruction
.other_instruction:
    mov cx, word [rbp]
    shr cx, 12                          ; get oldest byte
    cmp cx, 4                           ; 4 means MOVI
    je MOVI
    cmp cx, 15                          ; 15 means BRK
    je BRK
    cmp cx, 5                           ; 5 means XORI
    je XORI
    cmp cx, 7                           ; 7 means RCR
    je RCR
    cmp cx, 6                           ; 6 means either ADDI or CMPI                         
    je .ADDI_OR_CMPI
    cmp cx, 8                           ; 8 means either CLC or STC
    je .CLC_OR_STC
    cmp cx, 12                          ; 12 means a kind of jump instruction 
    je .jump_instruction
    jmp .finish                         ; incorrect instruction
.ADDI_OR_CMPI:
    cmp word [rbp], 0x6800
    jl ADDI                             ; instruction value is below 0x6800, so it must be ADDI
    jmp CMPI                            ; instruction is 0x6XXX and is greater or equal to 0x6800, must be CMPI
.CLC_OR_STC:
    cmp word [rbp],  0x8000
    je CLC                              ; instruction is CLC    
    jmp STC                             ; instruction is 0x8XXX and is not 0x8000, so it must be STC
.jump_instruction:
    cmp word [rbp], 0xC500              ; compare with minimal value of JZ instruction
    jnb JZ
    cmp word [rbp], 0xC400              ; compare with minimal value of JNZ instruction
    jnb JNZ
    cmp word [rbp], 0xC300              ; compare with minimal value of JC instruction
    jnb JC
    cmp word [rbp], 0xC200              ; compare with minimal value of JNC instruction
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
    push r13
    push r14
    push r15
    push rbp
    push rbx
    shl rcx, 3
    lea r14, [rel state]                ; get the state variable
    lea r14, [r14 + rcx]                ; get this core's state
.instructions_loop:
    cmp rdx, 0                          ; check if 'steps' count is 0, meaning all have been performed already
    je .finish                          ; finish if there are no instructions to perform
    inc byte [r14 + 4]       
    call decode_parameters              
    call decode_and_perform_instruction
    dec rdx
    jmp .instructions_loop
.finish:
    mov rax, qword [r14]                ; return this core's state
    pop rbx
    pop rbp
    pop r15
    pop r14
    pop r13

    ret
