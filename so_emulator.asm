global so_emul

section .bss

state:          resb 8
arg1:           resb 1
arg2:           resb 1
imm8:           resb 1
instruction:    resw 1

section .text


decode_parameters:
    mov r13w, word [rdi + r8 * 2]       ; r13w constains current instruction value
    mov [rel instruction], r13w ; save instruction value
    mov [rel imm8], r13b
    shr r13w, 3
    shr r13b, 5
    mov [rel arg1], r13b
    shr r13w, 3
    shr r13b, 5
    mov [rel arg2], r13b
    ret



set_arg2_to_memory_address:
    cmp r13b, 4
    jne .check_for_5
    mov r13b, [r14 + 2]     ; get X from state
    movsx r9, r13b          ; r9 is now an address, rsi + r9 means [X]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_5:
    cmp r13b, 5
    jne .check_for_6
    mov r13b, [r14 + 3]     ; get Y from state
    movsx r9, r13b          ; r9 is now an address, rsi + r9 means [Y]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_6:
    cmp r13b, 6
    jne .check_for_7
    mov r13b, [r14 + 2]     ; get X from state
    add r13b, [r14 + 1]     ; add D, sum is X + D
    movsx r9, r13b          ; r9 is now an address, rsi + r9 means [X + D]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_7:
    cmp r13b, 6
    mov r13b, [r14 + 3]     ; get Y from state
    add r13b, [r14 + 1]     ; add D, sum is Y + D
    movsx r9, r13b          ; r9 is now an address, rsi + r9 means [Y + D]
    mov r13b, [rsi + r9]
.finish:
    ret


set_arg2_to_register_value:
    movsx r9, r13b          ; get arg2 value as 64-bits
    mov r13b, [r14 + r9]    ; set value of arg2 with correct register
    ; mov r13b, 58
    ret


set_arg2:
    cmp r13b, 3                     ; check arg2
    jle .arg2_is_a_register      ; arg2 is a register 
    call set_arg2_to_memory_address ; arg2 is a memory address, set r13b accordingly
.arg2_is_a_register:
    call set_arg2_to_register_value ; arg2 is a register
    ret

set_arg_1_to_memory_address:
    cmp r15b, 4
    jne .check_for_5
    mov r15b, [r14 + 2]     ; get X from state
    movsx r9, r15b          ; r9 is now an address, rsi + r9 means [X]
    jmp .finish
.check_for_5:
    cmp r15b, 5
    jne .check_for_6
    mov r15b, [r14 + 3]     ; get Y from state
    movsx r9, r15b          ; r9 is now an address, rsi + r9 means [Y]
    jmp .finish
.check_for_6:
    cmp r15b, 6
    jne .check_for_7
    mov r15b, [r14 + 2]     ; get X from state
    add r15b, [r14 + 1]     ; add D, sum is X + D
    movsx r9, r15b          ; r9 is now an address, rsi + r9 means [X + D]
    jmp .finish
.check_for_7:
    cmp r15b, 6
    mov r15b, [r14 + 3]     ; get Y from state
    add r15b, [r14 + 1]     ; add D, sum is Y + D
    movsx r9, r15b          ; r9 is now an address, rsi + r9 means [Y + D]
.finish:
    ret  

; sets Z register to either 0 or 1
; modifies AL register
set_Z_register:
    cmp al, 0
    jne .ZF_is_not_zero
    ; al == 0
    inc al ; al = 1
    mov [r14 + 7], al
    ret
.ZF_is_not_zero:
    mov al, 0
    mov [r14 + 7], al
    ret

; set C register to 1 if carry flag is set
; set C register to 0 otherwise
; C register is at [r14 + 6]
set_C_register:
    ; mov al, 0
    ; mov [r14 + 6], al
    ; adc [r14 + 6], al   ; will add 0 if CF is 0, otherwise will ad 1
    ret


OR:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    call set_arg2 ; sets r13b to a correct arg2 value
    cmp r15b, 3
    jle .arg1_is_a_register

    call set_arg_1_to_memory_address
    or [rsi + r9], r13b
    ; set Z register
    mov al, [rsi + r9]
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    or [r14 + r15], r13b
    ; set Z register
    mov al, [r14 + r15]
    call set_Z_register
    jmp decode_and_perform_instruction.finish



ADD:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address ; rsi + r9 is a correct address to write onto
    add [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    add [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish

; TODO !!!!!!!!!!
; tutaj jest cos nie tak z ustawianiem C i Z
; inne funkcje generalnie dzialaja, z tego co widze
; trzeba spytac Marka jak tu powinno sie to rozkminic
ADC:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    add r13b, [r14 + 6]             ; add potentially the carry flag
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address ; rsi + r9 is a correct address to write onto
    
    mov al, 0                       ; to later set C register
    mov [r14 + 6], al               ; to later set C register, old value can now be forgotten
    
    add [rsi + r9], r13b
    adc [r14 + 6], al               ; will add 0 if CF is 0, otherwise will ad 1


    mov al, [rsi + r9]              ; to set Z 
    call set_Z_register
    ; call set_C_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b

    mov al, 0           ; to later set C register
    mov [r14 + 6], al   ; to later set C register, old value cant now be forgotten
    
    add [r14 + r15], r13b
    adc [r14 + 6], al   ; will add 0 if CF is 0, otherwise will ad 1

    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
    ; call set_C_register
    jmp decode_and_perform_instruction.finish



SUB:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address ; rsi + r9 is a correct address to write onto
    sub [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    sub [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish

MOV:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    
    call set_arg2
    cmp r15b, 3
    jle .arg1_is_a_register

    call set_arg_1_to_memory_address
    mov [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish
.arg1_is_a_register:
    movsx r15, r15b
    mov [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish

; arg1 += imm8 
MOVI:
    mov r15b, [rel arg1]
    mov r13b, [rel imm8] ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address
    mov [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    mov [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish


; to może być do zmiany
; nie wiem czy to na pewno dobrze robi, że usta
BRK:
    mov r10, 1      ; 1 means to break
    jmp decode_and_perform_instruction.finish

CLC:
    mov r15b, 0     ; value for C value
    mov [r14 + 6], r15b
    jmp decode_and_perform_instruction.finish

STC:
    mov r15b, 1
    mov [r14 + 6], r15b
    jmp decode_and_perform_instruction.finish

XORI:
    mov r15b, [rel arg1]
    mov r13b, [rel imm8] ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address
    xor [rsi + r9], r13b
    mov al, [rsi + r9]
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    mov [r14 + r15], r13b
    mov al, [r14 + r15]
    call set_Z_register
    jmp decode_and_perform_instruction.finish


decode_and_perform_instruction:
    mov r12w, [rdi + r8 * 2] 
    shr r12w, 14
    
    cmp word r12w, 0
    jne .other_instruction              ; instruction does not use arg1 and arg2
.classic_instruction:
    mov r12b, [rel instruction]         ; look at the first byte to check instruction
    cmp r12b, 4
    je ADD
    cmp r12b, 5
    je SUB
    cmp r12b, 0
    je MOV
    cmp r12b, 2
    je OR
    cmp r12b, 6
    je ADC


.other_instruction:
    mov r12w, [rel instruction]
    shr r12w, 12                    ; look at last 4 bytes
    cmp r12w, 4
    je MOVI
    cmp r12w, 15
    je BRK
    cmp r12w, 5
    je XORI
    ; cmp r12b, 6                         ; could be ADDI or CMPI
    ; je ADDI_or_CMPI



.finish:
    ret

; rdi - uint16_t const *code, instructions to perform
; rsi - uint8_t *data, 256 bytes of memory
; rdx - size_t steps, how many instructions to perform
; rcx - size_t cores, for concurrent calculations
so_emul:
    push r12
    push r13
    push r14
    push r15
    ; here begins the main loop
    ; we can modify rdx, so that will be the main loop iterator
    mov r14, state
    mov r8, 0
    mov r10, 0              ; r10 contains info about break. 1 means BRK was encountered
.instructions_loop:
    inc byte [r14 + 4]       ; increase program counter
    call decode_parameters
    call decode_and_perform_instruction
    ; check for BRK
    cmp r10, 1
    je .finish
    
    inc r8
    cmp r8, rdx
    jne .instructions_loop

.finish:
    mov rax, [rel state]
    pop r15
    pop r14
    pop r13
    pop r12

    ret
