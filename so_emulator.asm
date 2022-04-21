global so_emul

section .bss

state:          resb 8
arg1:           resb 1
arg2:           resb 1
imm8:           resb 1
instruction:    resw 1
steps:          resq 1

section .text


decode_parameters:
    ; tu nalezy przesuwac samo [rdi], zeby moc krokowo wykonywac instrukcje
    mov r13w, word [rdi + rbp]       ; r13w constains current instruction value
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

; nie wiem w którym momencie należy dodać wartość carry flag
; można albo zrobić arg2 += CF, potem arg1 += arg2
; ale to chyba nie ma znaczenia 
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
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b

    mov al, 0           ; to later set C register
    mov [r14 + 6], al   ; to later set C register, old value can now be forgotten
    
    add [r14 + r15], r13b
    adc [r14 + 6], al   ; will add 0 if CF is 0, otherwise will ad 1

    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
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

; chyba dziala w porzadku, analogicznie do ADC
SBB:
    mov r15b, [rel arg1]
    mov r13b, [rel arg2]
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    sub r13b, [r14 + 6]             ; subtract value of C register, now r13b contains correct value

    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address ; rsi + r9 is a correct address to write onto
    
    mov al, 0
    mov [r14 + 6], al               ; to later set C register, old value can now be forgotten
        
    sub [rsi + r9], r13b            ; main operation
    sbb [r14 + 6], al               ; will set C register to 1 if CF is set, 0 otherwise

    mov al, [rsi + r9]              ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b

    mov al, 0               ; to later set C register
    mov [r14 + 6], al       ; to later set C register, old value can now be forgotten

    sub [r14 + r15], r13b
    adc [r14 + 6], al       ; will set C register to 1 if CF is set, 0 otherwise
    
    mov al, [r14 + r15]     ; to set Z
    call set_Z_register
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
    xor r15b, r15b     ; value for C value
    mov [r14 + 6], r15b
    jmp decode_and_perform_instruction.finish

STC:
    mov r15b, 1
    mov [r14 + 6], r15b
    jmp decode_and_perform_instruction.finish

; do przetestowania jeszcze
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

    xor [r14 + r15], r13b
    mov al, [r14 + r15]

    call set_Z_register
    jmp decode_and_perform_instruction.finish

ADDI:
    mov r15b, [rel arg1]
    mov r13b, [rel imm8]    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                            ; arg1 is a memory address
    call set_arg_1_to_memory_address
    add [rsi + r9], r13b    ; add imm8 to given memory address
    mov al, [rsi + r9]      ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movsx r15, r15b
    add [r14 + r15], r13b   ; add imm8 to given memory address
    mov al, [r14 + r15]     ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish

; Odejmuje wartość imm8 od arg1, ale nie zapisuje wyniku. Ustawia znaczniki C i Z zgodnie z wynikiem operacji.
CMPI:
    mov r15b, [rel arg1]
    mov r13b, [rel imm8]    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                            ; arg1 is a memory address
    call set_arg_1_to_memory_address
    mov [r14 + 7], byte 0   ; Z register's value will be now set to 1 if comparison resultet in 0
    mov [r14 + 6], byte 0   ; C register can now be forgotten and will be set now

    cmp [rsi + r9], r13b
    jnz .set_C
    inc byte [r14 + 7]      ; will set C to 1 if CF is set, stay 0 otherwise

.arg1_is_a_register:
    movsx r15, r15b
    ; cmp arg1, imm8
    mov [r14 + 7], byte 0   ; Z register's value will be now set to 1 if comparison resultet in 0
    mov [r14 + 6], byte 0   ; C register can now be forgotten and will be set now
    cmp [r14 + r15], r13b
    jnz .set_C
                            ; result is 0, set Z register
    inc byte [r14 + 7]      ; set C to 1
.set_C:
                            ; Z is set already, now set C correctly 
    adc [r14 + 6], byte 0   ; will set C to 1 if CF is set, stay 0 otherwise

    jmp decode_and_perform_instruction.finish

; todo
RCR:


    ret

decode_and_perform_instruction:
    mov r12w, [rdi + rbp]
    add rbp, 2
    mov [rel steps], rbp
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
    cmp r12b, 7
    je SBB


.other_instruction:
    mov r12w, [rel instruction]
    shr r12w, 12                    
    cmp r12w, 4
    je MOVI
    cmp r12w, 15
    je BRK
    cmp r12w, 5
    je XORI
    cmp r12b, 7
    je RCR
    cmp r12b, 6                         ; could be ADDI or CMPI
    je .ADDI_OR_CMPI
    cmp r12b, 8
    je .CLC_OR_STC

.ADDI_OR_CMPI:
    cmp [rel instruction], word 0x6800
    jl ADDI                             ; instruction value is below 0x6800, so it must be ADDI
    jmp CMPI
                                        ; instruction is 0x6XXX and is greater or equal to 0x6800, must be CMPI
.CLC_OR_STC:
    cmp [rel instruction], word 0x8000
    je CLC                              ; instruction is CLC    
    jmp STC                             ; instruction is 0x8XXX and is not 0x8000, so it must be STC

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
    push rbp
    ; here begins the main loop
    ; we can modify rdx, so that will be the main loop iterator
    mov rbp, [rel steps]    ; rbp contains the number of instructions performed already
                            ; important, because code can be performed in steps
                            ; by calling so_emul multiple times to perform next instructions
    mov r14, state
    xor r8, r8
    xor r10, r10              ; r10 contains info about break. 1 means BRK was encountered
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
    pop rbp
    pop r15
    pop r14
    pop r13
    pop r12

    ret
