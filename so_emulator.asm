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

; todo - wywalić arg1, zastąpić wszędzie rejestrem r10

decode_parameters:
                        ; [rbp] will mean effectively [rdi + PC], that is the current instruction
    mov bpl, [r14 + 4]  ; rbp = PC
    dec bpl             ; becasue we increment it earlier for the next instruction
    movzx rbp, bpl
    shl rbp, 1          ; rbp = 2 * PC
    add rbp, rdi        ; rbp = rdi + 2 * PC

    mov r13w, word [rbp]

    mov bl, r13b
    shr r13w, 3
    shr r13b, 5
    mov r10b, r13b
    shr r13w, 3
    shr r13b, 5
    mov r11b, r13b
    ret


set_arg2_to_memory_address:
    cmp r13b, 4
    jne .check_for_5
    mov r13b, [r14 + 2]     ; get X from state
    movzx r9, r13b          ; r9 is now an address, rsi + r9 means [X]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_5:
    cmp r13b, 5
    jne .check_for_6
    mov r13b, [r14 + 3]     ; get Y from state
    movzx r9, r13b          ; r9 is now an address, rsi + r9 means [Y]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_6:
    cmp r13b, 6
    jne .check_for_7
    mov r13b, [r14 + 2]     ; get X from state


    add r13b, [r14 + 1]     ; add D, sum is X + D
    
    movzx r9, r13b          ; r9 is now an address, rsi + r9 means [X + D]
    mov r13b, [rsi + r9]
    jmp .finish
.check_for_7:
    cmp r13b, 7
    mov r13b, [r14 + 3]     ; get Y from state
    add r13b, [r14 + 1]     ; add D, sum is Y + D
    movzx r9, r13b          ; r9 is now an address, rsi + r9 means [Y + D]
    mov r13b, [rsi + r9]
.finish:
    ret


set_arg2_to_register_value:
    movzx r9, r13b          ; get arg2 value as 64-bits
    mov r13b, [r14 + r9]    ; set value of arg2 with correct register
    ret


set_arg2:
    cmp r13b, 3                     ; check arg2
    jle .arg2_is_a_register         ; arg2 is a register 
    call set_arg2_to_memory_address ; arg2 is a memory address, set r13b accordingly
    ret
.arg2_is_a_register:
    call set_arg2_to_register_value ; arg2 is a register
    ret

set_arg_1_to_memory_address:
    cmp r15b, 4
    jne .check_for_5
    mov r15b, [r14 + 2]     ; get X from state
    movzx r9, r15b          ; r9 is now an address, rsi + r9 means [X]
    jmp .finish
.check_for_5:
    cmp r15b, 5
    jne .check_for_6
    mov r15b, [r14 + 3]     ; get Y from state
    movzx r9, r15b          ; r9 is now an address, rsi + r9 means [Y]
    jmp .finish
.check_for_6:
    cmp r15b, 6
    jne .check_for_7
    mov r15b, [r14 + 2]     ; get X from state
    add r15b, [r14 + 1]     ; add D, sum is X + D
    movzx r9, r15b          ; r9 is now an address, rsi + r9 means [X + D]
    jmp .finish
.check_for_7:
    cmp r15b, 7
    mov r15b, [r14 + 3]     ; get Y from state
    add r15b, [r14 + 1]     ; add D, sum is Y + D
    movzx r9, r15b          ; r9 is now an address, rsi + r9 means [Y + D]
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
    mov r15b, r10b
    mov r13b, r11b
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
    movzx r15, r15b
    or [r14 + r15], r13b
    ; set Z register
    mov al, [r14 + r15]
    call set_Z_register
    jmp decode_and_perform_instruction.finish

; Zamienia miejscami wartości arg1 i arg2. Nie modyfikuje znaczników. 
; Jeśli arg1 wskazuje na pamięć, a arg2 jest rejestrem, to instrukcja jest atomowa.
; Jeśli arg2 wskazuje na pamięć, to instrukcja nie jest atomowa.
XCHG:
    mov r15b, r10b
    mov r13b, r11b
    ; czy arg2 jest rejestrem?
    cmp r13b, 3                 ; check if arg2 is a register
    jle .may_be_atomic_arg2_reg
                                ; arg2 is a memory address
.cant_be_atomic_arg2_mem:
    cmp r15b, 3                     ; check if arg1 is a register
    jg .is_not_atomic_arg1_mem_arg2_mem
                                    ; arg1 is a register
                                    ; arg2 is a memory address
    jmp .is_not_atomic_arg1_reg_arg2_mem


.may_be_atomic_arg2_reg:
                                    ; now check if arg1 is a memory address
    cmp r15b, 3                     ; check if arg1 is a memory address
    jg .is_atomic_arg1_mem_arg2_reg ; is > 3, so it's a memory address
    
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
    call set_arg_1_to_memory_address ; [rsi + r9] points to correct arg1 address
    mov r12, r9 
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

ADD:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address; rsi + r9 is a correct address to write onto
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

; nie wiem w którym momencie należy dodać wartość carry flag
; można albo zrobić arg2 += CF, potem arg1 += arg2
; ale to chyba nie ma znaczenia 
ADC:
    mov r15b, r10b
    mov r13b, r11b
    
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
    movzx r15, r15b

    mov al, 0           ; to later set C register
    mov [r14 + 6], al   ; to later set C register, old value can now be forgotten
    
    add [r14 + r15], r13b
    adc [r14 + 6], al   ; will add 0 if CF is 0, otherwise will ad 1

    mov al, [r14 + r15]             ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish



SUB:
    mov r15b, r10b
    mov r13b, r11b
    
    call set_arg2                   ; sets r13b to a correct arg2 value, either of a register or a memory address
    cmp r15b, 3     
    jle .arg1_is_a_register
                                    ; arg1 is a memory address
    call set_arg_1_to_memory_address ; rsi + r9 is a correct address to write onto
    sub [rsi + r9], r13b
    mov al, [rsi + r9]
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movzx r15, r15b
    sub [r14 + r15], r13b
    mov al, [r14 + r15]
    call set_Z_register
    jmp decode_and_perform_instruction.finish

; chyba dziala w porzadku, analogicznie do ADC
SBB:
    mov r15b, r10b
    mov r13b, r11b
    
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
    movzx r15, r15b

    mov al, 0               ; to later set C register
    mov [r14 + 6], al       ; to later set C register, old value can now be forgotten

    sub [r14 + r15], r13b
    adc [r14 + 6], al       ; will set C register to 1 if CF is set, 0 otherwise
    
    mov al, [r14 + r15]     ; to set Z
    call set_Z_register
    jmp decode_and_perform_instruction.finish


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

; arg1 += imm8 
MOVI:
    mov r15b, r10b
    mov r13b, bl ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address
    mov [rsi + r9], r13b
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movzx r15, r15b
    mov [r14 + r15], r13b
    jmp decode_and_perform_instruction.finish

BRK:
    mov rdx, 1
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
    mov r15b, r10b
    mov r13b, bl ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address

    xor [rsi + r9], r13b
    mov al, [rsi + r9]

    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movzx r15, r15b
    xor [r14 + r15], r13b
    mov al, [r14 + r15]

    call set_Z_register
    jmp decode_and_perform_instruction.finish

ADDI:
    mov r15b, r10b
    mov r13b, bl    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                            ; arg1 is a memory address
    call set_arg_1_to_memory_address
    add [rsi + r9], r13b    ; add imm8 to given memory address
    mov al, [rsi + r9]      ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish

.arg1_is_a_register:
    movzx r15, r15b
    add [r14 + r15], r13b   ; add imm8 to given memory address
    mov al, [r14 + r15]     ; to set Z register
    call set_Z_register
    jmp decode_and_perform_instruction.finish

; Odejmuje wartość imm8 od arg1, ale nie zapisuje wyniku. Ustawia znaczniki C i Z zgodnie z wynikiem operacji.
CMPI:
    mov r15b, r10b
    mov r13b, bl    ; r13b is an immediate

    cmp r15b, 3
    jle .arg1_is_a_register
                            ; arg1 is a memory address
    call set_arg_1_to_memory_address
    mov [r14 + 7], byte 0   ; Z register's value will be now set to 1 if comparison resultet in 0
    mov [r14 + 6], byte 0   ; C register can now be forgotten and will be set now

    cmp [rsi + r9], r13b
    jnz .set_C
    inc byte [r14 + 7]      ; will set Z to 1 if ZF is set, stay 0 otherwise

.arg1_is_a_register:
    movzx r15, r15b
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


RCR:
    mov r15b, r10b        ; arg1 is the value that will be rotated alongside C register
    cmp r15b, 3
    jle .arg1_is_a_register
    call set_arg_1_to_memory_address

    xor r13b, r13b              ; r13b = 0
    cmp [r14 + 6], byte 1       ; check if C register is set
    jne .CF_done_arg1_is_a_memory_address

    inc r13b                    ; r13b = 1, representing that CF flag is set
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
    mov [r14 + 6], byte 0       ; old value of C register can be forgotten, will be set quickly
                                ; CF is ready, now perform rcr
                                
    rcr byte [rsi + r9], 1
                                ; r14 + r15 is a register correctly rotated with CF
    setc [r14 + 6]              ; set C register if CF is set after rcr instruction
                                ; operation finished
    ret


.arg1_is_a_register:
    movzx r15, r15b
    xor r13b, r13b              ; r13b = 0
    cmp [r14 + 6], byte 1       ; check if C register is set
    jne .CF_done_arg1_is_a_register
                                ; C register is set, set CF flag as well
    inc r13b                    ; r13b = 1, representing that CF flag is set
.CF_done_arg1_is_a_register:
                                ; CF set <=> (r13b == 1)
    cmp r13b, 1
    jne .CF_is_not_set
                                ; CF is set
    stc
    jmp .finish_arg1_register

.CF_is_not_set:
    clc
    jmp .finish_arg1_register

.finish_arg1_register:
                                ; CF value is set correctly
    mov [r14 + 6], byte 0       ; old value of C register can be forgotten, will be set quickly
                                ; CF is ready, now perform rcr

    rcr byte [r14 + r15], 1     ; rotate with carry
                                ; r14 + r15 is a register correctly rotated with CF
    setc [r14 + 6]              ; set C register if CF is set after rcr instruction
                                ; operation finished
    ret


; r14 + 4 to PC
; czy PC + r13b > 255?
JMP:
                                ; since instruction is known, r12 can be overwritten
    
    mov r15b, [r14 + 4]         ; save old PC value
    mov r13b, bl        ; get jump value
                                ; if PC = 13 and jump_value = -7 so actually 249
                                ; then PC = 262, but overflows and actually contains 6
    add [r14 + 4], r13b
                                ; [r14 + 4] is set
                                ; now adjust rbp
                                ; if [r14 + 4] is now smaller than it was
                                ; then make rbp smaller as well
                                ; by how much?
                                ; by 2 times new_PC - old_PC

    mov r12b, [r14 + 4]         ; r12b is the new_PC
                                ; if new > old, then add new - old
                                ;               else add old - new
    cmp r15b, r12b
    jl .new_PC_is_bigger
                                ; new PC is smaller, diff is old - new
    

    sub r15b, r12b              ; difference: old - new        
    movzx r15, r15b             ; move to 64-bit register
    jmp decode_and_perform_instruction.finish
    
.new_PC_is_bigger:
                                ; r12b > r15b
    sub r12b, r15b              ; difference: new - old
    mov r15b, r12b              ; move to 64-bit register
    movzx r15, r15b
    jmp decode_and_perform_instruction.finish

JZ:
    cmp [r14 + 7], byte 1
    jne decode_and_perform_instruction.finish
                            ; Z is set
    jmp JMP

JNZ:
    cmp [r14 + 7], byte 0

    jne decode_and_perform_instruction.finish
                            ; Z is not set
    jmp JMP           

JC:
    cmp [r14 + 6], byte 1
    jne decode_and_perform_instruction.finish
                            ; C is set
    jmp JMP

JNC:
    cmp [r14 + 6], byte 0
    jne decode_and_perform_instruction.finish
                            ; C is not set
    jmp JMP

decode_and_perform_instruction:
    mov r12w, word [rbp]

    shr r12w, 14

    cmp word r12w, 0
    jne .other_instruction              ; instruction does not use arg1 and arg2
.classic_instruction:
    mov r12b, byte [rbp]
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
    cmp r12b, 8                         
    je XCHG

    jmp .finish                         ; incorrect instruction

.other_instruction:


    mov r12w, word [rbp]
    shr r12w, 12                    
    cmp r12w, 4
    je MOVI
    cmp r12w, 15
    je BRK
    cmp r12w, 5
    je XORI
    cmp r12w, 7
    je RCR
    cmp r12w, 6                         
    je .ADDI_OR_CMPI
    cmp r12w, 8
    je .CLC_OR_STC
    cmp r12w, 12
    je .jump_instruction

.ADDI_OR_CMPI:
    cmp [rbp], word 0x6800
    jl ADDI                             ; instruction value is below 0x6800, so it must be ADDI
    jmp CMPI
                                        ; instruction is 0x6XXX and is greater or equal to 0x6800, must be CMPI
.CLC_OR_STC:
    cmp [rbp], word 0x8000
    je CLC                              ; instruction is CLC    
    jmp STC                             ; instruction is 0x8XXX and is not 0x8000, so it must be STC

.jump_instruction:
    cmp [rbp], word 0xC500  ; compare with minimal value of JZ instruction
    jge JZ
    cmp [rbp], word 0xC400  ; compare with minimal value of JNZ instruction
    jge JNZ
    cmp [rbp], word 0xC300  ; compare with minimal value of JC instruction
    jge JC
    cmp [rbp], word 0xC200  ; compare with minimal value of JNC instruction
    jge JNC
                                        ; it's a jump instruction, bute none of the above
                                        ; so it must be simply a JMP instruction
    jmp JMP
    

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
    push rbx
    ; here begins the main loop
    ; we can modify rdx, so that will be the main loop iterator

    lea r14, [rel state]
    lea r14, [r14 + rcx * 8]
    cmp rdx, 0
    je .finish
.instructions_loop:
    inc byte [r14 + 4]       ; increase program counter
    call decode_parameters
    call decode_and_perform_instruction

    dec rdx
    cmp rdx, 0
    jg .instructions_loop

.finish:

    mov rax, [r14]
    pop rbx
    pop rbp
    pop r15
    pop r14
    pop r13
    pop r12

    ret

