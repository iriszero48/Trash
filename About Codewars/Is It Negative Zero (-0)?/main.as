SECTION .TEXT
global isNegativeZero
isNegativeZero:
    movd edx, xmm0
    mov rax, 0
    cmp edx, 0x80000000
    jne .end 
    mov rax, 1
.end:
    ret
