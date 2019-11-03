section .text
global growingPlant
growingPlant:
    mov rax, 1
    mov rbx, 0
.loop:
    add rbx, rdi
    cmp rbx, rdx
    jae .end
    sub rbx, rsi
    inc rax
    cmp rbx, rdx
    jbe .loop
.end:
    ret
