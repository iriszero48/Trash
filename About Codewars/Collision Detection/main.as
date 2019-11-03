global is_collision
section .text
is_collision:
    mov rax, 0
    subss xmm0, xmm3
    subss xmm1, xmm4
    mulss xmm0, xmm0
    mulss xmm1, xmm1
    addss xmm0, xmm1
    addss xmm2, xmm5
    mulss xmm2, xmm2
    comiss xmm0, xmm2
    jae .end 
    mov rax, 1
.end:
    ret
