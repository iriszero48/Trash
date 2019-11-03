SECTION .text
global make_negative

make_negative: 
    mov eax, edi
    cdq
    mov eax, edx
    xor eax, edi
    sub eax, edx
    neg eax
    ret
