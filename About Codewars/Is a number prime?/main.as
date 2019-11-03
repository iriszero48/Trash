global is_prime

section .text

is_prime:
    cmp edi, 1
    jle false
    cmp edi, 2
    je true
    mov ebx, 2
    jmp forif
for:
    mov eax, edi
    cdq
    idiv ebx
    mov eax, edx
    test eax, eax
    jne forloop
    jmp false
forloop:
    inc ebx
forif:
    mov eax, edi
    mov edx, eax
    shr edx, 0x1f
    add eax, edx
    sar eax, 1
    cmp ebx, eax
    jle for
true:
    mov eax, 1
    ret
false:
    mov eax, 0
    ret
