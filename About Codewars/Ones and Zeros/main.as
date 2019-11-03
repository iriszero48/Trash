SECTION .text
global binary_array_to_number
binary_array_to_number:
    mov edx, 0
    mov eax, 0
.loop:
    shl eax, 1
    add eax, [rdi + rdx]
    add edx, 4
    dec esi
    cmp esi, 0
    jne .loop
.end:
    ret
