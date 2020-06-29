SECTION .text
global small_enough

small_enough:
.begin:
    cmp rsi, 0
    je .end
    mov ebx, [rdi]
    add rdi, 4
    dec rsi
    cmp ebx, edx
    jle .begin
    mov eax, 0
    ret
.end:
    mov eax, 1
    ret
