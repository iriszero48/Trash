#include<stdio.h>
int main()
{
    int N, i;
    scanf("%d", &N);
    for (i = 0; i < N; i += 5);
    if (i - N > 1)
        printf("Fishing in day %d", N);
    else
        printf("Drying in day %d", N);
    return 0;
}
