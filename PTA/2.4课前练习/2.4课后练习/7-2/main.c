#include<stdio.h>
int main()
{
    int N,i,sign=-1;
    float sum = 0;
    scanf("%d",&N);
    for(i = 1;i<=N;i++)
    {
        sum += (sign*=-1)*((float)i)/((float)(2*i-1));
    }
    printf("%.3f\n",sum);
    return 0;
}
