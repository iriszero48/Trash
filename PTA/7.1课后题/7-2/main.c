#include<stdio.h>
#include<stdint.h>
int main()
{
    int a[1001]={0};  
    int n,t,i,f,max=0,g=0;
    scanf("%d",&n);
    while(n--)
    {
        scanf("%d",&t);
        for(i=0;i<t;i++)
        {
            scanf("%d",&f);
            a[f]++;
        }
    }
    for(i=0;i<=1000;i++)
    {
        if(a[i]>=max)
        {
            max=a[i];
            g=i;
        }
    }
    printf("%d %d\n",g,a[g]);
}
