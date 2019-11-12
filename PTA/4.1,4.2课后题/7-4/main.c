#include<stdio.h>
#include<math.h>
int main()
{
    int a,d=1,len=0,p=0;
    char shu[1001];
    scanf("%d",&a);
    while(1)
    {
        len++;
    if(p||d/a)
        shu[p++]='0'+d/a;
        d=d%a;
        if(d==0)
        {
            shu[p]='\0';
            printf("%s %d",shu,len);
            break;
        }
        d=d*10+1;
    }
    return 0;
}
