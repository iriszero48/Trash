#include <stdio.h>
#include <stdbool.h>
#include <math.h>
inline bool p(int x)
{
    for(int i=2;i<((int)sqrt(x))+1;++i)
    {
        if(x%i==0)return false;
    }
    return true;
}
int main()
{
    int n=0,count=0;
    scanf("%d",&n);
    for(int i =2;i<=n;++i)
    {
        if(p(i))
        {
            ++count;
        }
    }
    printf("%d",count);
}
