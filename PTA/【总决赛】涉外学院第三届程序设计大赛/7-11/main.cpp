#include <iostream>
#include <stdio.h>
using namespace std;
int main()
{
    int x,k;
    while (cin >> x >> k)
    {
        double s=x;
        for(int __i=0;__i<k;++__i)
        {
            s*=1.1;
        }
        printf("%.2f\n",s);
    }
}
