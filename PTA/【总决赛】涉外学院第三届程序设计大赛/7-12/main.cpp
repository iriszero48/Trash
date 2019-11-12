#include <iostream>
#include <stdlib.h>
using namespace std;
int main()
{
    int n,m;
    cin>>n>>m;
    int* v=(int*)malloc(sizeof(n));
    for(int i=0;i<n;++i)
    {
        int x;
        cin >>x;
        v[i]=x;
    }
    for(int _i=0;_i<m;++_i)
    {
        int x,l,r;
        cin >>x>>l>>r;
        if(x==1)
        {
            int m=0,mi=0;
            for(int i=l-1;i<r;++i)
            {
                if(v[i]>m)
                {
                    m=v[i];
                    mi=i+1;
                }
            }
            cout<<mi<<" "<<m<<endl;
        }
        else
        {
            v[l-1]=r;
        }
    }

}
