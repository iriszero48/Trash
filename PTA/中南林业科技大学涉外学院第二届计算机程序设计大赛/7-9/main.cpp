#include<iostream>
#include<math.h>
using namespace std;
int main()
{
    auto b=0,n=0,i=0,c=0,d=0;
    cin>>n;
    if(n==1)
        cout<<"None";
    for(b=2; b<=n; b++)
    {
        c=pow(2,b)-1;
        for(i=2; i<c; i++)
        {
            if(c%i==0)
            {
                d=1;
                break;
            }
        }
        if(d==0)
            cout<<c<<endl;
        d=0;
    }


}
