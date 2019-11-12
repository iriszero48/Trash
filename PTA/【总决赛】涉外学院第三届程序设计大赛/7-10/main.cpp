#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <algorithm>
using namespace std;
int main()
{
    int n;
    double x;
    vector<double> v;
    while(cin >> n)
    {
        for(int i=0;i<n;++i)
        {
            cin >> x;
            v.push_back(x);
        }
        sort(v.begin(),v.end());
        for(int i=n-1;i>0;--i)
        {
            printf("%.2f ",v[i]);
        }
        printf("%.2f\n",v[0]);
        v.clear();
    }
}
