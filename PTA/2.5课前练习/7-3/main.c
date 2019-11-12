#include<stdio.h>
#include<math.h>
int main()
{
	int n;
	double sum = 0;
    scanf("%d",&n);
    for(int i = 1;i<=n;i++)
    {
		sum += pow(2, i);
    }
    printf("result = %.0f",sum);
    return 0;
}
