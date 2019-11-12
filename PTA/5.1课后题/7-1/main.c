#include<stdio.h>
#include<math.h>
int main()
{
  int i,n,a,b,sum;
  scanf("%d\n",&n);
  for(i=1;i<=n;i++){
  	sum=1;
    scanf("%d\n",&a);
    for(b=2;b<sqrt(a);b++){
	  if(a%b==0){
	  	sum=0;
	  	break;
	  }
    }
    if(sum==0||a==1)
    printf("No\n");
    else
    printf("Yes\n");
  }
  return 0;
}
