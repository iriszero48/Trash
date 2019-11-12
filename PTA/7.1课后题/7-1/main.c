#include<stdio.h>
int main()
{
	int g,i;
	int a,b,x[20];
	g = 0;
	scanf("%d %d\n",&a,&b);
	for(i = 0;i < a;i ++){
		scanf("%d",&x[i]);
		}
	for(i = 0;i < a;i ++){
		if(b == x[i]){
			printf("%d",i);
			g = 1;
			break;
			}
			}
			if(g == 0){
				printf("Not Found");
				}
		
		
	
	
	
	}
