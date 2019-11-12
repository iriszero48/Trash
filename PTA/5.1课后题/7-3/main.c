#include<stdio.h>
#include<math.h>
int fact(int x);
int main(){
	int q,w,i,g,sum;
	g = 0,sum = 0;
	scanf("%d %d",&q,&w);
	for(i = q;i > 1;i --){
		if(g <= w){
			if(g == w||i == 2){
				if(fact(i)){
					sum += i;
					printf("%d=%d",i,sum);
					break;
					}
					else{
						printf("=%d",sum);
						break;
						
						}
				}
				else{
			if(fact(i)){
				if(g + 1 == w){
					printf("%d",i);
					sum += i;
					g ++;
					continue;
					}
					else{
			printf("%d+",i);
			sum += i;
			g ++;
			continue;
			}
			}
			else{
				continue;
				}
				}
			}
		}	
	}
	int fact(int x){
		int a,i;
		if (x == 1)
		return 0;
		a = sqrt(x);
		for (i = 2;i <= a;i++)
			if(x % i == 0){
			return 0;
			}
			return 1;		
		}
