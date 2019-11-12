#include<stdio.h>
int main(){
  int a,i,h,w;
  double x,y,z;
  scanf("%d\n",&a);
  for(i = 1;i <= a;i ++){
    scanf("%d%d",&h,&w);
    w = w;
    x = (h -100.0)*0.9*2;
    y = w - x;
    z = x - w;
    if(y >= x*0.1){
      printf("You are tai pang le!\n");
    }
    else if(z >= x*0.1){
      printf("You are tai shou le!\n");
    }
    else{
      printf("You are wan mei!\n");
    }
  }
  return 0;
}
