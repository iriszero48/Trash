#include<stdio.h>
int main()
{
	int input;
	printf("[1] apple\n[2] pear\n[3] orange\n[4] grape\n[0] exit\n");
	for (int i = 0, exit = 0; i < 5 && exit == 0; i++)
	{
		scanf("%d", &input);
		switch (input)
		{
		case 1:
			printf("price = %.2f\n", 3.0);
			break;
		case 2:
			printf("price = %.2f\n", 2.5);
			break;
		case 3:
			printf("price = %.2f\n", 4.1);
			break;
		case 4:
			printf("price = %.2f\n", 10.2);
			break;
		case 0:
			exit = 1;
			break;
		default:
			printf("price = %.2f\n", 0.0);
			break;
		}
	}
	return 0;

}
