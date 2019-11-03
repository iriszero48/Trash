#include<iostream>
#include<cstdlib>
#include<ctime>
#include<string>
#define random(a,b) (rand()%(b-a+1)+a)
using namespace std;
bool game()
{
	srand((unsigned)time(NULL));
	string guess;
	for (guess = to_string((random(1, 9)) * 1000 + (random(0, 9)) * 100 + (random(0, 9)) * 10 + (random(0, 9))); !(guess[0] != guess[1] != guess[2] != guess[3]); guess = to_string((random(1, 9)) * 1000 + (random(0, 9)) * 100 + (random(0, 9)) * 10 + (random(0, 9))));
	cout << "猜一个4位数" << endl;
	string input;
	int m = 0, n = 0;
	for (int i = 0; i < 15; ++i)
	{
		cin >> input;
		if (guess == input)
		{
			cout << "fuck";
			return true;
		}
		else
		{
			for (int j = 0; j < 4; ++j)
			{
				if (input[j] == guess[j])
				{
					++m;
				}
				for (int k = 0; k < 4; ++k)
				{
					if (input[j] == guess[k] && j != k)
					{
						++n;
					}
				}
			}
		}
		cout << m << "A" << n << "B" << endl;
		if (i == 7)
		{
			cout << "7,con?" << endl;
			for (int choose;;)
			{
				cin >> choose;
				if (choose == 0)
				{
					return true;
				}
				else if (choose == 1)
				{
					break;
				}
			}
		}
	}
	cout << "dead";
	return true;
}
int main()
{
	game();
	return 0;
}
