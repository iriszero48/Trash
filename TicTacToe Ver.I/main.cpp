#include<iostream>
#include<conio.h>
#include<Windows.h>
#include<cmath>
#include<WinUser.h>
#define _2by1_player -98
#define _2by1_bot -102
using namespace std;
bool computer_first = false;
bool computer_vs_people = true;
int chess;
bool first;
int get_chess()
{
	chess += 1;
	return chess % 2 == 0 ? -1 : 1;
}
void set_chess_false()
{
	chess -= 1;
}
struct Pos
{
	int x;
	int y;
}position = { 1,1 };
Pos player_last_step = { 0 ,0 };
int chessman[3][3] = { {-100,-100,-100},{-100,-100,-100},{-100,-100,-100} };
//1 -1 -100
void set_position(int x, int y)
{
	position.x = x;
	position.y = y;
	COORD pos = { x ,y };
	HANDLE Out = GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleCursorPosition(Out, pos);
}
void left()
{
	if (position.x > 0)
	{
		set_position(position.x - 4, position.y);
	}
}
void right()
{
	if (position.x < 10)
	{
		set_position(position.x + 4, position.y);
	}
}
void up()
{
	if (position.y > 0)
	{
		set_position(position.x, position.y - 2);
	}
}
void down()
{
	if (position.y < 5)
	{
		set_position(position.x, position.y + 2);
	}
}
int mapx()
{
	return (position.x - 2) / 4;
}
int mapy()
{
	return (position.y - 1) / 2;
}
int get_chessman(int x, int y)
{
	return chessman[y][x];
}
bool set_chessman(int x, int y, int value)
{
	if (get_chessman(x, y) != -100)
	{
		return false;
	}
	else
	{
		chessman[y][x] = value;
		return true;
	}
}
int x_sum(int y)
{
	int sum = 0;
	for (int i = 0; i < 3; ++i)
	{
		sum += get_chessman(i, y);
	}
	return sum;
}
int y_sum(int x)
{
	int sum = 0;
	for (int i = 0; i < 3; ++i)
	{
		sum += get_chessman(x, i);
	}
	return sum;
}
int xx_sum()
{
	int sum = 0;
	for (int i = 0; i < 3; ++i)
	{
		sum += get_chessman(i, i);
	}
	return sum;
}
int xx_sum_rev()
{
	return get_chessman(0, 2) + get_chessman(1, 1) + get_chessman(2, 0);
}
bool xy_sum(int value)
{
	if (xx_sum() == value)
	{
		return true;
	}
	if (xx_sum_rev() == value)
	{
		return true;
	}
	else
	{
		return false;
	}
}
bool judge(int value)
{
	for (int i = 0; i < 3; ++i)
	{
		if (x_sum(i)==value)
		{
			return true;
		}
	}
	for (int i = 0; i < 3; ++i)
	{
		if (y_sum(i) == value)
		{
			return true;
		}
	}
	return xy_sum(value);
}
bool dogfall()
{
	for (int i = 0; i < 3; ++i)
	{
		for (int j = 0; j < 3; ++j)
		{
			if (get_chessman(i, j) == -100)
			{
				return false;
			}
		}
	}
	return true;
}
// 1 win 0 normal -1 error 2 dogfall
int decide(int chess)
{
	if (set_chessman(mapx(), mapy(), chess))
	{
		player_last_step.x = mapx();
		player_last_step.y = mapy();
		cout << ((chess == 1) ? "x" : "o");
		set_position(position.x, position.y);
		if (judge(chess * 3) == true)
		{
			cout << "fuck";
			return 1;
		}
		if (dogfall())
		{
			return 2;
		}
		return 0;
	}
	else
	{
		set_chess_false();
		return -1;
	}
}
int bot_decide(int x, int y, int chess)
{
	set_chessman(x, y, chess);
	set_position(2 + 4 * x, 1 + 2 * y);
	cout << ((chess == 1) ? "x" : "o");
	set_position(2 + 4 * x, 1 + 2 * y);
	if (judge(-3) == true)
	{
		cout << "ok";
		return 1;
	}
	if (dogfall())
	{
		return 2;
	}
	return 0;
}
int bot(int chess)
{
	for (int i = 0; i < 3; ++i)
	{
		if (x_sum(i) == _2by1_bot)
		{
			for (int j = 0; j < 3; ++j)
			{
				if (get_chessman(j, i) == -100)
				{
					return bot_decide(j, i, chess);
				}
			}
		}
	}
	for (int i = 0; i < 3; ++i)
	{
		if (y_sum(i) == _2by1_bot)
		{
			for (int j = 0; j < 3; ++j)
			{
				if (get_chessman(i, j) == -100)
				{
					return bot_decide(i, j, chess);
				}
			}
		}
	}
	if (xx_sum() == _2by1_bot)
	{
		for (int i = 0; i < 3; ++i)
		{
			if (get_chessman(i, i) == -100)
			{
				return bot_decide(i, i, chess);
			}
		}
	}
	if (xx_sum_rev() == _2by1_bot)
	{
		if (get_chessman(0, 2) == -100)
		{
			return bot_decide(0, 2, chess);
		}
		if (get_chessman(1, 1) == -100)
		{
			return bot_decide(1, 1, chess);
		}
		if (get_chessman(2, 0) == -100)
		{
			return bot_decide(2, 0, chess);
		}
	}
	for (int i = 0; i < 3; ++i)
	{
		if (x_sum(i) == _2by1_player)
		{
			for (int j = 0; j < 3; ++j)
			{
				if (get_chessman(j, i) == -100)
				{
					return bot_decide(j, i, chess);
				}
			}
		}
	}
	for (int i = 0; i < 3; ++i)
	{
		if (y_sum(i) == _2by1_player)
		{
			for (int j = 0; j < 3; ++j)
			{
				if (get_chessman(i, j) == -100)
				{
					return bot_decide(i, j, chess);
				}
			}
		}
	}
	if (xx_sum() == _2by1_player)
	{
		for (int i = 0; i < 3; ++i)
		{
			if (get_chessman(i, i) == -100)
			{
				return bot_decide(i, i, chess);
			}
		}
	}
	if (xx_sum_rev() == _2by1_player)
	{
		if (get_chessman(0, 2) == -100)
		{
			return bot_decide(0, 2, chess);
		}
		if (get_chessman(1, 1) == -100)
		{
			return bot_decide(1, 1, chess);
		}
		if (get_chessman(2, 0) == -100)
		{
			return bot_decide(2, 0, chess);
		}
	}
	for (int i = 0; i < 3; ++i)
	{
		for (int j = 0; j < 3; ++j)
		{
			if (get_chessman(i, j) == -100)
			{			
				return bot_decide(i, j, chess);
			}
		}
	}
}
bool game()
{
	int d;
	int bd;
	while (true)
	{
		int input = _getch();
		if (GetAsyncKeyState(VK_UP) < 0)
		{
			while (GetAsyncKeyState(VK_UP) < 0)
				1;
			up();
		}
		if (GetAsyncKeyState(VK_DOWN) < 0)
		{
			while (GetAsyncKeyState(VK_DOWN) < 0)
				1;
			down();
		}
		if (GetAsyncKeyState(VK_LEFT) < 0)
		{
			while (GetAsyncKeyState(VK_LEFT) < 0)
				1;
			left();
		}
		if (GetAsyncKeyState(VK_RIGHT) < 0)
		{
			while (GetAsyncKeyState(VK_RIGHT) < 0)
				1;
			right();
		}
		if (GetAsyncKeyState(VK_SPACE) < 0)
		{
			while (GetAsyncKeyState(VK_SPACE) < 0)
				1;
			d = decide(get_chess());
			if (d == 1)
			{
				return true;
			}
			else if (d == 2)
			{
				return false;
			}
			else if (d == 0)
			{
				if (computer_vs_people)
				{
					bd = bot(get_chess());
					if (bd == 1)
					{
						return true;
					}
					else if (bd == 2)
					{
						return false;
					}
				}
			}
		}
	}
}
void init()
{
	system("cls");
	chess = 0;
	first = true;
	position = { 1,1 };
	player_last_step.x = 0;
	player_last_step.y = 0;
	for (int i = 0; i < 3; ++i)
	{
		for (int j = 0; j < 3; ++j)
		{
			chessman[i][j] = -100;
		}
	}
	cout << "+---+---+---+" << endl << "|   |   |   |" << endl << "+---+---+---+" << endl << "|   |   |   |" << endl << "+---+---+---+" << endl << "|   |   |   |" << endl << "+---+---+---+";
	set_position(2, 1);
	if (computer_first&&computer_vs_people)
	{
		bot_decide(1, 1, -1);
	}
	set_position(2, 1);
}
void select()
{
	int inputn;
	cout << "1-人机 2-人人 ：";
	cin >> inputn;
	if (inputn == 2)
	{
		computer_vs_people = false;
	}
	char inputc;
	cout << "是否先走（y/n）";
	cin >> inputc;
	if (inputc == 'n')
	{
		computer_first = true;
	}
}
int main()
{
	select();
	while (1)
	{
		init();
		game();
		system("pause");
	}
}
