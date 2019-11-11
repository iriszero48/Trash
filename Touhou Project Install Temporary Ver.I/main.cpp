#include "stdafx.h"
#include<fstream>
#include<conio.h>
#include<Windows.h>
#include<cstdlib>
#include<WinUser.h>
#include<cstring>
#include<iostream>
using namespace std;
string str;
struct AllValue
{
	unsigned int line : 4;
	bool allContinue;
	bool isWindows7;
};
AllValue allValue = { 0,true,true };
void SetPosition(unsigned char line)
{
	COORD pos = { 0,line };
	HANDLE Out = GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleCursorPosition(Out, pos);
}

void Print(bool upOrDown)
{
	SetPosition(allValue.line);
	cout << '\0' << allValue.line - 2 << '\0';
	if (upOrDown)
	{
		--allValue.line;
		if (allValue.line == 2)
			allValue.line = 9;
	}
	else
	{
		++allValue.line;
		if (allValue.line == 10)
			allValue.line = 3;
	}
	SetPosition(allValue.line);
	cout << '[' << allValue.line - 2 << ']';
}

void GetKey(void)
{
	allValue.line = 3;
	SetPosition(allValue.line);
	cout << '[' << allValue.line - 2 << ']';
	bool select = true;
	int inputKeyValue;
	while (select)
	{
		inputKeyValue = _getch();
		if (GetAsyncKeyState(VK_UP) < 0)
		{
			while (GetAsyncKeyState(VK_UP) < 0)
				1;
			Print(true);
		}
		if (GetAsyncKeyState(VK_DOWN) < 0)
		{
			while (GetAsyncKeyState(VK_DOWN) < 0)
				1;
			Print(false);
		}
		if (inputKeyValue == 13)
		{
			while (_kbhit())
				1;
			select = false;
		}
	}
}

void Output(bool isWindows7, bool isDefault, bool isAll)
{
	system("cls");
	char inputTH[3];
	if (isAll == false)
	{
		cout << "THxx:";
		cin.getline(inputTH, 3);
	}
	if (isWindows7&&isDefault)
	{
		ofstream fout("TPTI Ver.I Temporary File Start.bat");
		fout << "runas /user:administrator TPIT Ver.I Temporary File.bat";
		fout.close();
	}
	string output;
	output = "@echo on\nset tpitpath=%~dp0\n";
	if (isDefault == false)
		output += "set /p installpath = Install path:\n";
	output += "xcopy %tpitpath%Touhou";
	if (isAll == false)
		output = output + "TH" + inputTH[0] + inputTH[1];
	if (isWindows7&&isDefault)
		output += " c:\\user\\administrator\\desktop";
	if (isWindows7 == false && isDefault)
		output += " c:\\documents and settings\\administrator\\desktop";
	if (isDefault == false)
		output += " %installpath%";
	output += " /w /f /s /k /e\n";
	if (isAll==false&&( inputTH[0]=='1'&& (inputTH[1]=='4'|| inputTH[1]=='5')))
	{
		output += "xcopy %tptipath%Touhou\\ShanghaiAlice\\TH";
		output = output + '1' + inputTH[1] + " C:\\Users\\Administrator\\AppData\\Roaming\\ShanghaiAlice /w /f /s /k /e\n";
	}
	output += "exit";
	ofstream fout("TPTI Ver.I Temporary File.bat");
	char *dst = new char[255];
	int i;
	for (i = 0;i <= output.length();i++)
		dst[i] = output[i];
	dst[i] = '\0';
	fout << dst;
	fout.close();
}
int main()
{
    return 0;
}

