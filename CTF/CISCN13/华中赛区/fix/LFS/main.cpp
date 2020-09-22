#include <cstdlib>
#include <iostream>

void GetFlag(const std::string& src)
{
    std::cout << "The flag is your log:" << src << "\n";
}

void Print()
{
    std::cout << "Printing......\n";
}

void Display(const std::string& s)
{
    std::cout << s << "\n";
}

void AddLog(std::string& a1)
{
	std::cout << "Please input new log info:";
    std::cin >> a1;
}

int main()
{
    std::string src{}; // [esp+4h] [ebp-FCh]
    std::string s1{}; // [esp+84h] [ebp-7Ch]

    std::cout << "Welcome to use LFS.\n" << "Please input admin password:";
    std::cin >> s1;
    if (s1 != "administrator")
    {
        std::cout << "Password Error!\n";
        exit(0);
    }
    std::cout << "Welcome!\n";
    while (true)
    {
        std::cout << "Input your operation:\n";
        std::cout << "1.Add a log.\n";
        std::cout << "2.Display all logs.\n";
        std::cout << "3.Print all logs.\n";
		std::cout << "0.Exit\n:";
        auto a = 0;
        std::cin >> a;
        switch (a)
        {
        case 0:
            exit(0);
        case 1:
            AddLog(src);
            break;
        case 2:
            Display(src);
            break;
        case 3:
            Print();
            break;
        case 4:
            GetFlag(src);
            break;
        default: ;
        }
    }
}
