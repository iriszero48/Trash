#include <iostream>
#include <fstream>
#include <string>
#include <thread>
#include <chrono>

std::string username{};
std::string password{};

std::string filename = "admin.txt";

std::string U;
std::string P;

bool Init = false;

bool sub_DFA()
{
    if (!Init)
    {
        std::fstream admin(filename, std::ios::in | std::ios::out);
        char buf[0x101] = { 0 };
        admin.read(buf, 0x100);
        U = std::string(buf);
        admin.read(buf, 0x100);
        P = std::string(buf);
        admin.close();
        Init = true;
    }

    std::cout << "input your user name\n>";
    std::cin >> username;
    std::cout << "input your password\n>";
    std::cin >> password;

    return username == U && password == P;
}

void sub_153C()
{
    using namespace std;
    std::cout << "welcome back " << username << "\n";
    while (true)
    {
#define fuck(x) else if((x) == input)
        std::string input;
        std::cin >> input;
        std::fstream fs;
        if (input == "ls")
        {
            system("ls /proc/self/");
        }
        fuck("open")
        {
            puts("input your filename");
            putchar('>');
            std::string fn{};
            std::cin >> fn;
            try {
                fs = fstream(fn, ios::in | ios::out);
            }
            catch (...)
            {
                cout << "open "<< fn<<" error";
                exit(0);
            }
        }
        fuck("readonly")
        {
            fs.close();
        }
        fuck("set_lseek")
        {
            puts("input you file offset");
            putchar('>');
            int offset;
            cin >> offset;
            fs.seekg(offset);
        }
        fuck("write")
        {
            puts("input your context");
            putchar('>');
            string con{};
            cin >> con;
            fs << con;
            puts("write success");
        }
        fuck("close")
        {
            fs.close();
        }
        fuck("exit")
        {
            exit(0);
        }
        fuck("help")
        {
            puts("ls:                list file");
            puts("open:              open file");
            puts("readonly:      readonly file");
            puts("set_lseek:set lseek for file");
            puts("write:            write file");
            puts("close:            close file");
            puts("exit:                 logout");
            puts("help:show the help for shell");
        }
        else
        {
            printf("command not found: %s", username.c_str());
            puts("if you don't know how to use the shell,please input help to show the help for shell");
        }
    }
}

bool sub_177C(char a1)
{
    return a1 > 96 && a1 <= 122 || a1 > 47 && a1 <= 57 || a1 > 64 && a1 <= 90;
}

void sub_1846()
{
    std::cout << "detected brute force attack\n";
    std::ifstream rd;
    try
    {
        rd = std::ifstream("/dev/urandom", std::ios::in);
    }
    catch (const std::exception& e)
    {
        std::cout << "open /dev/urandom error\n";
        exit(0);
    }

    std::fstream admin(filename, std::ios::in | std::ios::out);

    for (int i = 0; i <= 511;)
    {
        char buf[2] = { 0 };
        rd.read(buf, 1);
        if (sub_177C(buf[0]))
        {
            admin.write(buf, 1);
            ++i;
        }
    }
    rd.close();
    admin.close();
    
    for (int i = 0; i <= 4; ++i)
    {
        std::cout << "refuse connect\n" << "The remaining time is " << 5 - i << "s\n";
        using std::chrono::operator ""s;
        std::this_thread::sleep_for(+1s);
    }
}

int main()
{
    try
    {
        std::fstream admin(filename, std::ios::in | std::ios::out);
        char buf[0x101] = { 0 };
        admin.read(buf, 0x100);
        U = std::string(buf);
        admin.read(buf, 0x100);
        P = std::string(buf);
        admin.close();
    }
    catch (const std::exception& e)
    {
        std::cout << "fopen " << filename << " error" << "please contact us";
        exit(0);
    }
    int v4 = 3;
    int v5 = 0;
    while (v4)
    {
        if (sub_DFA())
            sub_153C();
        std::cout << "Login failed\n";
        if (!--v4 && !v5)
        {
            sub_1846();
            v4 = 3;
            v5 = 1;
        }
    }
    return 0LL;
}
