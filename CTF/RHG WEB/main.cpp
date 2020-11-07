#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include <regex>
using namespace std;

int main(int argc, char* argv[])
{/*
	system("python3 GetUrl.py");
	ifstream fr1;
	fr1.open("weburl.txt");
	vector<string> urlList;
	string temp;
	while (getline(fr1, temp)) urlList.push_back(temp);
	fr1.close();
	ifstream fr2;
	fr2.open("fuck.txt");
	vector<string> fuckList;
	while (getline(fr2,temp))fuckList.push_back(temp);
	fr2.close();*/
	system("python3 hb.py&");
	//system("python3 fix.py&");
	while (true)
	{
	system("python3 GetUrl.py");
        ifstream fr1;
        fr1.open("weburl.txt");
        vector<string> urlList;
        string temp;
        while (getline(fr1, temp)) urlList.push_back(temp);
        fr1.close();
        ifstream fr2;
        fr2.open("fuck.txt");
        vector<string> fuckList;
        while (getline(fr2,temp))fuckList.push_back(temp);
        fr2.close();
	system("python3 fix.py&");

		for_each(fuckList.begin(), fuckList.end(), [&](string& fuck)
		{
			for_each(urlList.begin(), urlList.end(), [&](string& url)
			{
				system((regex_replace(fuck, regex("\\$url\\$"), url) + "&").c_str());
			});
		});
		system("sleep 5");
	}
}
