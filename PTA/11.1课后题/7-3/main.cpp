#include <iostream>  
#include<string>
using namespace std;
int main()
{
	int count;
	cin >> count;
	string id, name, out_id, out_name;
	int sum = 0;
	for (int gr1, gr2, gr3, i = 0; i < count; ++i)
	{
		cin >> id >> name >> gr1 >> gr2 >> gr3;
		if (gr1 + gr2 + gr3 > sum)
		{
			out_id = id;
			out_name = name;
			sum = gr1 + gr2 + gr3;
		}
	}
	cout << out_name + " " << out_id + " " << sum;
	return 0;
}
