using namespace System;
int main(array<System::String ^> ^args)
{
	int count = Convert::ToInt32(Console::ReadLine());
	String^ maxn, ^minn;
	double maxp = 0, minp = 1000000;
	for (int i = 0; i < count; ++i)
	{
		String^ inn = Console::ReadLine();
		double inp = Convert::ToDouble(Console::ReadLine());
		if (inp>maxp)
		{
			maxp = inp;
			maxn = inn;
		}
		if (inp<minp)
		{
			minp = inp;
			minn = inn;
		}
	}
	Console::WriteLine(String::Format(L"{0:F}", maxp) + ", " + maxn);
	Console::WriteLine(String::Format(L"{0:F}", minp) + ", " + minn);
    return 0;
}
