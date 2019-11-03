#include "stdafx.h"

namespace Fuck
{
	public ref class File
	{
	public:
		System::String^ User = "";
		System::Collections::Generic::List<System::Char>^ Data = gcnew System::Collections::Generic::List<System::Char>();
		System::String^ Path = "";
		int index;
		File(System::String^ path)
		{
			Path = path;
		}
		File(System::String^ path, System::String^ data)
		{
			Path = path;
			index = 0;
			for (auto i = 0; i < data->Length; ++i) Data->Add(data[i]);
		}
		bool Open(System::String^ user)
		{
			if (User == "")
			{
				User = user;
				index = 0;
				return true;
			}
			System::Console::WriteLine("E: Could not open file " + Path);
			return false;
		}
		void Close(System::String^ user)
		{
			if (User == user) User = "";
			else System::Console::WriteLine("E: Could not close file " + Path);
		}
		void Read(System::String^ user, const int offset)
		{
			if (User == user)
			{
				for (auto i = index; index < Data->Count && i < offset; ++index, ++i) System::Console::Write(Data[index]);
			}
			else
			{
				System::Console::Write("E: Could not read file " + Path);
			}
			System::Console::WriteLine();
		}
		void Write(System::String^ user, System::String^ data)
		{
			if (User == user)
			{
				for (auto i = 0; i < data->Length; ++i, ++index)
				{
					if (index < Data->Count) Data[index] = data[i];
					else Data->Add(data[i]);
				}
			}
			else
			{
				System::Console::WriteLine("E: Could not write file " + Path);
			}
		}
	};
}
