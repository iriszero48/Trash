// ReSharper disable CommentTypo
#include <cstdio>
#include <vector>
#include <string>

using Row = array<int>;
using Map = array<Row^>;
using Maps = array<Map^>;

// ReSharper disable once CppInconsistentNaming
using Row_ = std::vector<int>;
// ReSharper disable once CppInconsistentNaming
using Map_ = std::vector<Row_>;
// ReSharper disable once CppInconsistentNaming
using Maps_ = std::vector<Map_>;

// ReSharper disable once CppDeclaratorNeverUsed
static Maps^ ToMaps(const Maps_& maps)
{
	auto res = gcnew Maps(static_cast<int>(maps.size()));
	for (auto i = 0; i < maps.size(); ++i)
	{
		res[i] = gcnew Map(static_cast<int>(maps[i].size()));
		for (auto j = 0; j < maps[i].size(); ++j)
		{
			res[i][j] = gcnew Row(static_cast<int>(maps[i][j].size()));
			for (auto k = 0; k < maps[i][j].size(); ++k)
			{
				res[i][j][k] = maps[i][j][k];
			}
		}
	}
	return res;
}

int main(const int argc, char* argv[])
{
//#define Debug
#ifndef Debug
	try
	{
		if (argc != 9)
		{
			throw gcnew System::Exception("parameter error");
		}

		auto parameter = gcnew System::Collections::Generic::Dictionary<System::String^, System::String^>();

		for (auto i = 1; i < argc; i += 2)
		{
			parameter->Add(gcnew System::String(argv[i]), gcnew System::String(argv[i + 1]));
		}

		Sudoku sudoku(System::Int32::Parse(parameter["-m"]), System::Int32::Parse(parameter["-n"]));
		sudoku.InitializeWithFilePath(parameter["-i"]);
		System::IO::File::WriteAllText(parameter["-o"], Sudoku::ToString(sudoku.Solve()));
	}
	catch (System::Exception^ exception)
	{
		fprintf(stderr, "Usage:\n  %s -m 宫格阶级 -n 待解答盘面数目 -i 指定输入文件 -o 指定程序的输出文件\n", argv[0]);
		System::Console::Error->WriteLine(exception->ToString());
		return EXIT_FAILURE;
	}
#else
	Sudoku sudoku(9, 1);
	//sudoku.InitializeWithArray(ToMaps(
	//{
	//	{
	//		{0,0,0, 0,0,0, 0,0,0},
	//		{0,0,0, 0,0,3, 0,8,5},
	//		{0,0,1, 0,2,0, 0,0,0},
	//			    	   
	//		{0,0,0, 5,0,7, 0,0,0},
	//		{0,0,4, 0,0,0, 1,0,0},
	//		{0,9,0, 0,0,0, 0,0,0},
	//			    	   
	//		{5,0,0, 0,0,0, 0,7,3},
	//		{0,0,2, 0,1,0, 0,0,0},
	//		{0,0,0, 0,4,0, 0,0,9}
	//	}
	//}));
	//sudoku.InitializeWithArray(ToMaps(
	//	{
	//		{
	//			{1,2,3},
	//			{2,3,0},
	//			{3,1,2}
	//		}
	//	}));
	//sudoku.InitializeWithArray(ToMaps(
	//	{
	//		{
	//			{0,0,6,5,0,0},
	//			{5,0,4,0,0,1},
	//			{0,3,0,1,2,0},
	//			{0,0,0,0,0,6},
	//			{1,0,0,0,0,0},
	//			{0,0,0,0,0,0}
	//		}
	//	}));
	Maps_ mp
	{
		{
			{ 2,0,0 },
			{ 0,0,1 },
			{ 0,0,0 }
		},
		{
			{ 0,0,1 },
			{ 0,0,0 },
			{ 2,0,0 }
		},
		{
			{ 0,0,0 },
			{ 0,0,0 },
			{ 3,3,0 }
		}
	};
	sudoku.InitializeWithArray(ToMaps(mp));
	System::Console::WriteLine(Sudoku::ToString(sudoku.Solve()));
	system("pause");
	//sudoku.InitializeWithArray(ToMaps(
	//{
	//	{
	//		{0,0,5,3,0,0,0,0,0},
	//		{8,0,0,0,0,0,0,2,0},
	//		{0,7,0,0,1,0,5,0,0},
	//
	//		{4,0,0,0,0,5,3,0,0},
	//		{0,1,0,0,7,0,0,0,6},
	//		{0,0,3,2,0,0,0,8,0},
	//
	//		{0,6,0,5,0,0,0,0,9},
	//		{0,0,4,0,0,0,0,3,0},
	//		{0,0,0,0,0,9,7,0,0}
	//	}
	//}));
	//sudoku.InitializeWithArray(ToMaps(
	//{
	//	{
	//		{0,0,0, 0,0,0, 0,0,0},
	//		{0,0,0, 0,0,3, 0,8,5},
	//		{0,0,1, 0,2,0, 0,0,0},
	//			    	   
	//		{0,0,0, 5,0,7, 0,0,0},
	//		{0,0,4, 0,0,0, 1,0,0},
	//		{0,9,0, 0,0,0, 0,0,0},
	//			    	   
	//		{5,0,0, 0,0,0, 0,7,3},
	//		{0,0,2, 0,1,0, 0,0,0},
	//		{0,0,0, 0,4,0, 0,0,9}
	//	}
	//}));
	//sudoku.InitializeWithFilePath("z:\\input.txt");
	//auto res = sudoku.Solve();
	//System::Console::WriteLine(Sudoku::ToString(res));
	//for (int i = 0; i < res->Length; ++i)
	//{
	//	if (res[i] == nullptr)
	//	{
	//		printf("\nNo solve\n");
	//		continue;
	//	}
	//	for (int row = 0; row < res[i]->Length; ++row)
	//	{
	//		for (int col = 0; col < res[i][row]->Length; ++col)
	//		{
	//			printf("%d", res[i][row][col]);
	//			if ((col + 1) % 3 == 0)
	//			{
	//				printf("|");
	//			}
	//		}
	//		printf("\n");
	//		if ((row + 1) % 3 == 0)
	//		{
	//			printf("---+---+---\n");
	//		}
	//	}
	//	printf("\n");
	//}
	//system("pause");
#endif
}
