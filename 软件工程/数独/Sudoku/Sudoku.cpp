#include "pch.h"
#include "Sudoku.h"

static int M = 0;

ref class Func sealed
{
public:
	static array<array<int>^>^ ChunkBySize(array<int>^ arr)
	{
		return Microsoft::FSharp::Collections::ArrayModule::ChunkBySize(M, arr);
	}
};

Sudoku::Sudoku(const int m, const int n) :m(m), n(n)
{
	if (m < 3 || m > 9)
	{
		throw gcnew System::Exception("M must be 3 to 9");
	}
	if (n < 0)
	{
		throw gcnew System::Exception("N must be greater than -1");
	}
}

void Sudoku::InitializeWithFilePath(System::String^ path)
{
	using namespace Microsoft::FSharp::Core;
	using ArrayModule = Microsoft::FSharp::Collections::ArrayModule;
	const auto mapSp = gcnew array<System::String^>(1) { " " };

	M = m;
	maps =
		ArrayModule::Take(
			n,
			ArrayModule::Map(
				FSharpFuncUtil::FSharpFuncUtil::ToFSharpFunc(gcnew System::Func<array<int>^, array<array<int>^>^>(Func::ChunkBySize)),
				ArrayModule::ChunkBySize(
					m * m,
					ArrayModule::Map(
						FSharpFuncUtil::FSharpFuncUtil::ToFSharpFunc(gcnew System::Func<System::String^, int>(System::Int32::Parse)),
						System::IO::File::ReadAllText(path)
							->Replace("\n", " ")
							->Replace("\r", " ")
							->Split(mapSp, System::StringSplitOptions::RemoveEmptyEntries)))));
if (maps->Length != n)
{
	throw gcnew System::Exception("N size no matches");
}
for (auto i = 0; i < maps->Length; ++i)
{
	if (maps[i]->Length != m)
	{
		throw gcnew System::Exception("M size no matches");
	}
	for (auto j = 0; j < m; ++j)
	{
		if (maps[i][j]->Length != m)
		{
			throw gcnew System::Exception("M size no matches");
		}
	}
}
}

void Sudoku::InitializeWithArray(Maps^ maps)
{
	this->maps = maps;
}

Sudoku::Maps^ Sudoku::Solve()
{
	using namespace Microsoft::SolverFoundation::Solvers;

	// 创建一个三维数组存放结果
	auto res = gcnew Maps(maps->Length);
	for (auto i = 0; i < maps->Length; ++i)
	{
		res[i] = gcnew Map(maps[i]->Length);
		for (auto j = 0; j < maps[i]->Length; ++j)
		{
			res[i][j] = gcnew array<int>(maps[i][j]->Length);
		}
	}

	// 并行遍历每个盘面，同时调度两个线程
	// #pragma omp parallel for schedule(dynamic, 2)
	for (auto i = 0; i < maps->Length; ++i)
	{
		// 初始化
		auto map = maps[i];
		auto s = ConstraintSystem::CreateSolver();
		const auto z = s->CreateIntegerInterval(1, maps[i]->Length);
		auto lp = s->CreateVariableArray(z, "n", maps[i]->Length, maps[i]->Length);

		// 为每行和已知条件添加约束
		for (auto row = 0; row < maps[i]->Length; ++row)
		{
			for (auto col = 0; col < maps[i]->Length; ++col)
			{
				if (map[row][col] > 0)
				{
					s->AddConstraints(s->Equal(map[row][col], lp[row][col]));
				}
			}
			s->AddConstraints(s->Unequal(GetSlice(lp, maps[i]->Length, row, row, 0, maps[i]->Length - 1)));
		}
		
		// 为每列添加约束
		for (auto col = 0; col < maps[i]->Length; ++col)
		{
			s->AddConstraints(s->Unequal(GetSlice(lp, maps[i]->Length, 0, maps[i]->Length - 1, col, col)));
		}

		// 为不同盘面阶数设置宫格大小
		auto stepRow = 0;
		auto stepCol = 0;
		switch (maps[i]->Length)
		{
		case 4:
			stepRow = 2;
			stepCol = 2;
			break;
		case 6:
			stepRow = 2;
			stepCol = 3;
			break;
		case 8:
			stepRow = 4;
			stepCol = 2;
			break;
		case 9:
			stepRow = 3;
			stepCol = 3;
			break;
		default:;
		}

		// 如果当前盘面阶数存在宫格，则为每个宫格添加约束
		if (stepRow != 0 && stepCol != 0)
		{
			for (auto row = 0; row < maps[i]->Length; row += stepRow)
			{
				for (auto col = 0; col < maps[i]->Length; col += stepCol)
				{
					s->AddConstraints(
						s->Unequal(
							GetSlice(lp, stepCol * stepRow, row, row + stepRow - 1, col, col + stepCol - 1)));
				}
			}
		}

		// 求解
		auto sol = s->Solve();
		try
		{
			for (auto row = 0; row < maps[i]->Length; ++row)
			{
				for (auto col = 0; col < maps[i]->Length; ++col)
				{
					res[i][row][col] = sol->GetIntegerValue(lp[row][col]);
				}
			}
		}
		catch (System::Exception^) // 无解情况返回nullptr
		{
			res[i] = nullptr;
		}
	}
	return res;
}

System::String^ Sudoku::ToString(Maps^ maps)
{
	auto sb = gcnew System::Text::StringBuilder();
	for (auto i = 0; i < maps->Length; ++i)
	{
		if (maps[i] == nullptr)
		{
			sb->Append("Unsolvable!\n\n");
			continue;
		}
		for (auto j = 0; j < maps[i]->Length; ++j)
		{
			for (auto k = 0; k < maps[i][j]->Length; ++k)
			{
				sb->Append(maps[i][j][k].ToString());
				if (k < maps[i][j]->Length - 1)
				{
					sb->Append(" ");
				}
			}
			sb->Append("\n");
		}
		if (i < maps->Length - 1)
		{
			sb->Append("\n");
		}
	}
	return sb->ToString();
}

array<Sudoku::CspTerm^>^ Sudoku::GetSlice(
	array<array<CspTerm^>^>^ lp,
	const int m,
	const int ra,
	const int rb,
	const int ca,
	const int cb)
{
	auto slice = gcnew array<CspTerm^>(m);
	auto i = 0;
	for (auto row = ra; row <= rb; ++row)
	{
		for (auto col = ca; col <= cb; ++col)
		{
			slice[i++] = lp[row][col];
		}
	}
	return slice;
}

