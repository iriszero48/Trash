using namespace Microsoft::VisualStudio::TestTools::UnitTesting;

#include <vector>

[TestClass]
public ref class SudokuTests sealed
{
	using Row = array<int>;
	using Map = array<Row^>;
	using Maps = array<Map^>;

	// ReSharper disable once CppInconsistentNaming
	using Row_ = std::vector<int>;
	// ReSharper disable once CppInconsistentNaming
	using Map_ = std::vector<Row_>;
	// ReSharper disable once CppInconsistentNaming
	using Maps_ = std::vector<Map_>;
	
	static Maps^ ToMaps(const Maps_& maps)
	{
#pragma warning(disable:4267)
		auto res = gcnew Maps(static_cast<int>(maps.size()));
		for (size_t i = 0; i < maps.size(); ++i)
		{
			res[i] = gcnew Map(static_cast<int>(maps[i].size()));
			for (size_t j = 0; j < maps[i].size(); ++j)
			{
				res[i][j] = gcnew Row(static_cast<int>(maps[i][j].size()));
				for (size_t k = 0; k < maps[i][j].size(); ++k)
				{
					res[i][j][k] = maps[i][j][k];
				}
			}
		}
		return res;
	}
public:
	[TestMethod]
	void Sudoku3X3()
	{
		const Maps_ mp
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
				{ 1,1,0 },
				{ 0,0,0 },
				{ 0,0,0 }
			}
		};

		auto s = gcnew Sudoku(3, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(2 1 3
3 2 1
1 3 2

3 2 1
1 3 2
2 1 3

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void Sudoku4X4()
	{
		const Maps_ mp
		{
			{
				{ 0,0, 2,0 },
				{ 0,0, 0,4 },
				
				{ 3,0, 0,1 },
				{ 0,1, 0,0 }
			},		   
			{		   
				{ 2,0, 0,0 },
				{ 0,1, 0,0 },
				
				{ 0,2, 3,0 },
				{ 0,0, 0,4 }
			},		   
			{		   
				{ 1,1, 0,0 },
				{ 0,0, 0,0 },
				
				{ 0,0, 0,0 },
				{ 0,0, 0,0 }
			}
		};

		auto s = gcnew Sudoku(4, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(1 4 2 3
2 3 1 4
3 2 4 1
4 1 3 2

2 4 1 3
3 1 4 2
4 2 3 1
1 3 2 4

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void Sudoku5X5()
	{
		const Maps_ mp
		{
			{
				{ 0,1,0,0,0 },
				{ 0,0,0,3,0 },
				{ 0,0,4,1,2 },
				{ 1,0,0,5,0 },
				{ 3,0,0,0,4 }
			},
			{
				{ 3,0,2,0,0 },
				{ 4,2,0,0,0 },
				{ 0,0,1,0,5 },
				{ 0,0,3,0,1 },
				{ 0,0,0,0,0 }
			},
			{
				{ 1,1,0,0,0 },
				{ 0,0,0,0,0 },
				{ 0,0,0,0,0 },
				{ 0,0,0,0,0 },
				{ 0,0,0,0,0 }
			}
		};

		auto s = gcnew Sudoku(5, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(2 1 3 4 5
4 2 5 3 1
5 3 4 1 2
1 4 2 5 3
3 5 1 2 4

3 1 2 5 4
4 2 5 1 3
2 3 1 4 5
5 4 3 2 1
1 5 4 3 2

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void Sudoku6X6()
	{
		const Maps_ mp
		{
			{
				{ 5,0,0, 0,4,0 },
				{ 0,0,0, 0,0,3 },
				
				{ 0,6,4, 2,0,0 },
				{ 2,0,0, 0,6,1 },
				
				{ 6,0,0, 3,0,0 },
				{ 0,0,2, 6,0,4 }
			},			 
			{			 
				{ 0,0,0, 2,0,0 },
				{ 2,0,0, 0,3,0 },
				
				{ 0,5,0, 3,0,2 },
				{ 3,2,6, 0,1,0 },
				
				{ 0,0,1, 0,0,3 },
				{ 0,0,0, 5,4,0 }
			},			 
			{			 
				{ 1,1,0, 0,0,0 },
				{ 0,0,0, 0,0,0 },
				
				{ 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0 },
				
				{ 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0 }
			}
		};

		auto s = gcnew Sudoku(6, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(5 2 3 1 4 6
4 1 6 5 2 3
1 6 4 2 3 5
2 3 5 4 6 1
6 4 1 3 5 2
3 5 2 6 1 4

4 1 3 2 5 6
2 6 5 1 3 4
1 5 4 3 6 2
3 2 6 4 1 5
5 4 1 6 2 3
6 3 2 5 4 1

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void Sudoku7X7()
	{
		const Maps_ mp
		{
				{
					{ 1,0,0,0,0,0,0 },
					{ 0,0,2,0,0,0,0 },
					{ 0,0,0,0,0,5,0 },
					{ 0,3,0,4,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,6,0 },
					{ 0,0,0,0,0,0,7 }
				},
				{
					{ 1,0,0,0,6,0,5 },
					{ 6,2,0,3,0,1,0 },
					{ 0,4,0,0,0,0,0 },
					{ 0,6,5,0,0,2,0 },
					{ 5,0,1,0,4,0,7 },
					{ 7,0,0,0,0,0,2 },
					{ 0,0,6,0,0,0,0 }
				},
				{
					{ 1,1,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 },
					{ 0,0,0,0,0,0,0 }
				}
		};

		auto s = gcnew Sudoku(7, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(1 5 3 6 2 7 4
5 6 2 7 1 4 3
2 1 4 3 7 5 6
7 3 6 4 5 1 2
4 2 7 1 6 3 5
3 7 5 2 4 6 1
6 4 1 5 3 2 7

1 7 2 4 6 3 5
6 2 7 3 5 1 4
2 4 3 5 1 7 6
4 6 5 1 7 2 3
5 3 1 2 4 6 7
7 1 4 6 3 5 2
3 5 6 7 2 4 1

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void Sudoku8X8()
	{
		const Maps_ mp
		{
			{
				{ 8,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 6,0 },
				{ 0,0, 0,3, 0,0, 0,0 },
				{ 0,0, 5,0, 0,0, 0,0 },

				{ 0,0, 0,0, 0,4, 0,0 },
				{ 0,0, 0,0, 2,0, 0,0 },
				{ 0,7, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,1 }
			},
			{
				{ 0,5, 0,0, 0,7, 4,0 },
				{ 0,0, 4,3, 2,0, 0,0 },
				{ 0,0, 1,0, 0,0, 0,0 },
				{ 7,0, 0,0, 4,0, 0,0 },

				{ 0,0, 0,0, 0,5, 0,2 },
				{ 0,0, 0,0, 6,0, 0,0 },
				{ 4,0, 3,0, 1,0, 0,0 },
				{ 6,0, 0,8, 0,0, 3,0 }
			},
			{
				{ 1,1, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 },
				
				{ 0,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 },
				{ 0,0, 0,0, 0,0, 0,0 }
			}
		};

		auto s = gcnew Sudoku(8, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(8 4 6 1 3 2 7 5
1 5 4 2 7 8 6 3
6 2 7 3 1 5 8 4
7 3 5 8 4 6 1 2
5 1 3 6 8 4 2 7
4 8 1 5 2 7 3 6
3 7 2 4 6 1 5 8
2 6 8 7 5 3 4 1

2 5 8 6 3 7 4 1
1 6 4 3 2 8 5 7
8 4 1 7 5 6 2 3
7 3 5 2 4 1 6 8
3 7 6 4 8 5 1 2
5 2 7 1 6 3 8 4
4 8 3 5 1 2 7 6
6 1 2 8 7 4 3 5

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}
	
	[TestMethod]
	void Sudoku9X9()
	{
		const Maps_ mp
		{
			{
				{ 0,0,5, 3,0,0, 0,0,0 },
				{ 8,0,0, 0,0,0, 0,2,0 },
				{ 0,7,0, 0,1,0, 5,0,0 },

				{ 4,0,0, 0,0,5, 3,0,0 },
				{ 0,1,0, 0,7,0, 0,0,6 },
				{ 0,0,3, 2,0,0, 0,8,0 },

				{ 0,6,0, 5,0,0, 0,0,9 },
				{ 0,0,4, 0,0,0, 0,3,0 },
				{ 0,0,0, 0,0,9, 7,0,0 }
			},
			{
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,3, 0,8,5 },
				{ 0,0,1, 0,2,0, 0,0,0 },

				{ 0,0,0, 5,0,7, 0,0,0 },
				{ 0,0,4, 0,0,0, 1,0,0 },
				{ 0,9,0, 0,0,0, 0,0,0 },

				{ 5,0,0, 0,0,0, 0,7,3 },
				{ 0,0,2, 0,1,0, 0,0,0 },
				{ 0,0,0, 0,4,0, 0,0,9 }
			},
			{
				{ 1,1,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 }
			}
		};

		auto s = gcnew Sudoku(9, 3);
		s->InitializeWithArray(ToMaps(mp));
		Assert::AreEqual(R"(1 4 5 3 2 7 6 9 8
8 3 9 6 5 4 1 2 7
6 7 2 9 1 8 5 4 3
4 9 6 1 8 5 3 7 2
2 1 8 4 7 3 9 5 6
7 5 3 2 9 6 4 8 1
3 6 7 5 4 2 8 1 9
9 8 4 7 6 1 2 3 5
5 2 1 8 3 9 7 6 4

9 8 7 6 5 4 3 2 1
2 4 6 1 7 3 9 8 5
3 5 1 9 2 8 7 4 6
1 2 8 5 3 7 6 9 4
6 3 4 8 9 2 1 5 7
7 9 5 4 6 1 8 3 2
5 1 9 2 8 6 4 7 3
4 7 2 3 1 9 5 6 8
8 6 3 7 4 5 2 1 9

Unsolvable!

)", Sudoku::ToString(s->Solve()));
	}

	[TestMethod]
	void FileInput()
	{
		const Maps_ mp
		{
			{
				{ 0,0,5, 3,0,0, 0,0,0 },
				{ 8,0,0, 0,0,0, 0,2,0 },
				{ 0,7,0, 0,1,0, 5,0,0 },

				{ 4,0,0, 0,0,5, 3,0,0 },
				{ 0,1,0, 0,7,0, 0,0,6 },
				{ 0,0,3, 2,0,0, 0,8,0 },

				{ 0,6,0, 5,0,0, 0,0,9 },
				{ 0,0,4, 0,0,0, 0,3,0 },
				{ 0,0,0, 0,0,9, 7,0,0 }
			},
			{
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,3, 0,8,5 },
				{ 0,0,1, 0,2,0, 0,0,0 },

				{ 0,0,0, 5,0,7, 0,0,0 },
				{ 0,0,4, 0,0,0, 1,0,0 },
				{ 0,9,0, 0,0,0, 0,0,0 },

				{ 5,0,0, 0,0,0, 0,7,3 },
				{ 0,0,2, 0,1,0, 0,0,0 },
				{ 0,0,0, 0,4,0, 0,0,9 }
			},
			{
				{ 1,1,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },

				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },

				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 },
				{ 0,0,0, 0,0,0, 0,0,0 }
			}
		};
		const auto filename = gcnew System::String("FileInput.txt");
		System::IO::File::WriteAllText(filename, Sudoku::ToString(ToMaps(mp)));
		auto sudokuArray = gcnew Sudoku(9, 3);
		sudokuArray->InitializeWithArray(ToMaps(mp));
		auto sudokuFile = gcnew Sudoku(9, 3);
		sudokuFile->InitializeWithFilePath(filename);
		Assert::AreEqual(Sudoku::ToString(sudokuArray->Solve()), Sudoku::ToString(sudokuFile->Solve()));
		System::IO::File::Delete(filename);
	}

	[TestMethod]
	void MOutOfRange()
	{
		
		try
		{
			gcnew Sudoku(10, 1);
		}
		catch (System::Exception^ exception)
		{
			Assert::IsTrue(exception->ToString()->Contains("M must be 3 to 9"));
		}
		try
		{
			gcnew Sudoku(2, 1);
		}
		catch (System::Exception^ exception)
		{
			Assert::IsTrue(exception->ToString()->Contains("M must be 3 to 9"));
		}
	}

	[TestMethod]
	void NOutOfRange()
	{
		try
		{
			gcnew Sudoku(9, -1);
		}
		catch (System::Exception^ exception)
		{
			Assert::IsTrue(exception->ToString()->Contains("N must be greater than -1"));
		}
	}
	
	[TestMethod]
	void NSizeNoMatch()
	{
		const Maps_ mp
		{
			{
				{ 0,0,5, 3,0,0, 0,0,0 },
				{ 8,0,0, 0,0,0, 0,2,0 },
				{ 0,7,0, 0,1,0, 5,0,0 },

				{ 4,0,0, 0,0,5, 3,0,0 },
				{ 0,1,0, 0,7,0, 0,0,6 },
				{ 0,0,3, 2,0,0, 0,8,0 },

				{ 0,6,0, 5,0,0, 0,0,9 },
				{ 0,0,4, 0,0,0, 0,3,0 },
				{ 0,0,0, 0,0,9, 7,0,0 }
			}
		};
		const auto filename = gcnew System::String("FileInput.txt");
		System::IO::File::WriteAllText(filename, Sudoku::ToString(ToMaps(mp)));
		auto sudokuFile = gcnew Sudoku(9, 2);
		try
		{
			sudokuFile->InitializeWithFilePath(filename);
		}
		catch (System::Exception^ exception)
		{
			Assert::IsTrue(exception->ToString()->Contains("System.InvalidOperationException"));
		}
		
		System::IO::File::Delete(filename);
	}

	[TestMethod]
	void MSizeNoMatch()
	{
		const Maps_ mp
		{
			{
				{ 0,0,5, 3,0,0, 0,0,0 },
				{ 8,0,0, 0,0,0, 0,2,0 },
				{ 0,7,0, 0,1,0, 5,0,0 },

				{ 4,0,0, 0,0,5, 3,0,0 },
				{ 0,1,0, 0,7,0, 0,0,6 },
				{ 0,0,3, 2,0,0, 0,8,0 },

				{ 0,6,0, 5,0,0, 0,0,9 },
				{ 0,0,4, 0,0,0, 0,3,0 },
				{ 0,0,0, 0,0,9, 7,0,0 }
			}
		};
		const auto filename = gcnew System::String("FileInput.txt");
		System::IO::File::WriteAllText(filename, Sudoku::ToString(ToMaps(mp))->Substring(2));
		auto sudokuFile = gcnew Sudoku(9, 1);
		try
		{
			sudokuFile->InitializeWithFilePath(filename);
		}
		catch (System::Exception^ exception)
		{
			Assert::IsTrue(exception->ToString()->Contains("M size no matches"));
		}

		System::IO::File::Delete(filename);
	}
};
