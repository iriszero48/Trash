#pragma once
#include "pch.h"

public ref class Sudoku sealed
{
public:
	using Map = array<array<int>^>;
	using Maps = array<Map^>;
	
	Sudoku(int m, int n);

	void InitializeWithFilePath(System::String^ path);
	void InitializeWithArray(Maps^ maps);
	Maps^ Solve();

	static System::String^ ToString(Maps^ maps);

private:
	using CspTerm = Microsoft::SolverFoundation::Solvers::CspTerm;

	Maps^ maps;
	int m;
	int n;
	
	static array<CspTerm^>^ GetSlice(array<array<CspTerm^>^>^ lp, int m, int ra, int rb, int ca, int cb);
};


