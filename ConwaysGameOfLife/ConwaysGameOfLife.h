#pragma once

#include "WindowsApp.h"

#include <vector>

enum class CellStatus { Dead, Alive };

class ConwayGameOfLife final : public WindowsApp
{
	const static int NumRows = 108;
	const static int NumCols = 192;
	const static int SizePerCell = 10;
	const static int Width = NumCols * SizePerCell;
	const static int Height = NumRows * SizePerCell;

	using Row = std::vector<CellStatus>;
	using Map = std::vector<Row>;

	Map map = Map(NumRows, Row(NumCols, CellStatus::Dead));
	HBITMAP bmp{};
	HDC cdc{};
	bool pause = false;
	void* pBits = nullptr;
	int32_t fpsLimit = 60;

	void SetCell(int row, int col, CellStatus status);

	static CellStatus GetCell(const Map& map, int row, int col);

	static int GetCount(const Map& map, int row, int col);

	static CellStatus DeadOrAlive(const Map& map, int row, int col);

	static void DrawAliveCell(HDC dc, int x, int y);

	void InitializeBmp();
	void Update();
	void Draw();

public:
	explicit ConwayGameOfLife(HINSTANCE hInstance);

	void GenerateGosperGliderGun(int offsetRow, int offsetCol);
	void Initialize() override;
	void Run() override;
	LRESULT WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) override;
};
