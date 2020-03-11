#include "ConwaysGameOfLife.h"

#include <string>
#include <array>

void ConwayGameOfLife::SetCell(const int row, const int col, const CellStatus status)
{
	map.at((row + NumRows) % NumRows).at((col + NumCols) % NumCols) = status;
}

CellStatus ConwayGameOfLife::GetCell(const Map& map, const int row, const int col)
{
	return map.at((row + NumRows) % NumRows).at((col + NumCols) % NumCols);
}

int ConwayGameOfLife::GetCount(const Map& map, const int row, const int col)
{
	auto count = 0;
	std::array<int, 3> ops{ -1, 0, 1 };
	for (auto colOp : ops)
	{
		for (auto rowOp : ops)
		{
			if (rowOp || colOp)
			{
				count += static_cast<int>(GetCell(map, row + rowOp, col + colOp));
			}
		}
	}
	return count;
}

CellStatus ConwayGameOfLife::DeadOrAlive(const Map& map, const int row, const int col)
{
	const auto count = GetCount(map, row, col);
	return CellStatus(!static_cast<int>(map.at(row).at(col)) && count == 3
		|| static_cast<int>(map.at(row).at(col)) && (count == 2 || count == 3));
}

void ConwayGameOfLife::Update()
{
	const auto prevMap = map;
	for (auto row = 0; row < NumRows; row++)
	{
		for (auto col = 0; col < NumCols; col++)
		{
			map.at(row).at(col) = DeadOrAlive(prevMap, row, col);
		}
	}
}

void ConwayGameOfLife::Draw()
{
	memset(pBits, RGB(255, 255, 255), Width * Height * sizeof int32_t);
	for (auto row = 0; row < NumRows; row++)
	{
		for (auto col = 0; col < NumCols; col++)
		{
			if (static_cast<int>(map.at(row).at(col)))
			{
				DrawAliveCell(cdc, row, col);
			}
		}
	}
	const auto hdc = GetDC(hWnd);
	BitBlt(hdc, 0, 0, Width, Height, cdc, 0, 0, SRCCOPY);
	ReleaseDC(hWnd, hdc);
}

void ConwayGameOfLife::InitializeBmp()
{
	BITMAPINFO bi;
	ZeroMemory(&bi, sizeof bi);
	bi.bmiHeader.biSize = sizeof bi.bmiHeader;
	bi.bmiHeader.biBitCount = sizeof(DWORD) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes = 1;
	bi.bmiHeader.biWidth = Width;
	bi.bmiHeader.biHeight = Height;
	const auto hdc = GetDC(hWnd);
	bmp = CreateDIBSection(hdc, &bi, DIB_RGB_COLORS, &pBits, nullptr, 0);
	cdc = CreateCompatibleDC(hdc);
	SelectObject(cdc, bmp);
	ReleaseDC(hWnd, hdc);
}

// ReSharper disable once CppParameterMayBeConst
void ConwayGameOfLife::DrawAliveCell(HDC dc, const int x, const int y)
{
	RECT rc;
	SetRect(&rc, y * SizePerCell, x * SizePerCell, (y + 1) * SizePerCell, (x + 1) * SizePerCell);
	FillRect(dc, &rc,
		static_cast<HBRUSH>(GetStockObject(BLACK_BRUSH)));
}

// ReSharper disable once CppParameterMayBeConst
ConwayGameOfLife::ConwayGameOfLife(HINSTANCE hInstance) : WindowsApp(hInstance)
{
}

void ConwayGameOfLife::GenerateGosperGliderGun(const int offsetRow, const int offsetCol)
{
	SetCell(5 + offsetRow, 1 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 1 + offsetCol, CellStatus::Alive);
	SetCell(5 + offsetRow, 2 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 2 + offsetCol, CellStatus::Alive);
	SetCell(5 + offsetRow, 11 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 11 + offsetCol, CellStatus::Alive);
	SetCell(7 + offsetRow, 11 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 12 + offsetCol, CellStatus::Alive);
	SetCell(8 + offsetRow, 12 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 13 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 14 + offsetCol, CellStatus::Alive);
	SetCell(9 + offsetRow, 13 + offsetCol, CellStatus::Alive);
	SetCell(9 + offsetRow, 14 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 15 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 16 + offsetCol, CellStatus::Alive);
	SetCell(8 + offsetRow, 16 + offsetCol, CellStatus::Alive);
	SetCell(5 + offsetRow, 17 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 17 + offsetCol, CellStatus::Alive);
	SetCell(7 + offsetRow, 17 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 18 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 21 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 21 + offsetCol, CellStatus::Alive);
	SetCell(5 + offsetRow, 21 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 22 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 22 + offsetCol, CellStatus::Alive);
	SetCell(5 + offsetRow, 22 + offsetCol, CellStatus::Alive);
	SetCell(2 + offsetRow, 23 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 23 + offsetCol, CellStatus::Alive);
	SetCell(1 + offsetRow, 25 + offsetCol, CellStatus::Alive);
	SetCell(2 + offsetRow, 25 + offsetCol, CellStatus::Alive);
	SetCell(6 + offsetRow, 25 + offsetCol, CellStatus::Alive);
	SetCell(7 + offsetRow, 25 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 35 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 35 + offsetCol, CellStatus::Alive);
	SetCell(3 + offsetRow, 36 + offsetCol, CellStatus::Alive);
	SetCell(4 + offsetRow, 36 + offsetCol, CellStatus::Alive);
}

void ConwayGameOfLife::Initialize()
{
	RECT rect{ 0,0,Width,Height };
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, false);
	InitializeWindows(rect.right - rect.left, rect.bottom - rect.top, L"Conway's Game of Life");
	InitializeBmp();
	Draw();
}

void ConwayGameOfLife::Run()
{
	uint64_t countsPerSec;
	QueryPerformanceFrequency(reinterpret_cast<LARGE_INTEGER*>(&countsPerSec));
	const auto secondsPerCount = 1.0 / static_cast<double>(countsPerSec);
	
	uint64_t currTime = 0;
	QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER*>(&currTime));
	auto prevTime = currTime;
	
	uint64_t fpsLimitCurrTime = 0;
	auto fpsLimitPrevTime = fpsLimitCurrTime;
	
	MSG msg{};
	auto fps = 0;
	while (msg.message != WM_QUIT)
	{
		if (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE) != 0)
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		else
		{
			if (!pause)
			{
				QueryPerformanceCounter(reinterpret_cast<LARGE_INTEGER*>(&currTime));
				if (static_cast<double>(currTime - prevTime)* secondsPerCount >= 1.)
				{
					prevTime = currTime;
					SetWindowText(hWnd, 
						(std::to_wstring(fps) + L"fps" 
							+ L" | Current Fps Limit:" + std::to_wstring(fpsLimit) 
							+ L"fps | space:pause | +/-:Fps Limit+/-10 | *:Fps Unlimited").c_str());
					fps = 0;
				}
				fpsLimitCurrTime = currTime;
				if (static_cast<double>(fpsLimitCurrTime - fpsLimitPrevTime)* secondsPerCount >= 1. / fpsLimit)
				{
					fpsLimitPrevTime = fpsLimitCurrTime;
					fps++;
					Update();
					Draw();
				}
			}
		}
	}
}

// ReSharper disable once CppParameterMayBeConst
LRESULT ConwayGameOfLife::WndProc(HWND hWnd, const UINT message, const WPARAM wParam, const LPARAM lParam)
{
	switch (message)
	{
	case WM_KEYDOWN: if (wParam == VK_SPACE)
		{
			pause = !pause;
		}
		else if (wParam == VK_ESCAPE)
		{
			DestroyWindow(hWnd);
		}
		else if (wParam == VK_ADD)
		{
			fpsLimit += 10;
		}
		else if (wParam == VK_SUBTRACT)
		{
			fpsLimit -= 10;
		}
		else if (wParam == VK_MULTIPLY)
		{
			fpsLimit = -10;
		}
		return 0;
	case WM_DESTROY: PostQuitMessage(0);
		return 0;
	default: return DefWindowProc(hWnd, message, wParam, lParam);
	}
}
