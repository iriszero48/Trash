#include <Windows.h>

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPWSTR lpCmdLine, int nCmdShow)
{
	WNDCLASS wc =
	{
		CS_HREDRAW | CS_VREDRAW,
		[](HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) -> LRESULT
		{
			switch (message)
			{
			case WM_KEYDOWN:
				if (wParam == VK_ESCAPE) DestroyWindow(hWnd);
				return 0;
			case WM_DESTROY:
				PostQuitMessage(0);
				return 0;
			default:
				return DefWindowProc(hWnd, message, wParam, lParam);
			}
		},
		0,
		0,
		hInstance,
		LoadIcon(hInstance, IDI_APPLICATION),
		LoadCursor(nullptr, IDC_ARROW),
		static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)),
		nullptr,
		"base"
	};
	RegisterClass(&wc);
	const HWND mainWnd = CreateWindow("base", "Fuck Win32", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, nullptr, nullptr, hInstance, nullptr);
	ShowWindow(mainWnd, nCmdShow);
	UpdateWindow(mainWnd);
	MSG msg;
	while (GetMessage(&msg, nullptr, 0, 0) != 0)
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 0;
}
