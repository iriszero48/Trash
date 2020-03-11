#include "WindowsApp.h"

// ReSharper disable once CppParameterMayBeConst
LRESULT WINAPI MainWndProc(HWND hWnd, const UINT message, const WPARAM wParam, const LPARAM lParam)
{
	return WindowsApp::GetApp()->WndProc(hWnd, message, wParam, lParam);
}

WindowsApp* WindowsApp::app = nullptr;

// ReSharper disable once CppParameterMayBeConst
WindowsApp::WindowsApp(HINSTANCE hInstance) : hInstance(hInstance)
{
	app = this;
}

WindowsApp::~WindowsApp() = default;

void WindowsApp::InitializeWindows(const int width, const int height, const std::wstring& title)
{
	WNDCLASS wc =
	{
		CS_HREDRAW | CS_VREDRAW,
		MainWndProc,
		0,
		0,
		hInstance,
		LoadIcon(hInstance, IDI_APPLICATION),
		LoadCursor(nullptr, IDC_ARROW),
		static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)),
		nullptr,
		L"main"
	};
	RegisterClass(&wc);
	hWnd = CreateWindow(
		L"main",
		title.c_str(),
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		width,
		height,
		nullptr,
		nullptr,
		hInstance,
		nullptr);
	ShowWindow(hWnd, SW_SHOW);
	UpdateWindow(hWnd);
}

WindowsApp* WindowsApp::GetApp()
{
	return app;
}
