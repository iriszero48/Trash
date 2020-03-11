#pragma once

#include <Windows.h>
#include <string>

class WindowsApp
{
protected:
	HINSTANCE hInstance;
	HWND hWnd{};

	static WindowsApp* app;
public:
	explicit WindowsApp(HINSTANCE hInstance);
	virtual ~WindowsApp();

	WindowsApp(const WindowsApp& windowsApp) = delete;
	WindowsApp(WindowsApp&& windowsApp) = delete;

	WindowsApp* operator=(const WindowsApp& windowsApp) = delete;
	WindowsApp* operator=(WindowsApp&& windowsApp) = delete;

	void InitializeWindows(int width, int height, const std::wstring& title);
	virtual void Initialize() = 0;
	virtual void Run() = 0;
	virtual LRESULT WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) = 0;

	static WindowsApp* GetApp();
};
