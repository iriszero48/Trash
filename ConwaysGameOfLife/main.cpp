#include "ConwaysGameOfLife.h"

// ReSharper disable once CppParameterMayBeConst
int WINAPI WinMain(_In_ HINSTANCE hInstance, _In_opt_ HINSTANCE, _In_ LPSTR, _In_ int)
{
	ConwayGameOfLife cgl(hInstance);
	cgl.Initialize();
	cgl.GenerateGosperGliderGun(0, 0);
	cgl.GenerateGosperGliderGun(30, 0);
	cgl.GenerateGosperGliderGun(60, 0);
	cgl.Run();
	return 0;
}
