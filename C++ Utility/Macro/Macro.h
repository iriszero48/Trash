#pragma once

#pragma region private

#define __Macro_ToStringFunc__(x) #x

#pragma endregion private

#pragma region public

#define MacroToString(x) __Macro_ToStringFunc__(x)
#define MacroLine MacroToString(__LINE__)

#pragma endregion public
