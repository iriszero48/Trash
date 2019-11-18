@echo off
setlocal enabledelayedexpansion
call :fib %1 res
echo %res%
exit /b %errorlevel%

:fib
if %1 equ 0 (
    set "%~2=0"
    exit /b 0
)
set "p=0"
set "n=1"
set /a e=%1-1
for /l %%i in (1,1,%e%) do (
    set /a s=!p!+!n!
    set "p=!n!"
    set "n=!s!"
)
set "%~2=%n%"
exit /b 0
