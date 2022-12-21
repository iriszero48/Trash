@ECHO OFF
ECHO Starting PHP FastCGI...
set PATH="D:\Program Files\php-8.1.5-Win32-vs16-x64";%PATH%
"D:\Program Files\php-8.1.5-Win32-vs16-x64\php-cgi.exe" -b 127.0.0.1:9000