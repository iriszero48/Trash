g++ -std=c++26 -O2 -static -march=native C++\main.cpp -o C++\main.gcc.exe -lstdc++exp
python test.py -e C++\main.exe

clang++ -std=c++26 -O2 -march=native C++\main.cpp -o C++\main.clang.exe -lstdc++exp
python test.py -e C++\main.clang.exe

python test.py -e C++\build\Release\EasyMatMark.exe

wsl -e "g++-14 -std=c++26 -O2 -static -march=native /mnt/d/Projects/Gits/Trash/EasyMatMarker/C++/main.cpp -o /mnt/d/Projects/Gits/Trash/EasyMatMarker/C++/main"
python test.py -e "wsl -e /mnt/d/Projects/Gits/Trash/EasyMatMarker/C++/main"

gcc C\main.c -O2 -std=c17 -static -march=native -o C\main.exe
python test.py -e C\main.exe

nim c -d:release --o:Nim\EasyMatMark.exe .\Nim\EasyMatMark.nim
python test.py -e Nim\EasyMatMark.exe

python test.py -e C#\C#\bin\Release\net8.0\C#.exe

python test.py -e C#\C#_4\C#_4\bin\Release\C#_4.exe

dmd -O .\D\EasyMatMark.d -od=D
python test.py -e D\EasyMatMark.exe

wsl -e "gdc -O2 -march=native /mnt/d/Projects/Gits/Trash/EasyMatMarker/D/EasyMatMark.d -o /mnt/d/Projects/Gits/Trash/EasyMatMarker/D/EasyMatMark"
python test.py -e "wsl -e /mnt/d/Projects/Gits/Trash/EasyMatMarker/D/EasyMatMark"

d:\Programs\pypy3.10-v7.3.16-win64\pypy.exe -m compileall -o 2 Python\EasyMatMark.py
python test.py -e "d:\Programs\pypy3.10-v7.3.16-win64\pypy.exe Python\__pycache__\EasyMatMark.pypy310.opt-2.pyc"

python test.py -e "node JavaScript\EasyMatMark.js"

d:\Programs\dart-sdk_3.5.0_a97f632\bin\dart.exe compile exe -o Dart\EasyMatMark.exe Dart\EasyMatMark.dart
python test.py -e Dart\EasyMatMark.exe

d:\Programs\dart-sdk_3.5.0_a97f632\bin\dart.exe compile js -o Dart\EasyMatMark.js Dart\EasyMatMark.dart
python test.py -e "node Dart\EasyMatMark.js"

gfortran -O2 -march=native Fortran\EasyMatMark.f90 -o Fortran\EasyMatMark.exe
python test.py -e Fortran\EasyMatMark.exe

python test.py -e Fortran\EasyMatMarker\EasyMatMarker\x64\Release\EasyMatMarker.exe

d:\Programs\Go\bin\go.exe build -o .\Go\EasyMatMark.exe .\Go\EasyMatMark.go
python test.py -e Go\EasyMatMark.exe

d:\Programs\openjdk-22.0.1_windows-x64_bin\jdk-22.0.1\bin\javac.exe -d .\Java\ .\Java\src\Main.java
python test.py -e "D:\Programs\openjdk-22.0.1_windows-x64_bin\jdk-22.0.1\bin\java.exe -classpath Java Main"

python test.py -e "D:\Programs\php-8.3.9-nts-Win32-vs16-x64\php.exe PHP\EasyMatMark.php"

python -m compileall -o 2 Python\EasyMatMark.py
python test.py -e "python Python\__pycache__\EasyMatMark.cpython-312.opt-2.pyc"

C:\Users\iriszero\.rustup\toolchains\stable-x86_64-pc-windows-msvc\bin\cargo build --release --manifest-path .\Rust\Cargo.toml
python test.py -e Rust\target\release\Rust.exe

python test.py -e "cd SASS & .\run.ps1"

cd Zig; zig build --release=fast -Dcpu=native; cd ..
python test.py -e Zig\zig-out\bin\Zig.exe

python test.py -e "ruby Ruby\EasyMatMark.rb"

python test.py -e F#\F#\bin\x64\Release\net8.0\F#.exe














# 22082.6               613.406
python test.py -e '"d:\Programs\GNU Octave\Octave-9.2.0\mi.2.0\mingw64\bin\octave.exe" .\Octave\EasyMatMark.m'

# 1966.5999000000002    54.6278
python test.py -e "powershell .\AutoHotkey\run.ps1"

# 526882.12728500366    14635.6
python test.py -e "D:\msys64\usr\bin\zsh-5.9.exe Zsh\EasyMatMark.zsh"

# 7073.244500000001     196.479
python test.py -e '"D:\Programs\Wolfram Research\Wolfram Engine\14.0\wolframscript.exe" Wolfram\EasyMatMark.wls'

# 631                   17.5278
python test.py -e "d:\Programs\wren-cli-windows-0.4.0\wren_cli.exe .\Wren\EasyMatMark.wren"

# 180                   5
python test.py -e "D:\Programs\php-8.3.9-nts-Win32-vs16-x64\php.exe PHP\EasyMatMark.php"
