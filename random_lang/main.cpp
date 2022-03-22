#include <random>
#include <iostream>
#include <filesystem>
#include <algorithm>

constexpr auto langs = R"(1 11l
2 8th
3 Ada
4 ALGOL 68
5 Arturo
6 AutoHotkey
7 BaCon
8 Batch File
9 BBC BASIC
10 C
10.1 Library: POSIX
10.2 Library: BSD libc
10.3 Windows
11 C#
12 C++
13 Caché ObjectScript
14 Clojure
15 CoffeeScript
16 Common Lisp
17 D
18 Delphi
19 E
20 Elixir
21 Emacs Lisp
22 Erlang
23 F#
24 Factor
25 Forth
26 Gambas
27 GAP
28 Go
29 Groovy
30 GUISS
31 Haskell
32 Icon and Unicon
32.1 Icon
32.2 Unicon
33 IDL
34 J
35 Java
36 JavaScript
37 Julia
38 Kotlin
39 Lasso
40 LiveCode
41 Lua
42 Mathematica/Wolfram Language
43 MATLAB / Octave
44 MAXScript
45 MoonScript
46 Nanoquery
47 Nim
48 Objeck
49 Objective-C
50 OCaml
51 ooRexx
51.1 version 1
51.2 version 2
52 Oz
53 Perl
54 Phix
55 PHP
55.1 PHP BFS (Breadth First Search)
56 PicoLisp
57 Pop11
58 PowerShell
59 Prolog
60 PureBasic
61 Prolog
62 Python
63 R
64 Racket
65 Raku
66 Rascal
67 REALbasic
68 Red
69 REXX
69.1 version 1
69.2 version 2
70 Ring
71 Ruby
72 Rust
73 Scala
74 Scheme
75 Seed7
76 Sidef
77 Smalltalk
78 Swift
79 Tcl
80 TXR
81 UNIX Shell
82 UnixPipes
83 Visual Basic .NET
84 Wren
85 zkl
86 Zsh)";

std::vector<std::filesystem::path> GetFilesRec(const std::filesystem::path& path)
{
    std::vector<std::filesystem::path> buf;
    for (auto p : std::filesystem::recursive_directory_iterator(path))
    {
        if (p.is_regular_file())
        {
            buf.push_back(p);
        }
    }
    return buf;
}

std::string GetPid(const std::filesystem::path& path)
{
    const auto u8p = path.u8string();
    return u8p.substr(1, u8p.find(")") - 1);
}

int main(int argc, char const *argv[])
{
    if (argc == 1)
    {
        std::cout << "find to_find copy_to" << std::endl;
        exit(EXIT_FAILURE);
    }

    const auto find_path = argv[1];
    const auto to_find_path = argv[2];
    const auto copy_to_path = argv[3];

    const auto find_files = GetFilesRec(find_path);
    const auto to_find_files = GetFilesRec(to_find_path);
    
    for (const auto &file : find_files)
    {
        const auto pid = GetPid(file);
        std::find(to_find_files.begin(), to_find_files.end(), [](const std::filesystem::path& path)
        {
            
        });
    }
    
}
