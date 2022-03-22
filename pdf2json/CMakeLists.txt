cmake_minimum_required(VERSION 3.9)
project(p2j VERSION 1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_VERBOSE_MAKEFILE ON)

include_directories(/usr/local/include/)

aux_source_directory(PdfJsonDev src)
add_executable(p2j ${src})

target_link_libraries(p2j /usr/local/lib/libmupdf.a /usr/local/lib/libmupdf-third.a)
