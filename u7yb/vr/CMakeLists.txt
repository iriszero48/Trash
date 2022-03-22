cmake_minimum_required(VERSION 3.9)
project(vr VERSION 1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_VERBOSE_MAKEFILE ON)

find_package(Threads REQUIRED)
find_package(nlohmann_json CONFIG REQUIRED)
find_package(FFMPEG REQUIRED)

add_executable(vr )

if (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0 AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 8.0)
    target_link_libraries(vr PRIVATE stdc++fs)
endif ()

target_include_directories(vr PRIVATE ${FFMPEG_INCLUDE_DIRS})

target_link_libraries(vr PRIVATE Threads::Threads ${FFMPEG_LIBRARIES} nlohmann_json::nlohmann_json)
