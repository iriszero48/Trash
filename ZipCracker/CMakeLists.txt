cmake_minimum_required(VERSION 3.9)
project(fz)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_VERBOSE_MAKEFILE ON)

if(UNIX)
	set(THREADS_PREFER_PTHREAD_FLAG ON)
	find_package(Threads REQUIRED)
endif()

aux_source_directory(. srcs)
add_executable(fz ${srcs})

if(WIN32)
	target_link_libraries(fz)
else()
	target_link_libraries(fz PUBLIC tbb Threads::Threads)
endif()
