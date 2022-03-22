cmake_minimum_required(VERSION 3.0.0)
project(ImageReviewer VERSION 0.1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_VERBOSE_MAKEFILE ON)

#set(JPEG_LIB_VERSION 70)

#find_package(JPEG REQUIRED)
find_package(OpenCV CONFIG REQUIRED)

add_executable(ImageReviewer ImageReviewer/main.cpp)

target_link_libraries(ImageReviewer PRIVATE opencv_core opencv_highgui opencv_imgcodecs)
#target_include_directories(ImageReviewer PRIVATE ${JPEG_INCLUDE_DIR})
