cmake_minimum_required (VERSION 2.8.11)
project (LPJmL_Util)
set(EXECUTABLE_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/bin")
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_files)

include_directories("${PROJECT_SOURCE_DIR}/include")
message("${cmake_include_path}")

add_subdirectory(src)
add_subdirectory(applications)
add_subdirectory(doc)
