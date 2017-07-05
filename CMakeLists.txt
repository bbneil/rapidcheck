cmake_minimum_required(VERSION 3.0.0)
project(rapidcheck)

# Don't warn about symbol visibility for static libraries with CMake 3.3 and later.
if(POLICY CMP0063)
  cmake_policy(SET CMP0063 NEW)
endif()

enable_testing()

option(RC_ENABLE_TESTS "Build RapidCheck tests" OFF)
option(RC_ENABLE_EXAMPLES "Build RapidCheck examples" OFF)

if(MSVC)
  # /bigobj - some object files become very large so we need this
  # /wd4503 - truncation of decorated name, not much we can do about it so
  #           disable it
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /bigobj /wd4503 /WX")
  # /RTC* is incompatible with /O2 needed for Random.cpp to speed it up
  string(REGEX REPLACE "/RTC(su|[1su])" ""
    CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror -Wno-missing-braces -std=c++11")
endif()

file(GLOB_RECURSE src_folder "src/*.cpp")

add_library(rapidcheck STATIC ${src_folder})

# Random is used a LOT so it should preferrably be really fast.
if(MSVC)
  set_property(SOURCE src/Random.cpp
    APPEND_STRING PROPERTY COMPILE_FLAGS " /O2")
else()
  set_property(SOURCE src/Random.cpp
    APPEND_STRING PROPERTY COMPILE_FLAGS " -O3")
endif()


target_include_directories(rapidcheck PUBLIC include)

if (RC_ENABLE_TESTS)
  add_subdirectory(test)
endif()

if (RC_ENABLE_EXAMPLES)
  add_subdirectory(examples)
endif()

add_subdirectory(extras)

install(DIRECTORY "include/" DESTINATION "include" PATTERN ".*" EXCLUDE)
if( CMAKE_SIZEOF_VOID_P EQUAL 8 )
    install(TARGETS rapidcheck DESTINATION "lib64")
else( CMAKE_SIZEOF_VOID_P EQUAL 8 ) 
    install(TARGETS rapidcheck DESTINATION "lib")
endif( CMAKE_SIZEOF_VOID_P EQUAL 8 )
