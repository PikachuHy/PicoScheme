cmake_minimum_required(VERSION 3.20)
project(PicoScheme VERSION 0.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(pscm)
target_sources(pscm PUBLIC
        src/cell.cpp
        src/clock.cpp
        src/gc.cpp
        src/number.cpp
        src/parser.cpp
        src/primop.cpp
        src/procedure.cpp
        src/scheme.cpp
        src/port.cpp
        )
target_include_directories(pscm PUBLIC src/include/picoscm src/include)

add_executable(pscm_test test/picoscm.cpp)
target_link_libraries(pscm_test PRIVATE pscm)
