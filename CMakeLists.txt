cmake_minimum_required(VERSION 3.20)
project(PicoScheme VERSION 0.1 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
find_package(Boost 1.78
        COMPONENTS REQUIRED
        stacktrace_basic
        stacktrace_addr2line
        )
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
        src/syntax.cpp
        src/promise.cpp
        src/frame.cpp
        src/continuation.cpp
        src/module.cpp
        src/compiler.cpp
        src/machine.cpp
        )
target_include_directories(pscm PUBLIC src/include)
if (__vscode__)
    target_compile_definitions(pscm PUBLIC __vscode__)
endif (__vscode__)
#target_compile_definitions(pscm PRIVATE BOOST_STACKTRACE_USE_ADDR2LINE)
target_compile_definitions(pscm PUBLIC _GNU_SOURCE)
target_link_libraries(pscm PUBLIC dl)
target_link_libraries(pscm PUBLIC Boost::boost
        Boost::stacktrace_basic
        Boost::stacktrace_addr2line
        )

add_executable(pscm_test test/picoscm.cpp)
target_link_libraries(pscm_test PRIVATE pscm)

add_executable(pscm_main main.cpp)
target_link_libraries(pscm_main PRIVATE pscm)
set_target_properties(pscm_main
        PROPERTIES
        OUTPUT_NAME pscm
        )

add_subdirectory(test)