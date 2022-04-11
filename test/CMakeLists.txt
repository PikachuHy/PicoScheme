enable_testing()
find_package(GTest REQUIRED)
add_executable(number_parser_test number_parser_test.cpp)
target_link_libraries(number_parser_test PRIVATE pscm GTest::gtest_main)