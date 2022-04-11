//
// Created by PikachuHy on 2022/4/11.
//

#include "picoscm/number.hpp"
#include <gtest/gtest.h>
#include <vector>
using namespace pscm;
TEST(number_parser_test, test_1)
{
    auto l = std::vector<String>{ L"i2", L"m2", L"i" };
    for (const auto& it : l) {
        NumberParser parser(it);
        parser.parse();
        ASSERT_FALSE(parser.parse_success()) << it;
    }
}
TEST(number_parser_test, test_2)
{
    auto l = std::vector<String>{ L"1+", L"1-" };
    for (const auto& it : l) {
        NumberParser parser(it);
        parser.parse();
        ASSERT_FALSE(parser.parse_success()) << it;
    }
}
TEST(number_parser_test, test_3)
{
    auto l = std::vector<std::pair<String, Number>>{
        {     L"i",  Complex(0,  1) },
        {  L"1+1i",  Complex(1,  1) },
        {  L"1-1i",  Complex(1, -1) },
        {   L"1-i",  Complex(1, -1) },
        {     L"1",     Int(1)   },
        {    L"+1",     Int(1)   },
        {    L"-1",    Int(-1)   },
        {     L"i",  Complex(0,  1) },
        {    L"+i",  Complex(0,  1) },
        {    L"-i",  Complex(0, -1) },
        {   L"1.0", Float(1.0)   },
        {   L"1e1",  Float(10)   },
        {  L"1e-1", Float(0.1)   },
        { L"1.0e1",  Float(10)   },
        {L"1.0e-1", Float(0.1)   },
    };
    for (const auto& [s, num] : l) {
        NumberParser parser(s);
        parser.parse();
        ASSERT_TRUE(parser.parse_success()) << s;
        ASSERT_EQ(parser.parsed_number(), num) << s;
    }
}