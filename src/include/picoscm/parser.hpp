/*********************************************************************************/
/**
 * @file stream.hpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#ifndef PARSER_HPP
#define PARSER_HPP

#include <istream>
#include <ostream>
#include <utility>

#include "number.hpp"
#include "scheme.h"

namespace pscm {

class Parser {

public:
    Parser(Scheme& scm, StringView in);

    //! Read the next scheme expression from the argument input stream.
    Cell read();

    //! Try to convert the argument string into a scheme number or
    //! return #false for an unsuccessful conversion.
    static Cell strnum(const String&);

    bool is_finished();

private:
    enum class Token {
        None,
        OBrace,  // (
        CBrace,  // )
        Comment, // \;[^\n\r]*

        Dot,
        Quote,
        QuasiQuote,
        Unquote,
        UnquoteSplice,
        True,
        False,
        Char,

        String, // "([^"]*)"
        Number, // (+|-)[0-9]+(\.[0-9]+)
        Symbol, // [a-zA-Z_%:+-][a-zA-Z_%:+-]*
        Vector,
        Regex, // #re"12343434"

        Eof,
        Error
    };

    Cell parse_list();
    Cell parse_vector();
    Token get_token();

    static bool is_alpha(int c);
    static bool is_special(int c);

    static Token lex_number(const String&, Number&);
    Token lex_string(String&);
    Token lex_regex(String&);
    Token lex_symbol(const String&);
    Token lex_unquote(const String&);
    Token lex_char(const String&, Char& c);

    Token lex_special(String&);
    Token skip_comment();
    Char read_char(bool return_eof = false);
    Char peak_char();
    void putback_char(Char c);
    Token put_back = Token::None;
    String strtok;
    Number numtok;
    Char chrtok;
    Scheme& scm;
    std::vector<StringView> string_list;
    std::size_t row = 0;
    std::size_t col = 0;

    struct Pos {
        std::size_t row;
        std::size_t col;
    };

    Pos start_pos;

    const Symbol s_quote = scm.symbol("quote"), s_quasiquote = scm.symbol("quasiquote"),
                 s_unquote = scm.symbol("unquote"), s_unquotesplice = scm.symbol("unquote-splicing"),
                 s_expr = scm.symbol();
};

struct parse_error : public std::exception {
    explicit parse_error(std::string str, std::size_t row, std::size_t col)
        : reason{ std::move(str) }
        , row{ row + 1 }
        , col{ col } {
    }

    [[nodiscard]] const char *what() const noexcept override {
        return reason.c_str();
    }

    std::size_t row;
    std::size_t col;

private:
    std::string reason;
};
} // namespace pscm
#endif // PARSER_HPP
