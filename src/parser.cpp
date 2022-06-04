/*********************************************************************************/
/**
 * @file parser.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <algorithm>
#include <cctype>
#include <cstring>

#include "picoscm/cell.hpp"
#include "picoscm/parser.hpp"

namespace pscm {

using namespace std::string_literals;

struct parse_eof_error : public std::exception {
    parse_eof_error(std::size_t row, std::size_t col) {
        reason.append("row ");
        reason.append(std::to_string(row));
        reason.append(", col ");
        reason.append(std::to_string(col));
    }

    [[nodiscard]] const char *what() const noexcept override {
        return reason.c_str();
    }

private:
    std::string reason{ "eof: " };
};

Parser::Parser(Scheme& scm, StringView in)
    : scm(scm) {
    std::size_t start_index = 0;
    for (std::size_t i = 0; i < in.size(); ++i) {
        if (in.at(i) == '\n') {
            string_list.emplace_back(in.substr(start_index, i - start_index + 1));
            start_index = i + 1;
        }
    }
    if (start_index < in.size()) {
        string_list.emplace_back(in.substr(start_index));
    }
}

/**
 * Lexical analyse the argument string for an integer, a floating point or complex number.
 *
 * @param str  String to analyse.
 * @param num  Uppon success, return the converted number.
 */
Parser::Token Parser::lex_number(const String& str, Number& num) {
    NumberParser numberParser(str);
    numberParser.parse();
    if (numberParser.parse_success()) {
        num = numberParser.parsed_number();
        return Token::Number;
    }
    else {
        return Token::Error;
    }
}

Cell Parser::strnum(const String& str) {
    Number num;
    Token tok;

    if (!str.compare(0, 2, L"#i")) {
        tok = lex_number(str.substr(2), num);
    }

    else if (!str.compare(0, 2, L"#e")) {
        tok = lex_number(str.substr(2), num);
        if (tok == Token::Number) {
            num = trunc(num);
        }
    }
    else {
        tok = lex_number(str, num);
    }

    if (tok == Token::Error) {
        return false;
    }

    return num;
}

/**
 * @brief Read characters from input stream into argument string.
 */
Parser::Token Parser::lex_string(String& str) {
    str.clear();
    Char c;

    while (true) {
        c = read_char();
        switch (c) {

        case '"':
            return Token::String;

        case '\\':
            str.push_back('\\');
            c = read_char();
            [[fallthrough]];

        default:
            if (iswprint(c))
                str.push_back(static_cast<Char>(c));
            else
                return Token::Error;
        }
    }
}

Parser::Token Parser::lex_regex(String& str) {
    if (str != L"#re" || read_char() != '\"') {
        return Token::Error;
    }

    if (lex_string(str) != Token::String) {
        return Token::Error;
    }

    return Token::Regex;
}

/**
 * @brief Lexical analyse the argument string for valid scheme
 *        symbol characters.
 */
Parser::Token Parser::lex_symbol(const String& str) {
    if (str == L"1+" || str == L"1-") {
        return Token::Symbol;
    }
    if (str.empty() || !is_alpha(str.front()))
        return Token::Error;

    for (auto c : str)
        if (!is_alpha(c) && !iswdigit(c))
            return Token::Error;

    return Token::Symbol;
}

Parser::Token Parser::lex_char(const String& str, Char& c) {
    constexpr struct {
        const Char *name;
        Int c;
    } stab[]{
        // clang-format off
            // https://www.gnu.org/software/guile/manual/html_node/Characters.html
            { L"#\\ht",         wchar_t(9)},
            { L"#\\eof",        EOF},
            { L"#\\alarm",     '\a'},
            { L"#\\backspace", '\b'},
            { L"#\\delete",    '\0'},
            { L"#\\escape",    '\0'},
            { L"#\\newline",   '\n'},
            { L"#\\null",      '\0'},
            { L"#\\return",    '\r'},
            { L"#\\space",     ' ' },
            { L"#\\tab",       '\t'},
            { L"#\\ae",        L'ä'},  { L"#\\AE",        L'Ä'},
            { L"#\\ue",        L'ü'},  { L"#\\UE",        L'Ü'},
            { L"#\\oe",        L'ö'},  { L"#\\OE",        L'Ö'},
            { L"#\\ss",        L'ß'},
            { L"#\\_0",        L'₀'},  { L"#\\^0",        L'⁰'},
            { L"#\\_1",        L'₁'},  { L"#\\^1",        L'¹'},
            { L"#\\_2",        L'₂'},  { L"#\\^2",        L'²'},
            { L"#\\_3",        L'₃'},  { L"#\\^3",        L'³'},
            { L"#\\_4",        L'₄'},  { L"#\\^4",        L'⁴'},
            { L"#\\_5",        L'₅'},  { L"#\\^5",        L'⁵'},
            { L"#\\_6",        L'₆'},  { L"#\\^6",        L'⁶'},
            { L"#\\_7",        L'₇'},  { L"#\\^7",        L'⁷'},
            { L"#\\_8",        L'₈'},  { L"#\\^8",        L'⁸'},
            { L"#\\_9",        L'₉'},  { L"#\\^9",        L'⁹'},
            { L"#\\alpha",     L'α'},
            { L"#\\beta",      L'β'},
            { L"#\\gamma",     L'γ'},  { L"#\\Gamma",     L'Γ'},
            { L"#\\delta",     L'δ'},  { L"#\\Delta",     L'Δ'},
            { L"#\\epsilon",   L'ε'},
            { L"#\\zeta",      L'ζ'},
            { L"#\\eta",       L'η'},
            { L"#\\theta",     L'θ'},
            { L"#\\iota",      L'ι'},
            { L"#\\kappa",     L'κ'},
            { L"#\\lambda",    L'λ'},
            { L"#\\mu",        L'μ'},
            { L"#\\nu",        L'ν'},
            { L"#\\xi",        L'ξ'},  { L"#\\Xi",       L'Ξ'},
            { L"#\\omicron",   L'ο'},
            { L"#\\pi",        L'π'},  { L"#\\Pi",       L'Π'},
            { L"#\\rho",       L'ρ'},
            { L"#\\tau",       L'τ'},
            { L"#\\sigma",     L'σ'},  { L"#\\Sigma",    L'Σ'},
            { L"#\\upsilon",   L'υ'},
            { L"#\\phi",       L'φ'},  { L"#\\Phi",      L'Φ'},
            { L"#\\chi",       L'χ'},
            { L"#\\psi",       L'ψ'},  { L"#\\Psi",      L'Ψ'},
            { L"#\\omega",     L'ω'},  { L"#\\Omega",    L'Ω'},
            { L"#\\le",        L'≤'},
            { L"#\\ge",        L'≥'},
            { L"#\\sim",       L'∼'},
            { L"#\\simeq",     L'≃'},
            { L"#\\approx",    L'≈'},
            { L"#\\nabla",     L'∇'},
            { L"#\\nabla",     L'∇'},
            { L"#\\nabla",     L'∇'},
            { L"#\\sum",       L'∑'},
            { L"#\\prod",      L'∏'},
            { L"#\\int",       L'∫'},
            { L"#\\oint",      L'∮'},
            { L"#\\pm",        L'±'},
            { L"#\\div",       L'÷'},
            { L"#\\cdot",      L'·'},
            { L"#\\star",      L'⋆'},
            { L"#\\circ",      L'∘'},
            { L"#\\bullet",    L'•'},
            { L"#\\diamond",   L'◇'},
            { L"#\\lhd",       L'◁'},
            { L"#\\rhd",       L'▷'},
            { L"#\\trup",      L'△'},
            { L"#\\trdown",    L'▽'},
            { L"#\\times",     L'×'},
            { L"#\\otimes",    L'⊗'},
            { L"#\\in",        L'∈'},
            { L"#\\notin",     L'∉'},
            { L"#\\subset",    L'⊂'},
            { L"#\\subseteq",  L'⊆'},
            { L"#\\in",        L'∈'},
            { L"#\\infty",     L'∞'},
        }; // clang-format on

    constexpr size_t ntab = sizeof(stab) / sizeof(*stab);

    if (str.size() == 2 && (std::isspace(peak_char()) || is_special(peak_char()))) {
        c = read_char();
        return Token::Char;
    }
    if (str.size() == 3) {
        c = str[2];
        return Token::Char;
    }
    else if (str.size() > 3 && str[2] == L'x') {
        String s{ str.substr(1) };
        s[0] = L'0';
        c = static_cast<Char>(stoi(s));
        return Token::Char;
    }
    else {
        String name;
        transform(str.begin(), str.end(), back_inserter(name), ::tolower);

        for (const auto& tab : stab)
            if (tab.name == name) {
                c = static_cast<Char>(tab.c);
                return Token::Char;
            }
    }
    return Token::Error;
}

//! Lexical analyse a special scheme symbol.
Parser::Token Parser::lex_special(String& str) {
    if (str == L"#")
        return Token::Vector;

    Token tok;

    switch (str.at(1)) {
    case 't':
        if (str == L"#t" || str == L"#true")
            return Token::True;
        [[fallthrough]];

    case 'f':
        if (str == L"#f" || str == L"#false")
            return Token::False;
        [[fallthrough]];

    case '\\':
        return lex_char(str, chrtok);

    case 'e':
        tok = lex_number(str.substr(2), numtok);
        if (tok == Token::Number)
            numtok = trunc(numtok);
        return tok;

    case 'i':
        return lex_number(str.substr(2), numtok);

    case 'r':
        return lex_regex(str);

    default:
        return Token::Error;
    }
}

//! Scan if str contains an scheme unquote "," or unquote-splicing ",@"
Parser::Token Parser::lex_unquote(const String& str) {
    if (str.size() != 1)
        return Token::Error;

    if (peak_char() == '@') {
        read_char();
        return Token::UnquoteSplice;
    }
    return Token::Unquote;
}

//! Skip a comment line.
Parser::Token Parser::skip_comment() {
    row++;
    col = 0;
    return Token::Comment;
}

//! Predicate returns true if the argument character is a special
//! scheme character, starting a new expression, string or comment.
bool Parser::is_special(int c) {
    return strchr("()\"'`,;", c);
}

//! Predicate return true if argument character is an allowed scheme character.
bool Parser::is_alpha(int c) {
    return iswgraph(c) && !iswdigit(c) && !is_special(c);
}

/**
 * Return the next token from the input stream.
 *
 * Depending on the token type, the token value is stored in member variable
 * strtok, numtok or chrtok. For invalid input an Error token is returned.
 */
Parser::Token Parser::get_token() {
    // Check if there is a put-back token available:
    if (put_back != Token::None) {
        Token tok = put_back;
        put_back = Token::None;
        return tok;
    }
    // Ignore all leading whitespaces:
    Char c;
    try {
        do {
            c = read_char();
        } while (iswspace(c));
    }
    catch (const parse_eof_error& ex) {
        return Token::Eof;
    }
    strtok.clear();
    strtok.push_back(static_cast<Char>(c));

    // Read chars until a trailing whitespace,
    // a special scheme character or EOF is reached:
    if (!is_special(c) && !is_finished()) {
        c = read_char(true);
        while (!iswspace(c) && !is_special(c) && !is_eof(c)) {
            strtok.push_back(static_cast<Char>(c));
            c = read_char(true);
        }
        putback_char(c);
    }
    // Lexical analyse token string according to the first character:
    switch (c = strtok.front()) {

    case '(':
        return Token::OBrace;

    case ')':
        return Token::CBrace;

    case '\'':
        return Token::Quote;

    case '`':
        return Token::QuasiQuote;

    case ',':
        return lex_unquote(strtok);

    case ';':
        return skip_comment();

    case '#':
        return lex_special(strtok);

    case '"':
        return lex_string(strtok);

    case '.':
        if (strtok.size() == 1)
            return Token::Dot;
        [[fallthrough]];

    default:
        NumberParser numberParser(strtok);
        numberParser.parse();
        if (numberParser.parse_success()) {
            numtok = numberParser.parsed_number();
            return Token::Number;
        }
        else {
            return lex_symbol(strtok);
        }
    }
}

bool Parser::is_finished() {
    try {
        peak_char();
        return false;
    }
    catch (const parse_eof_error& ex) {
        return true;
    }
}

Cell Parser::read() {
    start_pos = { row, col };
    for (;;) {
        Token token;
        try {
            token = get_token();
        }
        catch (const parse_eof_error& ex) {
            token = Token::Eof;
        }
        switch (token) {
        case Token::Comment:
            break;

        case Token::True:
            return true;

        case Token::False:
            return false;

        case Token::Char:
            return chrtok;

        case Token::Quote: {
            auto expr = read();
            // DEBUG_OUTPUT("read quote:", expr);
            return scm.list(s_quote, expr);
        }

        case Token::QuasiQuote: {
            auto expr = read();
            // DEBUG_OUTPUT("read quasiquote:", expr);
            return scm.list(s_quasiquote, expr);
        }

        case Token::Unquote: {
            auto expr = read();
            // DEBUG_OUTPUT("read unquote:", expr);
            return scm.list(s_unquote, expr);
        }

        case Token::UnquoteSplice: {
            auto expr = read();
            // DEBUG_OUTPUT("read unquotesplice:", expr);
            return scm.list(s_unquotesplice, expr);
        }

        case Token::Number:
            return numtok;

        case Token::String:
            return str(strtok);

        case Token::Regex:
            return regex(strtok);

        case Token::Symbol: {
            auto sym = scm.symbol(strtok);
            auto s = sym.value();
            if (!s.empty()) {
                if (s.front() == ':' || s.back() == ':') {
                    // DEBUG_OUTPUT("read keyword:", sym);
                    return Keyword(sym);
                }
            }
            // DEBUG_OUTPUT("read symbol:", sym);
            return sym;
        }

        case Token::Vector: {
            auto expr = parse_vector();
            // DEBUG_OUTPUT("read vector:", expr);
            return expr;
        }

        case Token::OBrace: {
            auto expr = parse_list();
            // DEBUG_OUTPUT("read list:", expr);
            return expr;
        }

        case Token::Eof:
            return static_cast<Char>(EOF);

        case Token::Error:
        default:
            throw parse_error("invalid token", row, col);
        }
    }
}

//! Read a scheme vector from stream.
Cell Parser::parse_vector() {
    VectorPtr vptr = vec(0, none);
    Token tok = get_token();

    if (tok == Token::OBrace)
        while (true) {
            switch (tok = get_token()) {
            case Token::Comment:
                break;
            case Token::CBrace:
                return vptr;
            case Token::Eof:
            case Token::Error:
                goto error;
            default:
                put_back = tok;
                vptr->push_back(read());
            }
        }
error:
    throw parse_error("error while reading vector", row, col);
}

//! Read a scheme list from stream.
Cell Parser::parse_list() {
    Cell list = nil, tail = nil;
    Cell cell;
    Token tok;

    while (true) {
        tok = get_token();
        switch (tok) {
        case Token::Comment:
            break;
        case Token::CBrace:
            return list;

        case Token::Dot:
            cell = read();
            tok = get_token();

            if (tok == Token::CBrace) {
                set_cdr(tail, cell);
                return list;
            }
            [[fallthrough]];
        case Token::Eof:
        case Token::Error:
            goto error;

        default:
            put_back = tok;
            cell = read();

            if (is_pair(tail)) {
                set_cdr(tail, scm.cons(cell, nil));
                tail = cdr(tail);
            }
            else {
                list = tail = scm.cons(cell, nil);
                scm.addenv(s_expr, list); // add list to env to prevent gc from deleting it.
            }
        }
    }
error:
    DEBUG_OUTPUT("error while reading list at", "row:", row, "col:", col);
    for (std::size_t i = start_pos.row; i < row; ++i) {
        std::wcout << string_list.at(i) << std::endl;
    }
    if (row >= string_list.size()) {
        std::wcout << std::endl;
        // print nothing
        std::wcout << "^";
        std::wcout << std::endl;
    }
    else {
        std::wcout << string_list.at(row) << std::endl;
        for (size_t i = 0; i < col; ++i) {
            std::wcout << " ";
        }
        std::wcout << "^";
        std::wcout << std::endl;
    }
    throw parse_error("error while reading list", row, col);
}

Char Parser::read_char(bool return_eof) {
    if (row >= string_list.size()) {
        if (return_eof) {
            return EOF;
        }
        throw parse_eof_error(row, col);
    }
    else if (col >= string_list[row].size()) {
        row++;
        col = 0;
    }
    else {
        return string_list.at(row).at(col++);
    }
    return read_char(return_eof);
}

Char Parser::peak_char() {
    auto c = read_char();
    putback_char(c);
    return c;
}

void Parser::putback_char(Char c) {
    if (c == EOF) {
        return;
    }
    if (col == 0) {
        DEBUG_OUTPUT("error putback char");
        throw std::runtime_error("error");
    }
    col--;
}

} // namespace pscm
