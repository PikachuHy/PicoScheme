/*********************************************************************************/
/**
 * @file port.hpp
 *
 * Implementation of the three Scheme IO-ports. The standard, file and string
 * ports are thin wrappers around the c++ std::iostream, std::fstream and
 * std::string_stream classes.
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#ifndef PORT_HPP
#define PORT_HPP

#include <codecvt>
#include <fstream>
#include <iostream>
#include <locale>
#include <memory>
#include <sstream>
#include <variant>
#include <vector>

#include "utils.hpp"

namespace pscm {

struct Cell;
enum class Intern;

//! Enable locale globally and set all standard io-ports accordingly.
//! @param name Name of the locale.
void enable_locale(const char *name = "en_US.UTF-8");

//! Stream manipulator type, to change the default stream output for
//! value to a scheme (display <expr>) output.
template <typename T>
struct DisplayManip {
    const T& value;
};

template <typename T>
DisplayManip<T> display(const T& val) {
    return { val };
}

//! Output stream operator for scheme (display <expr>) output.
std::wostream& operator<<(std::wostream& os, DisplayManip<Cell> cell);

//! Default output stream operator for scheme (write <expr>) output.
std::wostream& operator<<(std::wostream& os, const Cell& cell);

//! Output stream operator to write essential opcodes
//! with their descriptive scheme symbol name.
std::wostream& operator<<(std::wostream& os, Intern opcode);

/**
 * Scheme io-port fascade to represent either an std::iostream,
 * std::fstream or std::stringstream port.
 */
template <typename Char>
class Port {
public:
    using stream_type = std::basic_iostream<Char, std::char_traits<Char>>;
    using openmode = std::ios_base::openmode;

    [[nodiscard]] virtual bool isStandardPort() const {
        return false;
    }

    [[nodiscard]] virtual bool isFilePort() const {
        return false;
    }

    [[nodiscard]] virtual bool isStringPort() const {
        return false;
    }

    void clear() {
        return m_stream.clear();
    }

    void flush() {
        m_stream.flush();
    }

    [[nodiscard]] bool eof() const {
        return m_stream.eof();
    }

    [[nodiscard]] bool fail() const {
        return m_stream.fail();
    }

    [[nodiscard]] bool good() const {
        return m_stream.good();
    }

    [[nodiscard]] bool bad() const {
        return m_stream.bad();
    }

    virtual void close() {
        m_stream.flush().clear();
        m_stream.setstate(stream_type::eofbit);
    }

    operator stream_type &() {
        return m_stream;
    }

    stream_type& stream() const {
        return m_stream;
    }

    [[nodiscard]] bool isInput() const {
        return mode & stream_type::in;
    }

    [[nodiscard]] bool isOutput() const {
        return mode & stream_type::out;
    }

    [[nodiscard]] bool isBinary() const {
        return mode & stream_type::binary;
    }

protected:
    Port(stream_type& stream, openmode mode)
        : m_stream{ stream }
        , mode{ mode } {
        stream.exceptions(stream_type::badbit);
    }

    virtual ~Port() {
        std::ios_base::sync_with_stdio(true);
    }

private:
    stream_type& m_stream;
    openmode mode;
};

template <typename Char>
class StandardPort
    : virtual public std::basic_iostream<Char, std::char_traits<Char>>
    , virtual public Port<Char> {
public:
    using stream_type = std::basic_iostream<Char, std::char_traits<Char>>;
    using openmode = typename Port<Char>::openmode;
    using stream_type::flush;

    explicit StandardPort(openmode mode = stream_type::out)
        : stream_type{ std::wcin.rdbuf() }
        , Port<Char>{ *this, mode } {
        pscm::enable_locale();

        if (mode & stream_type::out) {
            stream_type::set_rdbuf(std::wcout.rdbuf());
            stream_type::copyfmt(std::wcout);
            stream_type::clear(std::wcout.rdstate());
        }
        if (mode & stream_type::in) {
            stream_type::set_rdbuf(std::wcin.rdbuf());
            stream_type::copyfmt(std::wcin);
            stream_type::clear(std::wcin.rdstate());
        }
    }

    [[nodiscard]] bool isStandardPort() const final {
        return true;
    }
};

template <typename Char>
class StringPort
    : virtual public std::basic_stringstream<Char, std::char_traits<Char>>
    , virtual public Port<Char> {
public:
    using stream_type = std::basic_stringstream<Char, std::char_traits<Char>>;
    using string_type = std::basic_string<Char, std::char_traits<Char>>;
    using openmode = typename Port<Char>::openmode;
    using stream_type::str;

    explicit StringPort(openmode mode)
        : stream_type{ mode }
        , Port<Char>{ *this, mode } {
    }

    explicit StringPort(const string_type& str, openmode mode)
        : stream_type{ str, mode }
        , Port<Char>{ mode } {
    }

    [[nodiscard]] bool isStringPort() const final {
        return true;
    }
};

template <typename Char>
class FilePort
    : virtual public std::basic_fstream<Char, std::char_traits<Char>>
    , virtual public Port<Char> {
public:
    using stream_type = std::basic_fstream<Char, std::char_traits<Char>>;
    using openmode = typename Port<Char>::openmode;
    using stream_type::eof;

    explicit FilePort(const std::basic_string<Char>& filename, openmode mode)
        : stream_type{ string_convert<char>(filename), mode }
        , Port<Char>{ *this, mode } {
    }

    void close() final {
        if (stream_type::is_open()) {
            stream_type::flush().clear();
            stream_type::close();
        }
    }

    [[nodiscard]] bool isFilePort() const final {
        return true;
    }
};

struct input_port_exception : public std::ios_base::failure {

    template <typename PORT>
    input_port_exception(PORT&& port)
        : std::ios_base::failure{ "input port exception" } {
        if (!port.isInput())
            reason = "not an input port";
        else if (port.fail())
            reason = "reading from input port failed";
        else if (port.eof())
            reason = "end of file reached";
        else if (port.bad())
            reason = "bad input port state";
        else
            reason = "unknown input port error";
        port.clear();
    }

    [[nodiscard]] const char *what() const noexcept override {
        return reason.c_str();
    }

private:
    std::string reason;
};

struct output_port_exception : public std::ios_base::failure {
    template <typename PORT>
    output_port_exception(PORT&& port)
        : std::ios_base::failure{ "output port exception" } {
        if (!port.isOutput())
            reason = "not an output port";
        else if (port.fail())
            reason = "writing to output port failed";
        else if (port.eof())
            reason = "end of file reached";
        else if (port.bad())
            reason = "bad output port state";
        else
            reason = "unknown output port error";
        port.clear();
    }

    [[nodiscard]] const char *what() const noexcept override {
        return reason.c_str();
    }

private:
    std::string reason;
};

template <typename CharT, typename Traits>
std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const std::vector<Cell>& args) {
    if (args.empty()) {
        return os << "[]";
    }
    os << "[";
    os << args[0];
    for (int i = 1; i < args.size(); ++i) {
        os << ", " << args[i];
    }
    os << "]";
    return os;
}

template <typename T>
void debug_output_helper(const T& t) {
    std::wcout << t << std::endl;
}

template <typename U, typename... T>
void debug_output_helper(const U& u, const T&...t) {
    std::wcout << u << " ";
    debug_output_helper(t...);
}
#ifdef __vscode__
#define __PRINT_FILE__ __FILE__
#else
#define __PRINT_FILE__ __FILE_NAME__
#endif
#define DEBUG_OUTPUT(...)                                                                                              \
    do {                                                                                                               \
        std::wcout << std::endl << __PRINT_FILE__ << ":" << __LINE__ << " [" << __func__ << "] ";                      \
        debug_output_helper(__VA_ARGS__);                                                                              \
    } while (0)

} // namespace pscm
#endif // PORT_HPP
