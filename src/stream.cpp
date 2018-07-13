/*********************************************************************************/ /**
 * @file stream.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <cstring>

#include "cell.hpp"
#include "stream.hpp"

namespace pscm {

/**
 * @brief Output stream operator for Cons type arguments.
 */
static std::ostream& operator<<(std::ostream& os, Cons* cons)
{
    Cell iter{ cons };

    os << '(' << car(iter);
    iter = cdr(iter);

    for (Cell slow{ iter }; is_pair(iter); iter = cdr(iter), slow = cdr(slow)) {
        os << ' ' << car(iter);

        if (!is_pair(iter = cdr(iter)) || slow == iter) {
            if (slow == iter)
                return os << " ...)"; // circular list detected

            break;
        }
        os << ' ' << car(iter);
    }
    if (is_nil(iter))
        os << ')'; // list end
    else
        os << " . " << iter << ')'; // dotted pair end

    return os;
}

static std::ostream& operator<<(std::ostream& os, const Vector& vec)
{
    os << "#(";
    for (const Cell& cell : vec)
        os << cell << ' ';
    return os << ')';
}

/**
 * @brief Output stream operator for Cell type arguments.
 */
std::ostream& operator<<(std::ostream& os, const Cell& cell)
{
    overloads fun{
        [&os](Nil) { os << "()"; },
        [&os](None) { os << "#none"; },
        [&os](Bool arg) { os << (arg ? "#t" : "#f"); },
        [&os](Char arg) { os << "#\\" << arg; },
        [&os](Number arg) { os << arg; },
        [&os](Intern arg) { os << "<intern " << static_cast<int>(arg) << '>'; },
        [&os](String arg) { os << '"' << *arg << '"'; },
        [&os](Vector arg) { os << arg; },
        [&os](Symbol arg) { os << arg.value(); },
        [&os](Symenv arg) { os << "<symenv>"; },
        [&os](Proc arg) { os << "<proc>"; },
        [&os](Port) { os << "<port>"; },
        [&os](Cons* arg) { os << arg; },

        /* catch missing overloads and emit compile time error message */
        [](auto arg) { static_assert(always_false<decltype(arg)>::value, "callable overload is missing"); }
    };
    std::visit(std::move(fun), static_cast<const Cell::base_type&>(cell));
    return os;
}

Port::Port()
    : pstream{ std::make_shared<stream_variant>(std::in_place_type_t<std::iostream>(), std::cout.rdbuf()) }
{
    is_open() || (throw std::invalid_argument("could open standard port"), 0);
}

bool Port::is_output() const noexcept
{
    return std::visit([](auto& stream) { return stream.out & std::ios_base::out; }, *pstream);
}

bool Port::is_input() const noexcept
{
    return std::visit([](auto& stream) { return stream.in & std::ios_base::in; }, *pstream);
}

bool Port::is_strport() const noexcept { return is_type<std::stringstream>(); }
bool Port::is_fileport() const noexcept { return is_type<std::fstream>(); }

bool Port::is_open() const noexcept
{
    return std::visit([](auto& os) {
        using S = std::decay_t<decltype(os)>;

        if constexpr (std::is_same_v<S, std::ofstream>)
            return os.is_open();
        else
            return os.good();
    },
        *pstream);
}

void Port::close()
{
    std::visit([](auto& os) {
        using S = std::decay_t<decltype(os)>;
        os.flush();
        os.clear();

        if constexpr (std::is_same_v<S, std::fstream>) {
            if (os.is_open())
                os.close();
        } else
            os.setstate(std::ios_base::eofbit);
    },
        *pstream);
}

bool Port::open(const std::filesystem::path& path, std::ios_base::openmode mode)
{
    close();
    *pstream = std::fstream{ path, mode };
    return is_open();
}

bool Port::open_str(const std::string& str, std::ios_base::openmode mode)
{
    close();
    *pstream = std::stringstream{ str, mode };
    return is_open();
}

std::string Port::str() const
{
    return std::get<std::stringstream>(*pstream).str();
}

std::iostream& Port::stream()
{
    return std::visit([](auto& os) -> std::iostream& { return os; }, *pstream);
}

bool Port::operator!=(const Port& stream) const noexcept
{
    return pstream.get() != stream.pstream.get();
}

bool Port::operator==(const Port& stream) const noexcept
{
    return !(*this != stream);
}

}; // namespace pscm
