/*********************************************************************************/ /**
 * @file number.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <assert.h>
#include <cmath>
#include <limits>

#include "picoscm/number.hpp"

namespace pscm {

bool is_integer(const Number& num)
{
    static overloads number{
        [](Int) -> bool { return true; },
        [](Float x) -> bool {
            return !(x > floor(x)
                || x < ceil(x)
                || std::abs(x) > static_cast<Float>(std::numeric_limits<Int>::max()));
        },
        [](const Complex& z) -> bool {
            return imag(z) < 0 || imag(z) > 0 ? false : is_integer(real(z));
        },
    };
    return visit(number, static_cast<const Number::base_type&>(num));
}

bool is_odd(const Number& num)
{
    static overloads number{
        [](Int i) -> bool { return std::abs(i) % Int{ 2 }; },
        [](Float x) -> bool { return fmod(x, 2.); },
        [](const Complex& z) -> bool { return imag(z) < 0 || imag(z) > 0 ? true : fmod(real(z), 2.); },
    };
    return visit(number, static_cast<const Number::base_type&>(num));
}

/**
 * @brief Check wheter an integer addition of both argument values would overflow.
 */
constexpr bool overflow_add(Int a, Int b)
{
    constexpr Int min = std::numeric_limits<Int>::min(),
                  max = std::numeric_limits<Int>::max();

    return (b > 0 && a > max - b) || (b < 0 && a < min - b);
}

/**
 * @brief Check wheater integer substraction of both arguments values would overflow.
 */
constexpr bool overflow_sub(Int a, Int b)
{
    constexpr Int min = std::numeric_limits<Int>::min(),
                  max = std::numeric_limits<Int>::max();

    return (b > 0 && a < min + b) || (b < 0 && a > max + b);
}

//! Predicate function to test wheter the argument numbers aren't equal.
bool operator!=(const Number& lhs, const Number& rhs)
{
    using value_type = Complex::value_type;

    static overloads fun{
        [](const Complex& z0, const Complex& z1) -> bool {
            return z0 != z1;
        },
        [](const Complex& z, Float x) -> bool {
            return z != Complex{ static_cast<value_type>(x), 0 };
        },
        [](Float x, const Complex& z) -> bool {
            return z != Complex{ static_cast<value_type>(x), 0 };
        },
        [](auto x, auto y) -> bool {
            return x != y;
        }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Predicate function to test wheter the argument numbers are equal.
 */
bool operator==(const Number& lhs, const Number& rhs)
{
    return !(lhs != rhs);
}

bool operator<(const Number& lhs, const Number& rhs)
{
    return visit([](auto x, auto y) {
        if constexpr (!std::is_same_v<Complex, decltype(x)> && !std::is_same_v<Complex, decltype(y)>) {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) < static_cast<T>(y);
        } else
            return ((void)(throw std::invalid_argument("uncomparable complex number")), false);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

bool operator>(const Number& lhs, const Number& rhs)
{
    return visit([](auto x, auto y) {
        if constexpr (!std::is_same_v<Complex, decltype(x)> && !std::is_same_v<Complex, decltype(y)>) {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return (static_cast<T>(x) > static_cast<T>(y));
        } else
            return ((void)(throw std::invalid_argument("uncomparable complex number")), false);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

bool operator<=(const Number& lhs, const Number& rhs)
{
    return visit([](auto x, auto y) {
        if constexpr (!std::is_same_v<Complex, decltype(x)> && !std::is_same_v<Complex, decltype(y)>) {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) <= static_cast<T>(y);
        } else
            return ((void)(throw std::invalid_argument("uncomparable complex number")), false);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

bool operator>=(const Number& lhs, const Number& rhs)
{
    return visit([](auto x, auto y) {
        if constexpr (!std::is_same_v<Complex, decltype(x)> && !std::is_same_v<Complex, decltype(y)>) {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) >= static_cast<T>(y);
        } else
            return ((void)(throw std::invalid_argument("uncomparable complex number")), false);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

Number min(const Number& lhs, const Number& rhs)
{
    return visit([](auto& x, auto& y) -> Number {
        using T = std::common_type_t<decltype(x), decltype(y)>;
        return y < x ? static_cast<T>(y) : static_cast<T>(x);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

Number max(const Number& lhs, const Number& rhs)
{
    return visit([](auto& x, auto& y) -> Number {
        using T = std::common_type_t<decltype(x), decltype(y)>;
        return y > x ? static_cast<T>(y) : static_cast<T>(x);
    },
        static_cast<const Number::base_type&>(lhs), static_cast<const Number::base_type&>(rhs));
}

Number inv(const Number& x)
{
    x != Number{ 0 } || ((void)(throw std::invalid_argument("divide by zero")), 0);

    return visit([](auto& x) -> Number {
        if constexpr (std::is_same_v<const Complex&, decltype(x)>)
            return 1 / x;
        else
            return 1 / static_cast<Float>(x);
    },
        static_cast<const Number::base_type&>(x));
}

Number operator-(const Number& x)
{
    return visit([](auto& x) -> Number { return -x; },
        static_cast<const Number::base_type&>(x));
}

Number operator%(const Number& lhs, const Number& rhs)
{
    static overloads fun{
        [](const Complex&, const Complex&) -> Number { return ((void)(throw std::invalid_argument("modulo - not definied for complex numbers")), 0); },
        [](const Complex&, auto) -> Number { return ((void)(throw std::invalid_argument("modulo - not definied for complex numbers")), 0); },
        [](auto, const Complex&) -> Number { return ((void)(throw std::invalid_argument("modulo - not definied for complex numbers")), 0); },
        [](Int i0, Int i1) -> Number { return (i1 + i0 % i1) % i1; },
        [](auto x, auto y) -> Number { return fmod((y + fmod(x, y)), y); }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

Number remainder(const Number& lhs, const Number& rhs)
{
    static overloads fun{
        [](const Complex&, const Complex&) -> Number { return ((void)(throw std::invalid_argument("modulo - not definied for complex numbers")), 0); },
        [](const Complex&, auto) -> Number { return ((void)(throw std::invalid_argument("remainder - not definied for complex numbers")), 0); },
        [](auto, const Complex&) -> Number { return ((void)(throw std::invalid_argument("remainder - not definied for complex numbers")), 0); },
        [](Int i0, Int i1) -> Number { return static_cast<Int>(std::remainder(i0, i1)); },
        [](auto x, auto y) -> Number { return std::remainder(x, y); }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Number addition operator.
 *
 * In case of an integer overflow, the returned number is converted into a floating point sum.
 */
Number operator+(const Number& lhs, const Number& rhs)
{
    using value_type = Complex::value_type;

    static overloads fun{
        [](const Complex& z0, const Complex& z1) -> Number { return z0 + z1; },
        [](const Complex& z, auto x) -> Number { return z + static_cast<value_type>(x); },
        [](auto x, const Complex& z) -> Number { return static_cast<value_type>(x) + z; },
        [](Int i0, Int i1) -> Number {
            if (overflow_add(i0, i1))
                return static_cast<value_type>(i0) + static_cast<value_type>(i1);
            else
                return i0 + i1;
        },
        [](auto x, auto y) -> Number {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) + static_cast<T>(y);
        }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Number addition operator.
 *
 * In case of an integer overflow, the returned number is converted into a floating point sum.
 */
Number operator-(const Number& lhs, const Number& rhs)
{
    using value_type = Complex::value_type;

    static overloads fun{
        [](const Complex& z0, const Complex& z1) -> Number { return z0 - z1; },
        [](const Complex& z, auto x) -> Number { return z - static_cast<value_type>(x); },
        [](auto x, const Complex& z) -> Number { return static_cast<value_type>(x) - z; },
        [](Int i0, Int i1) -> Number {
            if (overflow_sub(i0, i1))
                return static_cast<value_type>(i0) - static_cast<value_type>(i1);
            else
                return i0 - i1;
        },
        [](auto x, auto y) -> Number {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) - static_cast<T>(y);
        }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Number multiplication operator.
 */
Number operator*(const Number& lhs, const Number& rhs)
{
    using value_type = Complex::value_type;

    static overloads fun{
        [](const Complex& z0, const Complex& z1) -> Number { return z0 * z1; },
        [](const Complex& z, auto x) -> Number { return z * static_cast<value_type>(x); },
        [](auto x, const Complex& z) -> Number { return static_cast<value_type>(x) * z; },
        [](auto x, auto y) -> Number {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) * static_cast<T>(y);
        }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Number division operator.
 */
Number operator/(const Number& lhs, const Number& rhs)
{
    using value_type = Complex::value_type;

    rhs != Number{ 0 } || ((void)(throw std::invalid_argument("divide by zero")), 0);

    static overloads fun{
        [](const Complex& z0, const Complex& z1) -> Number { return z0 / z1; },
        [](const Complex& z, auto x) -> Number { return z / static_cast<value_type>(x); },
        [](auto x, const Complex& z) -> Number { return static_cast<value_type>(x) / z; },
        [](Int x, Int y) -> Number {
            if (std::remainder(x, y) != Int{ 0 })
                return x / static_cast<value_type>(y);
            else
                return x / y;
        },
        [](auto x, auto y) -> Number {
            using T = std::common_type_t<decltype(x), decltype(y)>;
            return static_cast<T>(x) / static_cast<T>(y);
        }
    };
    return visit(fun,
        static_cast<const Number::base_type&>(lhs),
        static_cast<const Number::base_type&>(rhs));
}

/**
 * @brief Addition assignment operator.
 */
Number& operator+=(Number& lhs, const Number& rhs)
{
    return lhs = lhs + rhs;
}

/**
 * @brief Addition assignment operator.
 */
Number& operator-=(Number& lhs, const Number& rhs)
{
    return lhs = lhs - rhs;
}

/**
 * @brief Addition assignment operator.
 */
Number& operator*=(Number& lhs, const Number& rhs)
{
    return lhs = lhs * rhs;
}

/**
 * @brief Addition assignment operator.
 */
Number& operator/=(Number& lhs, const Number& rhs)
{
    return lhs = lhs / rhs;
}

template <typename T>
static T round_even(T x)
{
    return x - std::remainder(x, T{ 1 });
}

/**
 * @brief Round a number to the nearest integer.
 */
Number round(const Number& x)
{
    static overloads num{
        [](Int i) -> Number { return i; },
        [](Float x) -> Number { return round_even(x); },
        [](const Complex& z) -> Number {
            return { round_even(z.real()), round_even(z.imag()) };
        }
    };
    return visit(num, static_cast<const Number::base_type&>(x));
}

/**
 * @brief Largest integer not greator then x.
 */
Number floor(const Number& x)
{
    if (is_complex(x)) {
        const auto& z = static_cast<Complex>(x);
        return { std::floor(z.real()), std::floor(z.imag()) };
    } else
        return std::floor(static_cast<Float>(x));
}

/**
 * @brief Largest integer not greator then x.
 */
Number ceil(const Number& x)
{
    if (is_complex(x)) {
        const auto& z = static_cast<Complex>(x);
        return { std::ceil(z.real()), std::ceil(z.imag()) };
    } else
        return std::ceil(static_cast<Float>(x));
}

Number quotient(const Number& lhs, const Number& rhs)
{
    Number res = lhs / rhs;

    if (is_int(res))
        return res;

    return is_positive(res) ? floor(res) : ceil(res);
}

/**
 * @brief Largest integer not greator then x.
 */
Number trunc(const Number& x)
{
    return is_negative(x) ? ceil(x) : floor(x);
}

/**
 * @brief Computes the sine of a ::Number.
 */
Number sin(const Number& x)
{
    return is_complex(x) ? std::sin(static_cast<Complex>(x))
                         : std::sin(static_cast<Float>(x));
}

/**
 * @brief Computes the cosine of a ::Number.
 */
Number cos(const Number& x)
{
    return is_complex(x) ? std::cos(static_cast<Complex>(x))
                         : std::cos(static_cast<Float>(x));
}

/**
 * @brief Computes the cosine of a ::Number.
 */
Number tan(const Number& x)
{
    return is_complex(x) ? std::tan(static_cast<Complex>(x))
                         : std::tan(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number asin(const Number& x)
{
    return is_type<Complex>(x) ? std::asin(static_cast<Complex>(x))
                               : std::asin(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number acos(const Number& x)
{
    return is_complex(x) ? std::acos(static_cast<Complex>(x))
                         : std::acos(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number atan(const Number& x)
{
    return is_complex(x) ? std::atan(static_cast<Complex>(x))
                         : std::atan(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number sinh(const Number& x)
{
    return is_complex(x) ? std::sinh(static_cast<Complex>(x))
                         : std::sinh(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number cosh(const Number& x)
{
    return is_complex(x) ? std::cosh(static_cast<Complex>(x))
                         : std::cosh(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number tanh(const Number& x)
{
    return is_complex(x) ? std::tanh(static_cast<Complex>(x))
                         : std::tanh(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number asinh(const Number& x)
{
    return is_complex(x) ? std::asinh(static_cast<Complex>(x))
                         : std::asinh(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number acosh(const Number& x)
{
    return is_complex(x) ? std::acosh(static_cast<Complex>(x))
                         : std::acosh(static_cast<Float>(x));
}

/**
 * @brief Computes the arcus sinus of a ::Number.
 */
Number atanh(const Number& x)
{
    return is_complex(x) ? std::atanh(static_cast<Complex>(x))
                         : std::atanh(static_cast<Float>(x));
}

/**
 * @brief Compute e exponential of a ::Number.
 */
Number exp(const Number& x)
{
    return is_complex(x) ? std::exp(static_cast<Complex>(x))
                         : std::exp(static_cast<Float>(x));
}

/**
 * @brief Compute natural logarithm of a ::Number.
 */
Number log(const Number& x)
{
    return is_complex(x) ? std::log(static_cast<Complex>(x))
                         : std::log(static_cast<Float>(x));
}

/**
 * @brief Compute natural logarithm of a ::Number.
 */
Number log10(const Number& x)
{
    return is_complex(x) ? std::log10(static_cast<Complex>(x))
                         : std::log10(static_cast<Float>(x));
}

/**
 * @brief Computes the square root of ::Number.
 */
Number sqrt(const Number& x)
{
    Number res = is_complex(x) || x < Number{ 0 } ? std::sqrt(static_cast<Complex>(x))
                                                  : std::sqrt(static_cast<Float>(x));

    if (is_int(x) && is_float(res) && !(res != trunc(res)))
        return static_cast<Int>(res);

    return res;
}

/**
 * @brief Computes the cubic root of ::Number.
 */
Number cbrt(const Number& x)
{
    return is_complex(x) || x < Number{ 0 } ? std::pow(static_cast<Complex>(x), static_cast<Complex>(-3))
                                            : std::cbrt(static_cast<Float>(x));
}

/**
 * @brief Computes x raised by y.
 */
Number hypot(const Number& x, const Number& y)
{
    return is_complex(x) || is_complex(y) ? sqrt(static_cast<Complex>(x)
                                                    * static_cast<Complex>(x)
                                                + static_cast<Complex>(y)
                                                    * static_cast<Complex>(y))
                                          : std::hypot(static_cast<Float>(x),
                                                static_cast<Float>(y));
}

Number hypot(const Number& x, const Number& y, const Number& z)
{
    return is_complex(x) || is_complex(y) ? sqrt(static_cast<Complex>(x)
                                                    * static_cast<Complex>(x)
                                                + static_cast<Complex>(y)
                                                    * static_cast<Complex>(y)
                                                + static_cast<Complex>(z)
                                                    * static_cast<Complex>(z))

                                          : std::hypot(static_cast<Float>(x),
                                                static_cast<Float>(y),
                                                static_cast<Float>(z));
}

/**
 * @brief Computes x raised by y.
 */
Number pow(const Number& x, const Number& y)
{
    if (is_zero(x))
        return is_zero(y) ? Int{ 1 } : Int{ 0 };

    static overloads fun{
        [](const Complex& x, const Complex& y) -> Number {
            return std::pow(x, y);
        },
        [](const Complex& z, auto& x) -> Number {
            return std::pow(z, static_cast<Complex>(x));
        },
        [](auto& x, const Complex& z) -> Number {
            return std::pow(static_cast<Complex>(x), z);
        },
        [](Int x, Int y) -> Number {
            constexpr Int min = std::numeric_limits<Int>::min(),
                          max = std::numeric_limits<Int>::max();

            auto res = std::pow(static_cast<Float>(x), static_cast<Float>(y));
            if (y < 0 || res < min || res > max)
                return res;
            return static_cast<Int>(res);
        },
        [](auto x, auto y) -> Number {
            return std::pow(static_cast<Float>(x), static_cast<Float>(y));
        }
    };
    return visit(fun, static_cast<const Number::base_type&>(x),
        static_cast<const Number::base_type&>(y));
}

/**
 * @brief Return the real part of a complex number.
 */
Number real(const Number& z)
{
    return static_cast<Complex>(z).real();
}

/**
 * @brief Return the imaginary part of a complex number or zero for floating point or
 *        integer arguments.
 */
Number imag(const Number& z)
{
    return static_cast<Complex>(z).imag();
}

Number rect(const Number& x, const Number& y)
{
    return { static_cast<Float>(x), static_cast<Float>(y) };
}

/**
 * @brief Construct a complex number from magnitude and phase angle.
 */
Number polar(const Number& r, const Number& theta)
{
    return std::polar(static_cast<Float>(r), static_cast<Float>(theta));
}

Number arg(const Number& z)
{
    return std::arg(static_cast<Complex>(z));
}

Number conj(const Number& z)
{
    return std::conj(static_cast<Complex>(z));
}

Number abs(const Number& x)
{
    return visit([](const auto& x) -> Number { return std::abs(x); },
        static_cast<const Number::base_type&>(x));
}

void NumberParser::parse()
{
    if (s.empty()) {
        mark_parse_fail();
        return;
    }
    if (s == L"i") {
        mark_parse_fail();
        return;
    }
    if (last_char() == 'i') {
        auto num_opt = parse_complex();
        if (parse_fail()) {
            return;
        }
        num = num_opt.value();
    } else {
        auto sign = parse_sign(true).value_or(false);
        auto num_opt = parse_num();
        if (parse_fail()) {
            return;
        }
        num = num_opt.value();
        if (sign) {
            num = -num;
        }
    }
    if (!reach_last()) {
        mark_parse_fail();
        return;
    }
}
std::optional<bool> NumberParser::parse_sign(bool optional)
{
    if (parse_fail()) return std::nullopt;
    if (s[index] == '-') {
        advance();
        return true;
    } else if (s[index] == '+') {
        advance();
        return false;
    } else if (optional) {
        return std::nullopt;
    } else {
        mark_parse_fail();
        return std::nullopt;
    }
}
std::optional<Int> NumberParser::parse_digit(bool optional)
{
    if (parse_fail()) return std::nullopt;
    if (exceed_max_len()) {
        mark_parse_fail();
        return std::nullopt;
    }
    int count = 0;
    Int ret = 0;
    while (!exceed_max_len() && is_digit()) {
        ret = ret * 10 + (s[index] - '0');
        advance();
        count++;
    }
    if (count == 0) {
        if (optional) {
            return std::nullopt;
        }
        mark_parse_fail();
        return std::nullopt;
    }
    return ret;
}
std::optional<Number> NumberParser::parse_num(bool optional)
{
    auto cur_index = index;
    if (parse_fail()) return std::nullopt;
    auto num = parse_digit(optional);
    if (parse_fail()) {
        return std::nullopt;
    }
    if (!num.has_value()) {
        if (optional) return std::nullopt;
        mark_parse_fail();
        return std::nullopt;
    }
    bool has_point = false;
    Int point_num;
    if (s[index] == '.') {
        is_flo = true;
        has_point = true;
        advance();
        point_num = parse_digit().value();
    }
    Float val = num.value() * 1.0;
    bool has_e = false;
    Int e_num;
    if (s[index] == 'e' || s[index] == 'E') {
        has_e = true;
        advance();
        auto sign = parse_sign(true);
        e_num = parse_digit().value();
        if (sign.value_or(false)) {
            e_num = -e_num;
        }
    }
    auto new_index = index;
    if (has_point || has_e) {
        // convert str to Float
        return convert_str_to_float(s.substr(cur_index, new_index - cur_index));
    } else {
        // just Int
        return num;
    }
}
std::optional<Number> NumberParser::parse_complex()
{
    Number ret;
    auto sign1 = parse_sign(true).value_or(false);
    if (has_sign_after(index)) {
        auto num1_opt = parse_num();
        if (parse_fail()) {
            return std::nullopt;
        }
        auto sign2_opt = parse_sign();
        if (parse_fail()) {
            return std::nullopt;
        }
        auto num1 = num1_opt.value();
        if (sign1) {
            num1 = -num1;
        }
        auto num2 = parse_num(true).value_or(1);
        auto sign2 = sign2_opt.value();
        if (sign2) {
            num2 = -num2;
        }
        ret = Number(num1, num2);
    } else {
        auto num_opt = parse_num(true);
        if (parse_fail()) {
            return std::nullopt;
        }
        auto num = num_opt.value_or(1);
        if (sign1) {
            num = -num;
        }
        ret = Number(0, num);
    }
    if (exceed_max_len()) {
        mark_parse_fail();
        return std::nullopt;
    }
    if (s[index] != 'i') {
        mark_parse_fail();
        return std::nullopt;
    }
    advance();
    return ret;
}
void NumberParser::advance()
{
    index++;
}
bool NumberParser::is_digit()
{
    return iswdigit(s[index]);
}
bool NumberParser::is_sign(size_t i)
{
    return s[i] == '+' || s[i] == '-';
}
void NumberParser::mark_parse_fail()
{
    success = false;
}
bool NumberParser::parse_fail() const
{
    return !success;
}
Char NumberParser::last_char() const
{
    return s.at(max_len - 1);
}
bool NumberParser::exceed_max_len() const
{
    return index >= max_len;
}
bool NumberParser::reach_last() const
{
    return index == max_len;
}
bool NumberParser::has_sign_after(size_t i)
{
    while (i < max_len) {
        if (is_sign(i)) {
            return true;
        }
        i++;
    }
    return false;
}
Number NumberParser::parsed_number() const
{
    return num;
}
std::optional<Float> NumberParser::convert_str_to_float(const String& str)
{
    errno = 0;
    wchar_t* end;
    double x = std::wcstod(str.c_str(), &end);
    // TODO: use big number
    if (errno == ERANGE) { // Ignore it for denormals
        if (!(x != 0 && x > -HUGE_VAL && x < HUGE_VAL)) {
            mark_parse_fail();
            DEBUG_OUTPUT("strtod: ERANGE");
            return std::nullopt;
        }
    } else if (errno) {
        mark_parse_fail();
        DEBUG_OUTPUT("strtod failed");
        return std::nullopt;
    }
    return x;
}
} // namespace pscm
