/********************************************************************************/
/**
 * @file procedure.hpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <set>

#include "picoscm/compiler.h"
#include "picoscm/procedure.hpp"
#include "picoscm/scheme.h"

namespace pscm {

/**
 *  Test argument list for unique symbols.
 *
 *  Predicate is used to check, that formal parameters of lambda
 *  expression  @verbatim (lambda (x y z ... x) code...) @endverbatim
 *  do not repeat in the argument list.
 */
static bool is_unique_symbol_list(Cell args) {
    using std::get;

    if (is_nil(args) || is_symbol(args))
        return true;

    std::set<Symbol> symset;

    for (/* */; is_pair(args); args = cdr(args)) {
        Cell sym = car(args);

        if (!is_symbol(sym) || !symset.insert(get<Symbol>(sym)).second)
            return false;
    }
    return is_nil(args) || (is_symbol(args) && symset.insert(get<Symbol>(args)).second);
}

/**
 * Closure to capture an environment pointer, a formal
 * argument list and a code list of one or more scheme expressions.
 */
struct Procedure::Closure {

    Closure(const SymenvPtr& senv, const Cell& args, const Cell& code, const Label& label, bool is_macro)
        : senv{ senv }
        , args{ args }
        , code{ code }
        , entry{ label }
        , is_macro{ is_macro } {
        if (!is_unique_symbol_list(args) || !is_pair(code))
            throw std::invalid_argument("invalid procedure definition");
    }

    Closure(const SymenvPtr& senv, const Cell& args, const Label& label, bool is_macro)
        : senv{ senv }
        , args{ args }
        , code{ none }
        , entry{ label }
        , is_macro{ is_macro } {
        if (!is_unique_symbol_list(args))
            throw std::invalid_argument("invalid procedure definition");
    }

    Closure(const SymenvPtr& senv, const Cell& args, const Cell& code, bool is_macro)
        : senv{ senv }
        , args{ args }
        , code{ code }
        , is_macro{ is_macro } {
        if (!is_unique_symbol_list(args) || !is_pair(code))
            throw std::invalid_argument("invalid procedure definition");
    }

    Closure(const SymenvPtr& senv, const Label& label, bool is_macro)
        : senv{ senv }
        , entry{ label }
        , is_macro{ is_macro } {
    }

    bool operator!=(const Closure& rhs) const noexcept {
        return senv != rhs.senv || args != rhs.args || code != rhs.code || entry != rhs.entry ||
               is_macro != rhs.is_macro;
    }

    SymenvPtr senv; //!< Symbol environment pointer.
    Cell args;      //!< Formal parameter symbol list or single symbol.
    Cell code;      //!< Lambda body expression list.
    Label entry;    //!< Compiled code entry.
    bool is_macro;
};

Procedure::Procedure(const SymenvPtr& senv, const Cell& args, const Cell& code, const Label& label, bool is_macro)
    : impl{ std::make_shared<Closure>(senv, args, code, is_macro) } {
}

Procedure::Procedure(const SymenvPtr& senv, const Cell& args, const Label& label, bool is_macro)
    : impl{ std::make_shared<Closure>(senv, args, label, is_macro) } {
}

Procedure::Procedure(const SymenvPtr& senv, const Cell& args, const Cell& code, bool is_macro)
    : impl{ std::make_shared<Closure>(senv, args, code, is_macro) } {
}

Procedure::Procedure(const SymenvPtr& senv, const Label& label, bool is_macro)
    : impl{ std::make_shared<Closure>(senv, label, is_macro) } {
}

SymenvPtr Procedure::senv() const noexcept {
    return impl->senv;
}

Cell Procedure::args() const noexcept {
    return impl->args;
}

Cell Procedure::code() const noexcept {
    return impl->code;
}

Label Procedure::entry() const noexcept {
    return impl->entry;
}

bool Procedure::is_macro() const noexcept {
    return impl->is_macro;
}

bool Procedure::operator!=(const Procedure& proc) const noexcept {
    return *impl != *proc.impl;
}

bool Procedure::operator==(const Procedure& proc) const noexcept {
    return !(*impl != *proc.impl);
}

} // namespace pscm
