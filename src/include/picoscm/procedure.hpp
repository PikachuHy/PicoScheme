/********************************************************************************/
/**
 * @file procedure.hpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#ifndef PROCEDURE_HPP
#define PROCEDURE_HPP

#include <functional>
#include <ostream>
#include <utility>

#include "label.h"
#include "types.hpp"

namespace pscm {

class Scheme;

/**
 * Procedure type to represent a scheme closure.
 *
 * @verbatim
 * (lambda args body)  => closure: [symenv, args, body](define *world* '())
 *  with:
 *  args        := [symbol | nil | list | dotted-list]
 *  list        := nil | (expr . list)
 *  dotted-list := (list . expr)
 *  body        := (expr . list)
 * @endverbatim
 */
class Procedure {
public:
    /**
     * Construct a new closure.
     * @param senv  Symbol environment pointer to capture.
     * @param args  Formal lambda expression argument list or symbol.
     * @param code  Non empty list of one or more scheme expression forming the lambda body.
     */
    Procedure(const SymenvPtr& senv, const Cell& args, const Cell& code, bool is_macro = false);
    Procedure(const SymenvPtr& senv, const Cell& args, const Cell& code, const Label& label, bool is_macro = false);
    Procedure(const SymenvPtr& senv, const Cell& args, const Label& label, bool is_macro = false);
    Procedure(const SymenvPtr& senv, const Label& label, bool is_macro = false);
    /// Predicate returns true if closure should be applied as macro.
    bool is_macro() const noexcept;

    SymenvPtr senv() const noexcept;
    Cell args() const noexcept;
    Cell code() const noexcept;
    Label entry() const noexcept;

    void compile(Scheme& scm);

    bool operator!=(const Procedure& proc) const noexcept;
    bool operator==(const Procedure& proc) const noexcept;

    struct Closure;

    struct hash : private std::hash<Closure *> {
        using argument_type = Procedure;
        using result_type = std::size_t;

        result_type operator()(const Procedure& proc) const noexcept {
            return std::hash<Closure *>::operator()(proc.impl.get());
        }
    };
private:
    std::shared_ptr<Closure> impl;
};

/**
 * Functor wrapper for external function objects.
 *
 * External function signature:
 *   func(Scheme& scm, const SymenvPtr& env, const std::vector<Cell>& argv) -> Cell
 */
class Function : public std::function<Cell(Scheme&, const SymenvPtr&, const std::vector<Cell>&)> {

    using function_type = std::function<Cell(Scheme&, const SymenvPtr&, const std::vector<Cell>&)>;

public:
    template <typename FunctionT>
    static FunctionPtr create(const Symbol& sym, FunctionT&& fun) {
        return std::shared_ptr<Function>{
            new Function{sym, function_type{ std::forward<FunctionT>(fun) }}
        };
    }

    const String& name() const {
        return sym.value();
    };

protected:
    /**
     * Function object constructor
     * @param sym Symbol bound to this function.
     * @param fun External procedure.
     */
    Function(const Symbol& sym, function_type&& fun)
        : function_type{ std::move(fun) }
        , sym{ sym } {
    }

private:
    Symbol sym;
};

} // namespace pscm

#endif // PROCEDURE_HPP
