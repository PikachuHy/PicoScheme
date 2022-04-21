/*********************************************************************************/
/**
 * @file promise.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_PROMISE_H
#define PICOSCHEME_PROMISE_H
#include <utility>

#include "procedure.hpp"
namespace pscm {
class Cell;
class Scheme;
class Promise {
public:
    Promise(Procedure proc)
        : proc(std::move(proc))
    {
    }

    Cell force(Scheme& scm, const SymenvPtr& env);

    bool operator==(const Promise& rhs) const
    {
        return proc == rhs.proc;
    }
    bool operator!=(const Promise& rhs) const
    {
        return !(rhs == *this);
    }

    struct hash {
        using argument_type = pscm::Promise;
        using result_type = std::size_t;

        result_type operator()(const Promise& p) const
        {
            return Procedure::hash{}(p.proc);
        }
    };
    template <typename OStream>
    friend OStream& operator<<(OStream& os, const Promise& p)
    {
        return os << "promise: " << p.proc;
    }

private:
    Procedure proc;
};
} // namespace pscm
#endif // PICOSCHEME_PROMISE_H
