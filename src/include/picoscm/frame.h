/********************************************************************************/
/**
 * @file frame.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#ifndef PICOSCHEME_FRAME_H
#define PICOSCHEME_FRAME_H
#include <stack>
#include <utility>

#include "cell.hpp"
#include "procedure.hpp"
#include "types.hpp"

namespace pscm {
class Frame {
public:
    Frame(SymenvPtr env, Cell cell)
        : m_env(env)
        , m_expr(cell) {
    }

    const SymenvPtr& env() const {
        return m_env;
    }

    const Cell& expr() const {
        return m_expr;
    }

    const Cell& args() const {
        return cdr(m_expr);
    }

    const Cell& op() const {
        return car(m_expr);
    }

    void push_arg(Cell cell) {
        m_args_stack.push_back(cell);
    }

    [[nodiscard]] int arg_count() const {
        return m_args_stack.size();
    }

    [[nodiscard]] const std::vector<Cell>& varg() const {
        return m_args_stack;
    }

private:
    SymenvPtr m_env;
    Cell m_expr;
    std::vector<Cell> m_args_stack;
};
} // namespace pscm
#endif // PICOSCHEME_FRAME_H
