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
#include <utility>

#include "cell.hpp"
#include "procedure.hpp"
#include "types.hpp"
namespace pscm {
class Frame {
public:
    Frame(Cell op, const SymenvPtr& env, const Cell& args)
        : m_op(std::move(op)),
          m_env(env),
          m_args(args)
    {
    }
    const Cell& args() const
    {
        return m_args;
    }
    Cell op() const
    {
        return m_op;
    }

private:
    Cell m_op;
    const SymenvPtr& m_env;
    const Cell& m_args;
};
} // namespace pscm
#endif // PICOSCHEME_FRAME_H
