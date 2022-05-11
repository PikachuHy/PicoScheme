/********************************************************************************/
/**
 * @file continuation.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_CONTINUATION_H
#define PICOSCHEME_CONTINUATION_H
#include "compiler.h"
#include "frame.h"
#include <utility>
#include <vector>

namespace pscm {
class Continuation {
public:
    explicit Continuation(FrameStack frames)
        : m_frames(std::move(frames)) {
        m_frames.pop_back();
    };

    Continuation(FrameStack frames, SymenvPtr& env, Cell cell)
        : m_frames(std::move(frames)) {
        m_frames.pop_back();
        m_frames.emplace_back(env, cell);
    };

    Continuation(std::stack<Cell> stack, std::unordered_map<Register, Cell> reg)
        : m_stack(std::move(stack))
        , m_reg(std::move(reg)) {
    }

    [[nodiscard]] const FrameStack& frames() const {
        return m_frames;
    }

    [[nodiscard]] const std::stack<Cell>& stack() const {
        return m_stack;
    }

    [[nodiscard]] const std::unordered_map<Register, Cell>& reg() const {
        return m_reg;
    }

private:
    FrameStack m_frames;
    std::stack<Cell> m_stack;
    std::unordered_map<Register, Cell> m_reg;
};
} // namespace pscm
#endif // PICOSCHEME_CONTINUATION_H
