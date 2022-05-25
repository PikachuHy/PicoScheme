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
#include "dynamic_wind.h"
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

    Continuation(std::vector<Cell> stack, std::unordered_map<Register, Cell> reg, std::vector<DynamicWind> wind)
        : m_stack(std::move(stack))
        , m_reg(std::move(reg))
        , m_wind(std::move(wind)) {
    }

    [[nodiscard]] const FrameStack& frames() const {
        return m_frames;
    }

    [[nodiscard]] const std::vector<Cell>& stack() const {
        return m_stack;
    }

    [[nodiscard]] const std::unordered_map<Register, Cell>& reg() const {
        return m_reg;
    }

    [[nodiscard]] const std::vector<DynamicWind>& wind() const {
        return m_wind;
    }

private:
    FrameStack m_frames;
    std::vector<Cell> m_stack;
    std::unordered_map<Register, Cell> m_reg;
    std::vector<DynamicWind> m_wind;
};
} // namespace pscm
#endif // PICOSCHEME_CONTINUATION_H
