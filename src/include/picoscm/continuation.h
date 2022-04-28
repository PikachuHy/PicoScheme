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

    [[nodiscard]] const FrameStack& frames() const {
        return m_frames;
    }

private:
    FrameStack m_frames;
};
} // namespace pscm
#endif // PICOSCHEME_CONTINUATION_H
