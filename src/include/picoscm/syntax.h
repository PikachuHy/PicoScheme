/********************************************************************************/
/**
 * @file syntax.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#ifndef PICOSCHEME_SYNTAX_H
#define PICOSCHEME_SYNTAX_H

#include "cell.hpp"

namespace pscm {
class Syntax {
public:
    void add(const Cell& cell, const Procedure& proc);
    const Procedure& match(const Cell& cell);

private:
    bool can_match(Cell cell, Cell pattern);

private:
    CellHashMap<Procedure> _macro;
};
} // namespace pscm

#endif // PICOSCHEME_SYNTAX_H
