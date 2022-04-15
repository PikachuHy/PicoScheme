/********************************************************************************/
/**
 * @file syntax.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#include "picoscm/syntax.h"
#include "picoscm/port.hpp"

namespace pscm {

void Syntax::add(const Cell& cell, const Procedure& proc)
{
    _macro.insert_or_assign(cell, proc);
}


const Procedure& Syntax::match(const Cell& cell)
{
    for(const auto& [k, v]: _macro) {
        if (can_match(cell, k)) {
            return v;
        }
    }
    DEBUG_OUTPUT("args:", cell);
    throw std::runtime_error("can't match");
}

bool Syntax::can_match(Cell cell, Cell pattern)
{
    while (is_pair(cell) && is_pair(pattern)) {
        cell = cdr(cell);
        pattern = cdr(pattern);
    }
    if (is_nil(cell) && is_nil(pattern)) {
        return true;
    }
    return false;
}
}