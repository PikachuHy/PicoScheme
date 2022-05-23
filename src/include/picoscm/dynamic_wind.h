/********************************************************************************/
/**
 * @file label.h
 *
 * @version   0.1
 * @date      2018-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_DYNAMIC_WIND_H
#define PICOSCHEME_DYNAMIC_WIND_H
#include "procedure.hpp"

namespace pscm {
struct DynamicWind {
    DynamicWind(Procedure before, Procedure thunk, Procedure after)
        : before(std::move(before))
        , thunk(std::move(thunk))
        , after(std::move(after)) {
    }

    Procedure before;
    Procedure thunk;
    Procedure after;
};
} // namespace pscm
#endif // PICOSCHEME_DYNAMIC_WIND_H
