/*********************************************************************************/
/**
 * @file promise.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#include "picoscm/promise.h"
#include "picoscm/cell.hpp"
#include "picoscm/scheme.hpp"

namespace pscm {

Cell Promise::force(Scheme& scm, const SymenvPtr& env) {
    auto [new_env, args] = proc.apply(scm, nullptr, nil);
    auto expr = scm.syntax_begin(new_env, args);
    return scm.eval(new_env, expr);
}
} // namespace pscm