/********************************************************************************/
/**
 * @file machine.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_MACHINE_H
#define PICOSCHEME_MACHINE_H
#include "types.hpp"

namespace pscm {
class Scheme;
class MachineImpl;

class Machine {
public:
    Machine(Scheme& scm);
    Cell run(SymenvPtr env, Cell expr);

private:
    sptr<MachineImpl> impl;
};

} // namespace pscm

#endif // PICOSCHEME_MACHINE_H
