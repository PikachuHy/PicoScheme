//
// Created by PikachuHy on 2022/5/9.
//

#ifndef PICOSCHEME_COMPILEDPROCEDUREIMPL_H
#define PICOSCHEME_COMPILEDPROCEDUREIMPL_H
#include "picoscm/compiler.h"
#include "picoscm/number.hpp"

namespace pscm {
class MachineImpl;

struct CompiledProcedureImpl {
    CompiledProcedureImpl(MachineImpl& m, Label label, SymenvPtr senv, bool is_macro)
        : m(m)
        , label(label)
        , senv(senv)
        , is_macro(is_macro) {
    }

    MachineImpl& m;
    Label label;
    SymenvPtr senv;
    bool is_macro;

    bool operator==(const CompiledProcedureImpl& rhs) const {
        return label == rhs.label && senv == rhs.senv;
    }

    bool operator!=(const CompiledProcedureImpl& rhs) const {
        return !(rhs == *this);
    }

    Int entry() const;
};
} // namespace pscm
#endif // PICOSCHEME_COMPILEDPROCEDUREIMPL_H
