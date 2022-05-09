//
// Created by PikachuHy on 2022/5/9.
//

#ifndef PICOSCHEME_MACHINEIMPL_H
#define PICOSCHEME_MACHINEIMPL_H
#include "picoscm/compiler.h"
#include <stack>
#include <unordered_map>

namespace pscm {
class Scheme;

class MachineImpl {
public:
    MachineImpl(Scheme& scm)
        : scm(scm) {
    }

    Cell run(const CodeList& code_list, const SymenvPtr& env);

    void print_reg() const;

private:
    void fill_label_map(const CodeList& code_list);

private:
    Scheme& scm;
    std::unordered_map<Register, Cell> reg;
    std::stack<Cell> stack;
    std::unordered_map<Label, Int> label_map;
    CodeList all_code_list;
    friend class Machine;
    friend class CodeRunner;
    friend class CompiledProcedureImpl;
};

} // namespace pscm
#endif // PICOSCHEME_MACHINEIMPL_H
