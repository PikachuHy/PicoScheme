/********************************************************************************/
/**
 * @file compiler.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_COMPILER_H
#define PICOSCHEME_COMPILER_H
#include "cell.hpp"
#include <set>
#include <utility>

namespace pscm {
class CompiledCode {
public:
private:
};
enum class Instruction {
    GOTO,
    ASSIGN,
    PERFORM,
    TEST,
    BRANCH,
    SAVE,
    RESTORE,
    LABEL,
};
enum class Register {
    EXP,
    FUN,
    PROC,
    ENV,
    CONTINUE,
    ARGL,
    VAL,

};
enum class LinkageEnum {
    NEXT,
    RETURN,
};
enum class LabelEnum {
    TRUE_BRANCH,
    FALSE_BRANCH,
    AFTER_IF,
    PROC_RETURN,
    PRIMITIVE_BRANCH,
    COMPILED_BRANCH,
    AFTER_CALL,
    ENTRY,
    AFTER_LAMBDA
};

struct Label {
    LabelEnum type;
    Int num;

    bool operator==(const Label& rhs) const {
        return type == rhs.type && num == rhs.num;
    }

    bool operator!=(const Label& rhs) const {
        return !(rhs == *this);
    }
};
using Linkage = std::variant<LinkageEnum, Label>;
using Operand = std::variant<Cell, Register, Label>;
using InstCode = std::variant<Instruction, Operand>;
using Target = Register;
using Regs = std::vector<Register>;
using CodeList = std::vector<InstCode>;

struct InstSeq {
    InstSeq(Regs needs, Regs modifies, CodeList statements)
        : needs(std::move(needs))
        , modifies(std::move(modifies))
        , statements(std::move(statements)) {
    }

    InstSeq() {
    }

    InstSeq(InstCode code) {
        statements.push_back(code);
    }

    InstSeq(InstCode code1, InstCode code2) {
        statements.push_back(code1);
        statements.push_back(code2);
    }

    Regs needs;
    Regs modifies;
    CodeList statements;
};

class compiler_error : public std::exception {
public:
    compiler_error(const std::string& msg, Cell cell);
    compiler_error(const std::string& msg, const InstCode& code);

    const char *what() const noexcept override;

private:
    std::string reason;
};
class CompilerPrivate;

class Compiler {
public:
    Compiler(Scheme& scm);
    CompiledCode compile(Scheme& scm, const SymenvPtr& env, Cell cell);

private:
    std::shared_ptr<CompilerPrivate> c;
};

} // namespace pscm

namespace std {
template <>
struct hash<pscm::Label> {
    size_t operator()(const pscm::Label& label) const {
        return hash<pscm::LabelEnum>()(label.type) + hash<pscm::Int>()(label.num);
    }
};
} // namespace std

namespace pscm {

class Machine {
public:
    Machine(Scheme& scm)
        : scm(scm) {
    }

    void run(const CodeList& code_list, const SymenvPtr& env);

    void print_reg() const;

private:
    void fill_label_map(const CodeList& code_list);

private:
    Scheme& scm;
    std::unordered_map<Register, Cell> reg;
    std::stack<Cell> stack;
    std::unordered_map<Label, Int> label_map;
    CodeList all_code_list;
    friend class CodeRunner;
    friend class CompiledProcedureImpl;
};
} // namespace pscm
#endif // PICOSCHEME_COMPILER_H
