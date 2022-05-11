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
enum class Instruction {
    GOTO,
    ASSIGN,
    PERFORM,
    TEST,
    BRANCH,
    SAVE,
    RESTORE,
    LABEL,
    CONT,
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

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Instruction& inst) {
    switch (inst) {
    case Instruction::GOTO:
        os << "goto";
        break;
    case Instruction::ASSIGN:
        os << "assign";
        break;
    case Instruction::PERFORM:
        os << "perform";
        break;
    case Instruction::TEST:
        os << "test";
        break;
    case Instruction::BRANCH:
        os << "branch";
        break;
    case Instruction::SAVE:
        os << "save";
        break;
    case Instruction::RESTORE:
        os << "restore";
        break;
    case Instruction::LABEL:
        os << "label";
        break;
    case Instruction::CONT:
        os << "cont";
        break;
    }
    return os;
}

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Register& reg) {
    switch (reg) {
    case Register::VAL:
        os << "val";
        break;
    case Register::EXP:
        os << "exp";
        break;
    case Register::FUN:
        os << "fun";
        break;
    case Register::PROC:
        os << "proc";
        break;
    case Register::ENV:
        os << "env";
        break;
    case Register::CONTINUE:
        os << "continue";
        break;
    case Register::ARGL:
        os << "argl";
        break;
    }
    return os;
}

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, LinkageEnum link) {
    switch (link) {
    case LinkageEnum::NEXT:
        os << "next";
        break;
    case LinkageEnum::RETURN:
        os << "return";
        break;
    }
    return os;
}

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Label& label) {
    switch (label.type) {
    case LabelEnum::TRUE_BRANCH:
        os << "true-branch";
        break;
    case LabelEnum::FALSE_BRANCH:
        os << "false-branch";
        break;
    case LabelEnum::AFTER_IF:
        os << "after-if";
        break;
    case LabelEnum::PROC_RETURN:
        os << "proc-return";
        break;
    case LabelEnum::PRIMITIVE_BRANCH:
        os << "primitive-branch";
        break;
    case LabelEnum::COMPILED_BRANCH:
        os << "compiled-branch";
        break;
    case LabelEnum::AFTER_CALL:
        os << "after-call";
        break;
    case LabelEnum::ENTRY:
        os << "entry";
        break;
    case LabelEnum::AFTER_LAMBDA:
        os << "after-lambda";
        break;
    }
    os << label.num;
    return os;
}

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Operand& operand) {
    overloads stream{
        // clang-format off
        [&os](const Cell& arg)         { os << arg; },
        [&os](const Register& arg)     { os << arg; },
        [&os](const Label& arg)        { os << arg; }
        // clang-format on
    };
    std::visit(stream, operand);
    return os;
}

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const InstCode& code) {
    overloads stream{
        // clang-format off
        [&os](const Instruction& arg) { os << arg; },
        [&os](const Operand& arg)     { os << arg; },
        [&os](const Label& arg)       { os << arg; }
        // clang-format on
    };
    std::visit(stream, code);
    return os;
}

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

class CompiledCode {
public:
    CompiledCode(sptr<InstSeq> code)
        : code(std::move(code)) {
    }

    CompiledCode(const InstSeq& seq);

    CompiledCode();

private:
    sptr<InstSeq> code;
    friend class Machine;
};

class CompilerImpl;

class Compiler {
public:
    Compiler(Scheme& scm, const SymenvPtr& env);
    CompiledCode compile(const Cell& cell);

private:
    sptr<CompilerImpl> c;
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

#endif // PICOSCHEME_COMPILER_H
