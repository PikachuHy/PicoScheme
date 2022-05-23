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
#include "label.h"
#include <ostream>
#include <set>
#include <utility>

namespace pscm {
enum class Instruction {
    NOOP,
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

struct Comment {
    String msg;
    Cell cell;

    template <typename CharT>
    friend std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Comment& comment) {
        os << ";;; " << comment.msg << " " << comment.cell;
        return os;
    }
};

using Linkage = std::variant<LinkageEnum, Label>;
using InstCode = std::variant<Instruction, Cell, Register, Comment>;
using Target = Register;
using Regs = std::vector<Register>;

class CodeList {
public:
    CodeList() {
        tail = &dummy;
        last_visited = nullptr;
        total = 0;
    }

    CodeList(std::initializer_list<InstCode> l) {
        tail = &dummy;
        last_visited = nullptr;
        total = 0;
        for (const auto& it : l) {
            push_back(it);
        }
    }

    struct Node {
        Node(InstCode code = Instruction::NOOP, Node *next = nullptr)
            : code(std::move(code))
            , next(next) {
        }

        InstCode code;
        Node *next;
    };

    template <typename T, typename... Args>
    void push_back(T code, Args&&...args) {
        push_back(code);
        push_back(std::forward<Args>(args)...);
    }

    template <typename T>
    void push_back(T code) {
        tail->next = new Node(code);
        tail = tail->next;
        total++;
    }

    template <typename T, typename... Args>
    void push_front(T code, Args&&...args) {
        push_front(std::forward<Args>(args)...);
        push_front(code);
    }

    template <typename T>
    void push_front(T code) {
        dummy.next = new Node(std::move(code), dummy.next);
        total++;
    }

    CodeList& merge(const CodeList& l) {
        total += l.total;
        tail->next = l.dummy.next;
        tail = l.tail;
        return *this;
    }

    std::size_t size() const {
        return total;
    }

    bool empty() const noexcept {
        return dummy.next == nullptr;
    }

    InstCode operator[](std::size_t index) const {
        if (last_visited && last_visited_index < index) {
            while (last_visited && last_visited_index < index) {
                last_visited_index++;
                last_visited = last_visited->next;
            }
            return last_visited_code(index);
        }

        reset_last_visited(index);
        last_visited_index = index;
        return last_visited_code(index);
    }

    Node *tail;

    struct Iterator {
        Iterator(Node *head)
            : it(head) {
        }

        Iterator& operator++() {
            it = it->next;
            return *this;
        }

        InstCode operator*() {
            return it->code;
        }

        bool operator==(const Iterator& rhs) const {
            return it == rhs.it;
        }

        bool operator!=(const Iterator& rhs) const {
            return !(rhs == *this);
        }

        Node *it;
    };

    Iterator begin() {
        return Iterator(dummy.next);
    }

    Iterator end() {
        return Iterator(nullptr);
    }

private:
    void reset_last_visited(std::size_t index) const {
        last_visited = dummy.next;
        for (int i = 0; last_visited && i < index; ++i) {
            last_visited = last_visited->next;
        }
    }

    InstCode last_visited_code(std::size_t index) const {
        if (last_visited) {
            return last_visited->code;
        }
        throw std::out_of_range(std::to_string(index));
    }

private:
    Node dummy;
    std::size_t total;
    mutable Node *last_visited;
    mutable std::size_t last_visited_index;
};

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Instruction& inst) {
    switch (inst) {
    case Instruction::NOOP:
        os << "noop";
        break;
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
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const InstCode& code) {
    overloads stream{
        // clang-format off
        [&os](const Instruction& arg)   { os << arg; },
        [&os](const Cell& arg)          { os << arg; },
        [&os](const Register& arg)      { os << arg; },
        [&os](const Comment& arg)       { os << arg; }
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

    InstSeq(std::initializer_list<InstCode> codes) {
        for (const auto& code : codes) {
            statements.push_back(code);
        }
    }

    Regs needs;
    Regs modifies;
    CodeList statements;
};

class compiler_error : public std::exception {
public:
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
