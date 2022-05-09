/********************************************************************************/
/**
 * @file machine.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#include "picoscm/machine.h"
#include "picoscm/compiler.h"
#include "picoscm/scheme.hpp"

#include "impl/CompiledProcedureImpl.h"
#include "impl/MachineImpl.h"

namespace pscm {

bool is_reg(const Operand& operand) {
    return is_type<Register>(operand);
}

bool is_reg(const InstCode& code) {
    return is_type<Operand>(code) && is_type<Register>(get<Operand>(code));
}

Register get_reg(const InstCode& code) {
    return get<Register>(get<Operand>(code));
}

bool is_op(const Operand& operand) {
    return is_type<Cell>(operand) && is_type<Intern>(get<Cell>(operand));
}

Intern get_op(const Operand& operand) {
    return get<Intern>(get<Cell>(operand));
}

bool is_op(const InstCode& code) {
    return is_type<Operand>(code) && is_op(get<Operand>(code));
}

Intern get_op(const InstCode& code) {
    return get_op(get<Operand>(code));
}

bool is_label(const InstCode& code) {
    return is_type<Operand>(code) && is_type<Label>(get<Operand>(code));
}

Label get_label(const InstCode& code) {
    return get<Label>(get<Operand>(code));
}

bool is_inst(const InstCode& code) {
    return is_type<Instruction>(code);
}

class CodeListPrinter {
public:
    CodeListPrinter(const InstSeq& seq)
        : code_list(seq.statements) {
    }

    void print() {
        reset();
        /*
        for(auto code: code_list) {
            print_code(code);
            std::wcout << std::endl;
        }
        DEBUG_OUTPUT("print format");
         */
        print_code_list();
    }

private:
    void reset() {
        i = 0;
    }

    void print_label(Cell label) {
        std::wcout << car(label);
        std::wcout << cdr(label);
    }

    void print_args(int n) {
        while (n > 0) {
            i++;
            n--;
            std::wcout << " ";
            print_code(code_list[i]);
        }
    }

    void print_op() {
        auto op = get_op(code_list[i]);
        switch (op) {
        case Intern::op_make_compiled_procedure: {
            std::wcout << "make-compiled-procedure";
            print_args(2);
            break;
        }
        case Intern::op_compiled_procedure_env: {
            std::wcout << "compiled-procedure-env";
            print_args(1);
            break;
        }
        case Intern::op_compiled_procedure_entry: {
            std::wcout << "compiled-procedure-entry";
            print_args(1);
            break;
        }
        case Intern::op_extend_environment: {
            std::wcout << "extend-environment";
            print_args(3);
            break;
        }
        case Intern::op_define_variable: {
            std::wcout << "define-variable!";
            print_args(3);
            break;
        }
        case Intern::op_list: {
            std::wcout << "list";
            print_args(1);
            break;
        }
        case Intern::op_lookup_variable_value: {
            std::wcout << "lookup-variable-value";
            print_args(2);
            break;
        }
        case Intern::op_cons: {
            std::wcout << "cons";
            print_args(2);
            break;
        }
        case Intern::op_is_primitive_procedure: {
            std::wcout << "primitive-procedure?";
            print_args(1);
            break;
        }
        case Intern::op_apply_primitive_procedure: {
            std::wcout << "apply-primitive-procedure";
            print_args(2);
            break;
        }
        case Intern::op_is_false: {
            std::wcout << "false?";
            print_args(1);
            break;
        }
        default: {
            DEBUG_OUTPUT("unknown op:", op);
            throw std::runtime_error("unknown op");
        }
        }
    }

    void print_code(const InstCode& code) {
        std::wcout << code;
    }

    void print_assign() {
        auto code = code_list[i];
        std::cout << "assign";
        print_args(1);
        if (is_op(code_list[i + 1])) {
            i++;
            std::wcout << " ";
            print_op();
        }
        else {
            print_args(1);
        }
    }

    void print_inst() {
        auto code = code_list[i];
        auto inst = get<Instruction>(code);
        switch (inst) {
        case Instruction::ASSIGN:
            print_assign();
            break;
        case Instruction::GOTO:
            std::wcout << "goto ";
            i++;
            print_code(code_list[i]);
            break;
        case Instruction::SAVE:
            std::wcout << "save ";
            i++;
            print_code(code_list[i]);
            break;
        case Instruction::PERFORM:
            std::wcout << "perform ";
            i++;
            print_op();
            break;
        case Instruction::TEST:
            std::wcout << "test ";
            if (is_op(code_list[i + 1])) {
                i++;
                print_op();
            }
            else {
                print_code(code_list[i]);
            }
            break;
        case Instruction::BRANCH:
            std::wcout << "branch ";
            i++;
            print_code(code_list[i]);
            break;
        case Instruction::RESTORE:
            std::wcout << "restore ";
            i++;
            print_code(code_list[i]);
            break;
        case Instruction::LABEL:
            i++;
            std::wcout << "\b\b";
            print_code(code_list[i]);
            std::wcout << ":";
            break;
        default: {
            DEBUG_OUTPUT("unknown inst:", inst);
            throw std::runtime_error("unknown inst");
        }
        }
    }

    void print_code_list() {
        while (i < code_list.size()) {
            const auto& code = code_list[i];
            if (is_inst(code)) {
                std::wcout << "  ";
                print_inst();
            }
            else {
                print_code(code);
            }
            std::wcout << std::endl;
            i++;
        }
    }

private:
    const CodeList& code_list;
    int i;
};

class CodeRunner {
public:
    CodeRunner(MachineImpl& m, const CodeList& code_list)
        : m(m)
        , code_list(code_list) {
    }

    void run(int pos);

private:
    InstCode fetch_code() {
        if (i + 1 > code_list.size()) {
            DEBUG_OUTPUT("error: overflow", i + 1, "/", code_list.size());
        }
        i++;
        return code_list[i];
    }

    Register fetch_reg() {
        return get_reg(fetch_code());
    }

    Operand fetch_operand() {
        auto code = fetch_code();
        if (is_type<Operand>(code)) {
            return get<Operand>(code);
        }
        DEBUG_OUTPUT("error:", code, "is not operand");
        throw std::runtime_error("error");
    }

    Cell fetch_cell() {
        auto operand = fetch_operand();
        if (is_type<Cell>(operand)) {
            return get<Cell>(operand);
        }
        DEBUG_OUTPUT("error:", operand, "is not cell");
        throw std::runtime_error("error");
    }

    Label fetch_label() {
        return get_label(fetch_code());
    }

    void assign_reg(Register r, const Operand& v);

    void run_inst(Instruction inst);

    Cell run_op(Intern op);

private:
    MachineImpl& m;
    const CodeList& code_list;
    int i;
};

void CodeRunner::assign_reg(Register r, const Operand& v) {
    if (is_reg(v)) {
        auto r2 = get<Register>(v);
        m.reg[r] = m.reg[r2];
    }
    else {
        auto v2 = get<Cell>(v);
        m.reg[r] = v2;
    }
}

void CodeRunner::run(int pos) {
    i = pos;
    do {
        auto code = fetch_code();
        if (is_inst(code)) {
            auto inst = get<Instruction>(code);
            run_inst(inst);
        }
        else {
            DEBUG_OUTPUT("wrong inst code:");
            for (int j = std::max(0, i - 2); j < std::min(int(code_list.size()), i + 2); ++j) {
                std::wcout << code_list[j];
                if (i == j) {
                    std::wcout << " <----";
                }
                std::wcout << std::endl;
            }
            std::wcout << "Register:" << std::endl;
            for (const auto& [k, v] : m.reg) {
                std::wcout << k << ": " << v << std::endl;
            }
            throw std::runtime_error("bad instruction");
        }
    } while (i + 1 < code_list.size());
}

Cell CodeRunner::run_op(Intern op) {
    switch (op) {
    case Intern::op_lookup_variable_value: {
        auto v = fetch_operand();
        auto r = fetch_reg();
        std::wcout << "lookup-variable-value " << v << " " << r;
        if (is_type<Cell>(v)) {
            auto vv = get<Cell>(v);
            if (!is_symbol(vv)) {
                DEBUG_OUTPUT("error:", vv);
                std::cout << boost::stacktrace::stacktrace();
                std::cout << std::endl;
                throw std::runtime_error("error");
            }
            auto sym = get<Symbol>(vv);
            auto env = get<SymenvPtr>(m.reg[r]);
            std::wcout << std::endl;
            std::wcout << "env:" << env;
            return env->get(sym);
        }
        DEBUG_OUTPUT("error operand:", v);
        throw std::runtime_error("error");
    }
    case Intern::op_list: {
        auto r = fetch_reg();
        auto v = m.reg[r];
        std::wcout << "list " << r;
        return m.scm.cons(v, nil);
    }
    case Intern::op_cons: {
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        std::wcout << "cons " << r1 << " " << r2;
        return m.scm.cons(m.reg[r1], m.reg[r2]);
    }
    case Intern::op_is_primitive_procedure: {
        auto v = fetch_operand();
        std::wcout << "primitive-procedure? " << v;
        Cell vv;
        if (is_type<Cell>(v)) {
            vv = get<Cell>(v);
        }
        else if (is_reg(v)) {
            auto r = get_reg(v);
            vv = m.reg[r];
        }
        else {
            DEBUG_OUTPUT("error:", v);
            std::cout << boost::stacktrace::stacktrace();
            std::cout << std::endl;
            throw std::runtime_error("error");
        }
        return is_intern(vv);
    }
    case Intern::op_apply_primitive_procedure: {
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        std::wcout << "apply-primitive-procedure " << r1 << " " << r2;
        auto proc = m.reg[r1];
        auto argl = m.reg[r2];
        std::vector<Cell> args;
        auto it = argl;
        while (is_pair(it)) {
            args.push_back(car(it));
            it = cdr(it);
        }
        auto env = get<SymenvPtr>(m.reg[Register::ENV]);
        return m.scm.apply(env, get<Intern>(proc), args);
    }
    case Intern::op_make_compiled_procedure: {
        auto label = fetch_label();
        auto r = fetch_reg();
        std::wcout << "make-compiled-procedure " << label << " " << r;
        auto env = get<SymenvPtr>(m.reg[r]);
        auto proc = std::make_shared<CompiledProcedureImpl>(m, label, env);
        return proc;
    }
    case Intern::op_compiled_procedure_env: {
        auto v = fetch_operand();
        std::wcout << "compiled-procedure-env " << v;
        if (is_type<Cell>(v)) {
            auto vv = get<Cell>(v);
            if (is_type<CompiledProcedure>(vv)) {
                auto proc = get<CompiledProcedure>(vv);
                return proc.env();
            }
        }
        else if (is_reg(v)) {
            auto r = get_reg(v);
            auto vv = m.reg[r];
            if (is_type<CompiledProcedure>(vv)) {
                auto proc = get<CompiledProcedure>(vv);
                return proc.env();
            }
        }
        DEBUG_OUTPUT("error operand:", v);
        m.print_reg();
        throw std::runtime_error("error");
    }
    case Intern::op_compiled_procedure_entry: {
        auto r = fetch_reg();
        std::wcout << "compiled-procedure-entry " << r;
        auto v = m.reg[r];
        if (is_type<CompiledProcedure>(v)) {
            auto proc = get<CompiledProcedure>(v);
            return proc.entry();
        }
        DEBUG_OUTPUT("error operand:", v);
        m.print_reg();
        throw std::runtime_error("error");
    }
    case Intern::op_extend_environment: {
        auto v = fetch_cell();
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        std::wcout << "extend-environment " << v << " " << r1 << " " << r2;
        auto argl = m.reg[r1];
        auto env = get<SymenvPtr>(m.reg[r2]);
        auto new_env = Symenv::create(env);
        while (is_pair(v) && is_pair(argl)) {
            auto sym = get<Symbol>(car(v));
            new_env->add(sym, car(argl));
            v = cdr(v);
            argl = cdr(argl);
        }
        return new_env;
    }
    case Intern::op_define_variable: {
        auto v = fetch_operand();
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        std::wcout << "define-variable! " << r1 << " " << r2;
        auto val = m.reg[r1];
        auto env = get<SymenvPtr>(m.reg[r2]);
        if (is_type<Cell>(v)) {
            auto vv = get<Cell>(v);
            if (is_symbol(vv)) {
                auto sym = get<Symbol>(vv);
                env->add(sym, val);
                return none;
            }
        }
        DEBUG_OUTPUT("error operand:", v);
        m.print_reg();
        throw std::runtime_error("error");
    }
    case Intern::op_is_false: {
        auto r = fetch_reg();
        std::wcout << "false? " << r;
        auto v = m.reg[r];
        return is_false(v);
    }
    default: {
        DEBUG_OUTPUT("unknown op:", op);
        m.print_reg();
        throw std::runtime_error("unknown op");
    }
    }
}

void CodeRunner::run_inst(Instruction inst) {
    switch (inst) {
    case Instruction::ASSIGN: {
        auto r = fetch_reg();
        auto v = fetch_operand();
        std::wcout << "  assign " << r << " ";
        if (is_op(v)) {
            assign_reg(r, run_op(get_op(v)));
        }
        else if (is_label(v)) {
            std::wcout << v;
            auto label = get_label(v);
            auto pos = m.label_map.at(label);
            assign_reg(r, pos);
        }
        else {
            std::wcout << v;
            assign_reg(r, v);
        }
        std::wcout << std::endl;
        std::wcout << "--> " << m.reg.at(r) << std::endl;
        break;
    }
    case Instruction::TEST: {
        auto v = fetch_code();
        std::wcout << "  test ";
        if (is_op(v)) {
            if (is_true(run_op(get_op(v)))) {
            }
            else {
                // skip branch label
                i += 2;
            }
        }
        else {
            std::wcout << v;
        }
        std::wcout << std::endl;
        break;
    }
    case Instruction::LABEL: {
        i += 1;
        std::wcout << code_list[i] << ":" << std::endl;
        break;
    }
    case Instruction::BRANCH: {
        auto v = fetch_label();
        std::wcout << "  branch " << v << std::endl;
        auto pos = m.label_map.at(v);
        i = pos;
        break;
    }
    case Instruction::PERFORM: {
        std::wcout << "  perform ";
        auto v = fetch_operand();
        if (is_op(v)) {
            run_op(get_op(v));
            std::wcout << std::endl;
            break;
        }
        std::wcout << v;
        std::wcout << std::endl;
        DEBUG_OUTPUT("error operand:", v);
        throw std::runtime_error("error");
    }
    case Instruction::GOTO: {
        auto v = fetch_operand();
        std::wcout << "  goto " << v << std::endl;
        if (is_label(v)) {
            auto label = get_label(v);
            auto pos = m.label_map.at(label);
            i = pos;
            break;
        }
        else if (is_reg(v)) {
            auto r = get_reg(v);
            auto vv = m.reg[r];
            if (is_number(vv)) {
                auto num = get<Number>(vv);
                if (is_type<Int>(num)) {
                    auto pos = get<Int>(num);
                    i = pos;
                    break;
                }
            }
        }
        else {
            if (is_type<Cell>(v)) {
                auto vv = get<Cell>(v);
                if (is_type<Number>(vv)) {
                    auto num = get<Number>(vv);
                    if (is_type<Int>(num)) {
                        auto pos = get<Int>(num);
                        i = pos;
                        break;
                    }
                }
            }
        }
        DEBUG_OUTPUT("error operand:", v);
        throw std::runtime_error("error");
    }
    case Instruction::SAVE: {
        auto r = fetch_reg();
        std::wcout << "  save " << r << std::endl;
        m.stack.push(m.reg[r]);
        break;
    }
    case Instruction::RESTORE: {
        auto r = fetch_reg();
        std::wcout << "  restore " << r << std::endl;
        m.reg[r] = m.stack.top();
        m.stack.pop();
        break;
    }
    default: {
        DEBUG_OUTPUT("unknown inst:", inst);
        throw std::runtime_error("unknown inst");
    }
    }
}

Cell MachineImpl::run(const CodeList& code_list, const SymenvPtr& env) {
    DEBUG_OUTPUT("run: ");
    fill_label_map(code_list);
    auto pos = all_code_list.size();
    all_code_list.reserve(all_code_list.size() + code_list.size());
    std::copy(code_list.begin(), code_list.end(), std::back_inserter(all_code_list));
    reg[Register::ENV] = env;
    reg[Register::CONTINUE] = Int(-1);
    CodeRunner runner(*this, all_code_list);
    runner.run(pos - 1);
    std::wcout << reg[Register::VAL] << std::endl;

    return reg[Register::VAL];
}

void MachineImpl::print_reg() const {
    DEBUG_OUTPUT("machine registers:");
    for (const auto& [k, v] : reg) {
        std::wcout << k << " --> " << v << std::endl;
    }
}

void MachineImpl::fill_label_map(const CodeList& code_list) {
    int i = 0;
    while (i < code_list.size()) {
        auto code = code_list[i];
        if (is_inst(code)) {
            auto inst = get<Instruction>(code);
            if (inst == Instruction::LABEL) {
                i++;
                code = code_list[i];
                if (is_label(code)) {
                    auto label = get_label(code);
                    // DEBUG_OUTPUT("label:", label, "-->", i);
                    auto pos = i + all_code_list.size();
                    label_map[label] = pos;
                }
                else {
                    DEBUG_OUTPUT("inst:", code);
                    throw std::runtime_error("inst error");
                }
            }
        }
        i++;
    }
}

Int CompiledProcedureImpl::entry() const {
    auto it = m.label_map.find(label);
    if (it == m.label_map.end()) {
        DEBUG_OUTPUT("error:", label);
        for (const auto& [k, v] : m.label_map) {
            std::wcout << k << " --> " << v << std::endl;
        }
        throw std::runtime_error("error");
    }
    else {
        return it->second;
    }
}

Machine::Machine(Scheme& scm)
    : impl(std::make_shared<MachineImpl>(scm)) {
}

Cell Machine::run(SymenvPtr env, Cell expr) {
    auto code = Compiler(impl->scm, env).compile(expr);
    auto ret = impl->run(code.code->statements, env);
    return ret;
}
} // namespace pscm