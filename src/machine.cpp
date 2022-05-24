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
#include "picoscm/continuation.h"
#include "picoscm/dynamic_wind.h"
#include "picoscm/promise.h"
#include "picoscm/scheme.hpp"

#include "impl/CodeListPrinter.h"
#include "impl/CompiledProcedureImpl.h"
#include "impl/MachineImpl.h"

namespace pscm {

#define LOG_TRACE(msg)                                                                                                 \
    do {                                                                                                               \
        if (print_trace) {                                                                                             \
            stream << msg;                                                                                             \
            stream.flush();                                                                                            \
        }                                                                                                              \
    } while (0)

#define LF std::endl;

bool is_reg(const InstCode& code) {
    return is_type<Register>(code);
}

Register get_reg(const InstCode& code) {
    return get<Register>(code);
}

bool is_op(const InstCode& code) {
    if (!is_type<Cell>(code)) {
        return false;
    }
    return is_intern(get<Cell>(code));
}

Intern get_op(const InstCode& code) {
    return get<Intern>(get<Cell>(code));
}

bool is_label(const InstCode& code) {
    if (!is_type<Cell>(code)) {
        return false;
    }
    auto cell = get<Cell>(code);
    if (!is_type<Label>(cell)) {
        return false;
    }
    return true;
}

Label get_label(const InstCode& code) {
    return get<Label>(get<Cell>(code));
}
bool is_inst(const InstCode& code) {
    return is_type<Instruction>(code);
}

class bytecode_error : public std::exception {
public:
    template <typename T>
    bytecode_error(std::string msg, const T& code) {
        reason = std::move(msg);
        std::wstringstream ss;
        ss << code;
        reason.append(string_convert<char>(ss.str()));
    }

    [[nodiscard]] const char *what() const noexcept override {
        return reason.c_str();
    }

private:
    std::string reason;
};

void CodeListPrinter::print_op() {
    auto op = get_op(code_list[i]);
    print(" ");
    switch (op) {
    case Intern::op_make_compiled_macro:
    case Intern::op_make_compiled_procedure: {
        print("make-compiled-procedure");
        print_args(3);
        break;
    }
    case Intern::op_compiled_procedure_env: {
        print("compiled-procedure-env");
        print_args(1);
        break;
    }
    case Intern::op_compiled_procedure_entry: {
        print("compiled-procedure-entry");
        print_args(1);
        break;
    }
    case Intern::op_extend_environment: {
        print("extend-environment");
        print_args(3);
        break;
    }
    case Intern::op_define_variable: {
        print("define-variable!");
        print_args(3);
        break;
    }
    case Intern::op_list: {
        print("list");
        print_args(1);
        break;
    }
    case Intern::op_lookup_variable_value: {
        print("lookup-variable-value");
        print_args(2);
        break;
    }
    case Intern::op_cons: {
        print("cons");
        print_args(2);
        break;
    }
    case Intern::op_is_primitive_procedure: {
        print("primitive-procedure?");
        print_args(1);
        break;
    }
    case Intern::op_apply_primitive_procedure: {
        print("apply-primitive-procedure");
        print_args(2);
        break;
    }
    case Intern::op_is_false: {
        print("false?");
        print_args(1);
        break;
    }
    case Intern::op_set_variable_value: {
        print("set!");
        print_args(3);
        break;
    }
    default: {
        print(op);
        //        DEBUG_OUTPUT("unknown op:", op);
        //        throw std::runtime_error("unknown op");
    }
    }
}

void CodeListPrinter::print_code_list() {
    // DEBUG_OUTPUT("print", i, code_list.size());
    while (i < code_list.size()) {
        print_pos();
        const auto& code = code_list[i];
        if (is_inst(code)) {
            print("  ");
            print_inst();
        }
        else {
            print_code(code);
        }
        print_endl();
        i++;
    }
}

void CodeListPrinter::print_inst() {
    auto code = code_list[i];
    auto inst = get<Instruction>(code);
    if (inst != Instruction::LABEL && inst != Instruction::ASSIGN) {
        print(inst);
        print(" ");
    }
    switch (inst) {
    case Instruction::NOOP:
        break;
    case Instruction::ASSIGN:
        print_assign();
        break;
    case Instruction::GOTO:

        i++;
        print_code(code_list[i]);
        break;
    case Instruction::SAVE:
        i++;
        print_code(code_list[i]);
        break;
    case Instruction::PERFORM:
        i++;
        print_op();
        break;
    case Instruction::TEST:
        if (is_op(code_list[i + 1])) {
            i++;
            print_op();
        }
        else {
            print_code(code_list[i]);
        }
        break;
    case Instruction::BRANCH:
        i++;
        print_code(code_list[i]);
        break;
    case Instruction::RESTORE:
        i++;
        print_code(code_list[i]);
        break;
    case Instruction::LABEL:
        i++;
        print_code(code_list[i]);
        print(":");
        break;
    case Instruction::CONT:
        break;
    default: {
        DEBUG_OUTPUT("unknown inst:", inst);
        throw std::runtime_error("unknown inst");
    }
    }
}

void CodeListPrinter::print_assign() {
    auto code = code_list[i];
    print("assign");
    print_args(1);
    if (is_op(code_list[i + 1])) {
        i++;
        print(" ");
        print_op();
    }
    else {
        print_args(1);
    }
}

class CodeRunner {
public:
    CodeRunner(MachineImpl& m, const CodeList& code_list)
        : m(m)
        , code_list(code_list) {
        stream.open("run.log", std::ios::out | std::ios::app);
    }

    ~CodeRunner() {
        stream.close();
    }

    void run(std::size_t pos);

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

    Cell fetch_cell() {
        auto operand = fetch_code();
        if (is_type<Cell>(operand)) {
            return get<Cell>(operand);
        }
        DEBUG_OUTPUT("error:", operand, "is not cell");
        throw std::runtime_error("error");
    }

    Label fetch_label() {
        auto cell = fetch_cell();
        if (is_label(cell)) {
            return get<Label>(cell);
        }
        throw bytecode_error("cell is not label: ", cell);
    }

    void assign_reg(Register r, const InstCode& v);

    void run_inst(Instruction inst);

    Cell run_op(Intern op);

    Cell run_intern(const SymenvPtr& env, Intern op, const std::vector<Cell>& args);

    Cell apply(const CompiledProcedure& proc, const Cell& args);

private:
    MachineImpl& m;
    const CodeList& code_list;
    std::wofstream stream;
    int i;
    bool print_trace = true;
    bool print_cont = false;
};

void CodeRunner::assign_reg(Register r, const InstCode& v) {
    if (is_reg(v)) {
        auto r2 = get<Register>(v);
        m.reg[r] = m.reg[r2];
    }
    else {
        auto v2 = get<Cell>(v);
        m.reg[r] = v2;
    }
}

void CodeRunner::run(std::size_t pos) {
    i = pos;
    while (i + 1 < code_list.size()) {
        auto code = fetch_code();
        if (is_inst(code)) {
            try {
                auto inst = get<Instruction>(code);
                run_inst(inst);
            }
            catch (const ContPtr& cont) {
                if (print_cont) {
                    DEBUG_OUTPUT("call cont");
                }
                m.stack = cont->stack();
                auto val = m.reg[Register::VAL];
                if (print_cont) {
                    m.print_reg();
                    DEBUG_OUTPUT("restore reg from cont");
                }
                m.reg = cont->reg();
                if (print_cont) {
                    m.print_reg();
                }
                m.wind = cont->wind();
                if (!m.wind.empty()) {
                    m.run(m.wind.back().before, nil);
                }
                m.reg[Register::VAL] = val;
                auto new_pos = m.reg.at(Register::CONTINUE);
                if (is_label(new_pos)) {
                    auto label = get_label(new_pos);
                    new_pos = m.label_map.at(label);
                }
                if (is_number(new_pos)) {
                    auto num = get<Number>(new_pos);
                    if (!is_type<Int>(num)) {
                        throw bytecode_error("expect Int but got:", num);
                    }
                    i = get<Int>(num);
                    LOG_TRACE(";;; restore cont --> ");
                    LOG_TRACE(i);
                    LOG_TRACE(LF);
                    continue;
                }
                if (is_intern(new_pos)) {
                    auto op = get<Intern>(new_pos);
                    if (op == Intern::_done_) {
                        return;
                    }
                }
                throw bytecode_error("except Label or <primop _done_> but got:", pos);
            }
            catch (const std::runtime_error& ex) {
                m.print_reg();
                throw ex;
            }
        }
        else if (is_type<Comment>(code)) {
            LOG_TRACE(get<Comment>(code));
            LOG_TRACE(std::endl);
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
    };
}

Cell CodeRunner::apply(const CompiledProcedure& proc, const Cell& args) {
    m.reg[Register::CONTINUE] = Int(i);
    auto entry = proc.entry();
    if (is_number(entry)) {
        auto num = get<Number>(entry);
        if (is_type<Int>(num)) {
            auto pos = get<Int>(num);
            i = pos;
            return none;
        }
    }
    DEBUG_OUTPUT("entry:", entry);
    throw std::runtime_error("error proc entry");
}

Cell CodeRunner::run_intern(const SymenvPtr& env, Intern op, const std::vector<Cell>& args) {
    switch (op) {
    case Intern::op_call_with_output_string: {
        auto vv = args[0];
        if (!is_type<Procedure>(vv)) {
            throw bytecode_error("except Procedure but got:", vv);
        }
        auto proc = get<Procedure>(vv);
        auto port = std::make_shared<StringPort<Char>>(StringPort<Char>::out);
        m.run(proc, m.scm.cons(port, nil));
        return std::make_shared<String>(port->str());
    }
    case Intern::op_machine_print_trace: {
        if (is_true(args[0])) {
            print_trace = true;
        }
        else {
            print_trace = false;
        }
        return none;
    }
    case Intern::op_make_dynamic_wind: {
        auto before = get<Procedure>(args[0]);
        auto thunk = get<Procedure>(args[1]);
        auto after = get<Procedure>(args[2]);
        m.wind.emplace_back(before, thunk, after);
        return none;
    }
    case Intern::op_pop_dynamic_wind: {
        m.wind.pop_back();
        return none;
    }
    default: {
        return m.scm.apply(env, op, args);
    }
    }
}

Cell CodeRunner::run_op(Intern op) {
    switch (op) {
    case Intern::op_lookup_variable_value: {
        auto v = fetch_code();
        auto r = fetch_reg();
        LOG_TRACE("lookup-variable-value ");
        LOG_TRACE(v);
        LOG_TRACE(" ");
        LOG_TRACE(r);
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
            LOG_TRACE(LF);
            LOG_TRACE("env:");
            LOG_TRACE(env);
            LOG_TRACE(" sym:");
            LOG_TRACE(sym);
            return env->get(sym);
        }
        DEBUG_OUTPUT("error operand:", v);
        throw std::runtime_error("error");
    }
    case Intern::op_list: {
        auto r = fetch_reg();
        auto v = m.reg[r];
        LOG_TRACE("list ");
        LOG_TRACE(r);
        return m.scm.cons(v, nil);
    }
    case Intern::op_cons: {
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        LOG_TRACE("cons ");
        LOG_TRACE(r1);
        LOG_TRACE(" ");
        LOG_TRACE(r2);
        return m.scm.cons(m.reg[r1], m.reg[r2]);
    }
    case Intern::op_is_primitive_procedure: {
        auto v = fetch_code();
        LOG_TRACE("primitive-procedure? ");
        LOG_TRACE(v);
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
        LOG_TRACE("apply-primitive-procedure ");
        LOG_TRACE(r1);
        LOG_TRACE(" ");
        LOG_TRACE(r2);
        LOG_TRACE(LF);
        auto proc = m.reg[r1];
        auto argl = m.reg[r2];
        std::vector<Cell> args;
        auto it = argl;
        while (is_pair(it)) {
            args.push_back(car(it));
            it = cdr(it);
        }
        if (!is_nil(it)) {
            args.push_back(it);
        }
        auto env = get<SymenvPtr>(m.reg[Register::ENV]);
        return run_intern(env, get<Intern>(proc), args);
    }
    case Intern::op_make_compiled_macro:
    case Intern::op_make_compiled_procedure: {
        auto args = fetch_code();
        auto label = fetch_label();
        auto r = fetch_reg();
        LOG_TRACE("make-compiled-procedure ");
        LOG_TRACE(args);
        LOG_TRACE(" ");
        LOG_TRACE(label);
        LOG_TRACE(" ");
        LOG_TRACE(r);
        auto env = get<SymenvPtr>(m.reg[r]);
        auto proc = Procedure(env, get<Cell>(args), label, op == Intern::op_make_compiled_macro);
        return proc;
    }
    case Intern::op_compiled_procedure_env: {
        auto v = fetch_code();
        LOG_TRACE("compiled-procedure-env ");
        LOG_TRACE(v);
        if (is_type<Cell>(v)) {
            auto vv = get<Cell>(v);
            if (is_type<Procedure>(vv)) {
                auto proc = get<Procedure>(vv);
                return proc.senv();
            }
        }
        else if (is_reg(v)) {
            auto r = get_reg(v);
            auto vv = m.reg[r];
            if (is_type<Procedure>(vv)) {
                auto proc = get<Procedure>(vv);
                return proc.senv();
            }
        }
        DEBUG_OUTPUT("error operand:", v);
        m.print_reg();
        throw std::runtime_error("error");
    }
    case Intern::op_compiled_procedure_entry: {
        auto r = fetch_reg();
        LOG_TRACE("compiled-procedure-entry ");
        LOG_TRACE(r);
        auto v = m.reg[r];
        if (is_type<Procedure>(v)) {
            auto proc = get<Procedure>(v);
            return proc.entry();
        }
        if (is_cont(v)) {
            auto cont = get<ContPtr>(v);
            LOG_TRACE(LF);
            LOG_TRACE("call cont, continue = ");
            LOG_TRACE(cont->reg().at(Register::CONTINUE));
            LOG_TRACE(LF);
            throw cont;
        }
        DEBUG_OUTPUT("error operand:", v);
        DEBUG_OUTPUT("expect Procedure but got:", v);
        m.print_reg();
        throw std::runtime_error("error");
    }
    case Intern::op_extend_environment: {
        // extend-environment vars vals base-env
        auto v = fetch_cell();
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        LOG_TRACE("extend-environment ");
        LOG_TRACE(v);
        LOG_TRACE(" ");
        LOG_TRACE(r1);
        LOG_TRACE(" ");
        LOG_TRACE(r2);
        auto argl = m.reg[r1];
        auto env = get<SymenvPtr>(m.reg[r2]);
        auto new_env = Symenv::create(env);
        while (is_pair(v) && is_pair(argl)) {
            auto sym = get<Symbol>(car(v));
            new_env->add(sym, car(argl));
            v = cdr(v);
            argl = cdr(argl);
        }
        if (!is_nil(v)) {
            auto sym = get<Symbol>(v);
            new_env->add(sym, argl);
        }
        return new_env;
    }
    case Intern::op_define_variable: {
        auto v = fetch_code();
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        LOG_TRACE("define-variable! ");
        LOG_TRACE(v);
        LOG_TRACE(" ");
        LOG_TRACE(r1);
        LOG_TRACE(" ");
        LOG_TRACE(r2);
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
        LOG_TRACE("false? ");
        LOG_TRACE(r);
        auto v = m.reg[r];
        return is_false(v);
    }
    case Intern::op_set_variable_value: {
        auto v = fetch_code();
        auto r1 = fetch_reg();
        auto r2 = fetch_reg();
        LOG_TRACE("set! ");
        LOG_TRACE(v);
        LOG_TRACE(" ");
        LOG_TRACE(r1);
        LOG_TRACE(" ");
        LOG_TRACE(r2);
        if (!is_type<Cell>(v)) {
            throw bytecode_error("except Cell but got:", v);
        }
        auto vv = get<Cell>(v);
        if (!is_symbol(vv)) {
            throw bytecode_error("except Symbol but got:", vv);
        }
        auto sym = get<Symbol>(vv);
        auto val = m.reg[r1];
        auto env = get<SymenvPtr>(m.reg[r2]);
        env->set(sym, val);
        return none;
    }
    default: {
        return op;
        //        DEBUG_OUTPUT("unknown op:", op);
        //        m.print_reg();
        //        throw std::runtime_error("unknown op");
    }
    }
}

void CodeRunner::run_inst(Instruction inst) {
    switch (inst) {
    case Instruction::ASSIGN: {
        auto r = fetch_reg();
        auto v = fetch_code();
        LOG_TRACE("  assign ");
        LOG_TRACE(r);
        LOG_TRACE(" ");
        if (is_op(v)) {
            assign_reg(r, run_op(get_op(v)));
        }
        else if (is_label(v)) {
            LOG_TRACE(v);
            auto label = get_label(v);
            assign_reg(r, label);
        }
        else {
            LOG_TRACE(v);
            assign_reg(r, v);
        }
        LOG_TRACE(std::endl);
        LOG_TRACE("--> ");
        LOG_TRACE(m.reg.at(r));
        LOG_TRACE(std::endl);
        break;
    }
    case Instruction::TEST: {
        auto v = fetch_code();
        LOG_TRACE("  test ");
        if (is_op(v)) {
            if (is_true(run_op(get_op(v)))) {
                LOG_TRACE(LF);
                LOG_TRACE(";;; --> true");
            }
            else {
                LOG_TRACE(LF);
                LOG_TRACE(";;; --> false");
                // skip branch label
                i += 2;
            }
        }
        else {
            LOG_TRACE(v);
        }
        LOG_TRACE(std::endl);
        break;
    }
    case Instruction::LABEL: {
        i += 1;
        LOG_TRACE(code_list[i]);
        LOG_TRACE(":");
        LOG_TRACE(LF);
        break;
    }
    case Instruction::BRANCH: {
        auto v = fetch_label();
        LOG_TRACE("  branch ");
        LOG_TRACE(v);
        LOG_TRACE(LF);
        auto pos = m.label_map.at(v);
        i = pos;
        break;
    }
    case Instruction::PERFORM: {
        LOG_TRACE("  perform ");
        auto v = fetch_code();
        if (is_op(v)) {
            run_op(get_op(v));
            LOG_TRACE(LF);
            break;
        }
        std::wcout << v;
        std::wcout << std::endl;
        DEBUG_OUTPUT("error operand:", v);
        throw std::runtime_error("error");
    }
    case Instruction::GOTO: {
        auto v = fetch_code();
        LOG_TRACE("  goto ");
        LOG_TRACE(v);
        LOG_TRACE(LF);
        if (is_label(v)) {
            auto label = get_label(v);
            auto pos = m.label_map.at(label);
            i = pos;
            LOG_TRACE(";;; ");
            LOG_TRACE(" --> ");
            LOG_TRACE(label);
            LOG_TRACE(" ");
            LOG_TRACE(i);
            LOG_TRACE(LF);
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
                    LOG_TRACE(";;; ");
                    LOG_TRACE(" --> ");
                    LOG_TRACE(i);
                    LOG_TRACE(LF);
                    break;
                }
            }
            if (is_intern(vv)) {
                if (get<Intern>(vv) == Intern::_done_) {
                    i = code_list.size();
                    LOG_TRACE(";;; ");
                    LOG_TRACE(" --> ");
                    LOG_TRACE("DONE");
                    LOG_TRACE(LF);
                    break;
                }
            }
            if (is_label(vv)) {
                auto label = get_label(vv);
                auto pos = m.label_map.at(label);
                i = pos;
                LOG_TRACE(";;; ");
                LOG_TRACE(" --> ");
                LOG_TRACE(label);
                LOG_TRACE(" ");
                LOG_TRACE(i);
                LOG_TRACE(LF);
                break;
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
        LOG_TRACE("  save ");
        LOG_TRACE(r);
        LOG_TRACE(LF);
        m.stack.push(m.reg[r]);
        LOG_TRACE(";;; ");
        LOG_TRACE(r);
        LOG_TRACE(" --> ");
        LOG_TRACE(m.reg[r]);
        LOG_TRACE(LF);
        break;
    }
    case Instruction::RESTORE: {
        auto r = fetch_reg();
        LOG_TRACE("  restore ");
        LOG_TRACE(r);
        LOG_TRACE(LF);
        if (m.stack.empty()) {
            throw bytecode_error("stack is empty, while restore ", r);
        }
        m.reg[r] = m.stack.top();
        m.stack.pop();
        LOG_TRACE(";;; ");
        LOG_TRACE(r);
        LOG_TRACE(" --> ");
        LOG_TRACE(m.reg[r]);
        LOG_TRACE(LF);
        break;
    }
    case Instruction::CONT: {
        LOG_TRACE("  cont");
        LOG_TRACE(LF);
        Cell cont = std::make_shared<Continuation>(m.stack, m.reg, m.wind);
        LOG_TRACE(";;; ---> ");
        LOG_TRACE(cont);
        LOG_TRACE(LF);
        // m.print_reg();
        m.reg[Register::VAL] = cont;
        break;
    }
    default: {
        DEBUG_OUTPUT("unknown inst:", inst);
        throw std::runtime_error("unknown inst");
    }
    }
}

Cell MachineImpl::run(const CodeList& code_list, const SymenvPtr& env) {
    auto pos = load(code_list);
    return run(env, pos - 1);
}

size_t MachineImpl::load(const CodeList& code_list) {
    fill_label_map(code_list);
    auto pos = all_code_list.size();
    // DEBUG_OUTPUT("print bytecode:");
    CodeListPrinter(code_list, pos).print();
    all_code_list.merge(code_list);
    return pos;
}

Cell MachineImpl::run(const SymenvPtr& env, size_t pos) {
    reg[Register::ENV] = env;
    reg[Register::CONTINUE] = Intern::_done_;
    CodeRunner runner(*this, all_code_list);
    runner.run(pos);
    //    std::wcout << std::endl;
    //    std::wcout << " --> " << reg[Register::VAL] << std::endl;

    return reg[Register::VAL];
}

Cell MachineImpl::run(const Procedure& proc, const Cell& args) {
    auto env = proc.senv();
    auto entry = proc.entry();
    auto it = label_map.find(entry);
    if (it == label_map.end()) {
        throw bytecode_error("no label:", entry);
    }
    size_t pos = it->second;
    auto old_reg = reg;
    reg.clear();
    reg[Register::PROC] = proc;
    reg[Register::ARGL] = args;
    auto ret = run(env, pos);
    reg = old_reg;
    return ret;
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
                    label_map[label] = pos - 2;
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

bool is_eof(const Cell& cell) {
    Cell eof = -1;
    return cell == eof;
}

Cell Machine::run(SymenvPtr env, Cell expr) {
    if (is_eof(expr)) {
        return none;
    }
    auto code = Compiler(impl->scm, env).compile(expr);

    // DEBUG_OUTPUT("run bytecode:", expr);
    impl->reg.clear();
    auto ret = impl->run(code.code->statements, env);
    return ret;
    //    return none;
}

Cell Machine::run(const Procedure& proc, const Cell& args) {
    return impl->run(proc, args);
}

void Machine::load(const CodeList& code_list) {
    impl->load(code_list);
}
} // namespace pscm