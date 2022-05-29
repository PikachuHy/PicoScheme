/*********************************************************************************/
/**
 * @file scheme.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <chrono>
#include <filesystem>
#include <functional>
#include <iomanip>
#include <utility>

#include "picoscm/continuation.h"
#include "picoscm/gc.hpp"
#include "picoscm/machine.h"
#include "picoscm/parser.hpp"
#include "picoscm/port.hpp"
#include "picoscm/primop.hpp"
#include "picoscm/scheme.hpp"
#include "picoscm/syntax.h"
// clang-format off
#define DEBUG(...) if (debugging()) DEBUG_OUTPUT(__VA_ARGS__)

// clang-format on
namespace pscm {
namespace fs = std::filesystem;
using namespace std::string_literals;

static_assert(std::is_same_v<Char, String::value_type>);
static_assert(std::is_same_v<String, StringPtr::element_type>);
static_assert(std::is_same_v<String, Symbol::value_type>);
static_assert(std::is_same_v<Port<Char>, PortPtr::element_type>);
static_assert(std::is_same_v<Symbol, Symtab::Symbol>);
static_assert(std::is_same_v<Symenv, SymenvPtr::element_type>);
static_assert(std::is_same_v<Function, FunctionPtr::element_type>);

Scheme::Scheme(const SymenvPtr& env)
    : m_machine(std::make_shared<Machine>(*this)) {
    auto std_env = add_environment_defaults(*this);
    auto cwd = fs::current_path().string();
    module_paths.push_back(string_convert<Char>(cwd));
    current_module = Module(list(symbol("root")), Symenv::create(std_env));
    module_table.insert_or_assign(current_module.name(), current_module);
    module_stack.push(current_module);
    const char *val = getenv("PSCM_PATH");
    if (val) {
        auto pscm_path = string_convert<Char>(std::string(val));
        if (!pscm_path.empty()) {
            module_paths.push_back(pscm_path);
        }
    }
    else {
        DEBUG_OUTPUT("PSCM_PATH is empty");
    }
    init_op_table();
}

SymenvPtr Scheme::get_module_env(const Cell& module_name) {
    auto it = module_table.find(module_name);
    if (it != module_table.end()) {
        return it->second.env();
    }
    // load module
    return load_module(module_name, get_current_module_env()).env();
}

Module Scheme::load_module(const Cell& module_name, const SymenvPtr& env) {
    String module_file;
    Cell name = module_name;
    while (is_pair(name)) {
        module_file += L"/";
        module_file += get<Symbol>(car(name)).value();
        name = cdr(name);
    }
    for (const auto& path : module_paths) {
        String module_path = path + module_file + L".scm";
        if (!fs::exists(module_path)) {
            continue;
        }
        load(module_path);
        auto it = module_table.find(module_name);
        if (it == module_table.end()) {
            throw module_error("load module, no module defined", module_name);
        }
        return it->second;
    }
    throw module_error("no module: ", module_name);
}

Cell Scheme::apply(const SymenvPtr& env, Intern opcode, const std::vector<Cell>& args) {
    DEBUG("opcode:", opcode);
    auto it = m_op_table.find(opcode);
    if (it != m_op_table.end()) {
        Cell head = cons(none, nil);
        Cell tail = head;
        for (const auto& arg : args) {
            set_cdr(tail, cons(arg, nil));
            tail = cdr(tail);
        }
        return it->second(env, cdr(head));
    }
    else {
        return pscm::call(*this, env, opcode, args);
    }
}

Cell Scheme::apply(const SymenvPtr& env, const FunctionPtr& proc, const std::vector<Cell>& args) {
    return (*proc)(*this, env, args);
}

Cell Scheme::apply(const SymenvPtr& env, const Cell& cell, const std::vector<Cell>& args) {
    if (is_intern(cell)) {
        return apply(env, get<Intern>(cell), args);
    }
    else if (is_proc(cell)) {
        return apply(env, get<Procedure>(cell), args);
    }
    else {
        return apply(env, get<FunctionPtr>(cell), args);
    }
}

Cell Scheme::apply(const SymenvPtr& env, const Cell& op, const Cell& args) {
    if (is_proc(op)) {
        return apply(env, get<Procedure>(op), args);
    }
    else if (is_func(op)) {
        return apply(env, get<FunctionPtr>(op), eval_args(env, args));
    }
    else if (is_intern(op)) {
        return apply(env, get<Intern>(op), eval_args(env, args));
    }
    DEBUG_OUTPUT("op:", op);
    throw std::runtime_error("bad op, expect proc, func or intern");
}

void Scheme::repl(const SymenvPtr& env) {
    const SymenvPtr& senv = env ? env : get_current_module_env();
    Parser parser{ *this };

    auto &out = outPort().stream(), &in = inPort().stream();

    for (Cell expr;;)
        try {
            for (;;) {
                out << "> ";
                expr = none;
                expr = parser.read(in);
                expr = eval(senv, expr);

                if (is_none(expr))
                    continue;

                if (is_exit(expr))
                    return;

                out << expr << std::endl;
            }
        }
        catch (std::exception& e) {
            if (is_none(expr))
                out << e.what() << std::endl;
            else
                out << e.what() << ": " << expr << std::endl;
        }
}

void Scheme::load(const String& filename, const SymenvPtr& env) {
    DEBUG("load:", filename);
    using namespace std::chrono;
    auto t0 = high_resolution_clock::now();
    cur_file = filename;
    module_stack.push(get_current_module());
    using file_port = FilePort<Char>;
    const SymenvPtr& senv = env ? env : get_current_module_env();

    Parser parser{ *this };
    Cell expr = none;

    auto& out = outPort().stream();

    try {
        file_port in{ filename, file_port::in };

        if (!in.is_open())
            throw std::ios_base::failure("couldn't open input file: '"s + string_convert<char>(filename) + "'"s);

        while (!in.eof()) {
            expr = parser.read(in);
            DEBUG(expr);
            expr = m_machine->run(senv, expr);
            // expr = eval_with_continuation(senv, expr);
            DEBUG("-->", expr);
            expr = none;
        }
    }
    catch (const std::exception& e) {
        if (is_none(expr))
            out << e.what() << '\n';
        else
            out << e.what() << ": " << expr << '\n';
        out << "exception occurred when loading file:" << std::endl;
        // TODO: record line number
        out << filename << ":1" << std::endl;
        out << "trace" << std::endl;
    }
    module_stack.pop();
    auto t1 = high_resolution_clock::now();
    duration<double, std::ratio<1, 1>> cost_time = t1 - t0;
    DEBUG_OUTPUT("load", filename, "cost time:", cost_time.count(), "seconds");
}

std::vector<Cell> Scheme::eval_args(const SymenvPtr& env, Cell args, bool is_list) {
    std::vector<Cell> stack;

    if (is_list) { // expression: (proc x y ... z)
        for (/* */; is_pair(args); args = cdr(args))
            stack.push_back(eval(env, car(args)));

        return stack;
    }
    // expression: (apply proc x y ... (args ...))
    Cell last = nil;

    // evaluate (x y ...)
    for (/* */; is_pair(args); args = cdr(args))
        stack.push_back(last = eval(env, car(args)));

    if (is_nil(last)) { // last list (args ...) is nil
        if (!stack.empty())
            stack.pop_back();
        return stack;
    }
    // append arguments from last list (args ...)
    stack.back() = car(last);
    for (args = cdr(last); is_pair(args); args = cdr(args))
        stack.push_back(car(args));

    return stack;
}

Cell Scheme::eval(SymenvPtr env, Cell expr) {
    return m_machine->run(std::move(env), expr);
}

Cell Scheme::append_module_path(const std::vector<Cell>& vargs) {
    for (const auto& args : vargs) {
        auto path = get<StringPtr>(args);
        module_paths.push_back(*path);
    }
    return none;
}

Cell Scheme::eval_string(SymenvPtr env, const String& code) {
    Parser parser{ *this };
    Cell expr = none;
    std::wstringstream ss;
    ss << code;
    expr = parser.read(ss);
    expr = eval(env, expr);
    return expr;
}

void Scheme::addenv(const Symbol& sym, const Cell& val) {
    get_current_module_env()->add(sym, val);
}

void Scheme::addenv(std::initializer_list<std::pair<Symbol, Cell>> args) {
    get_current_module_env()->add(args);
}

SymenvPtr Scheme::newenv(const SymenvPtr& env) {
    return Symenv::create(env ? env : get_current_module_env());
}

Module Scheme::set_current_module(const Cell& cell) {
    if (!is_module(cell)) {
        DEBUG_OUTPUT("args");
        throw module_error("No module:", cell);
    }
    auto ret = current_module;
    current_module = get<Module>(cell);
    return ret;
}

void Scheme::init_op_table() {
}
} // namespace pscm
