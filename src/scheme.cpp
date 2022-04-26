/*********************************************************************************/
/**
 * @file scheme.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <filesystem>
#include <functional>
#include <iomanip>

#include "picoscm/continuation.h"
#include "picoscm/gc.hpp"
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

Scheme::Scheme(const SymenvPtr& env) {
    auto std_env = add_environment_defaults(*this);
    auto cwd = fs::current_path().string();
    module_paths.push_back(string_convert<Char>(cwd));
    current_module = list(symbol("root"));
    module_table[current_module] = Symenv::create(std_env);
    module_stack.push(current_module);
    init_op_table();
}

SymenvPtr Scheme::get_module_env(const Cell& module_name) {
    auto it = module_table.find(module_name);
    if (it != module_table.end()) {
        return it->second;
    }
    // load module
    return load_module(module_name, get_current_module_env());
}

SymenvPtr Scheme::load_module(const Cell& module_name, const SymenvPtr& env) {
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

void Scheme::push_frame(SymenvPtr& env, const Cell& expr) {
    m_frames.emplace_back(env, expr);
    auto op = _get_intern(env, m_frames.back().op());
    if (op == Intern::op_dynamic_wind) {
        DEBUG("before", cadr(expr));
        auto f = eval(env, cadr(expr));
        apply(env, f, nil);
    }
}

void Scheme::replace_frame(SymenvPtr& env, const Cell& expr) {
    pop_frame();
    push_frame(env, expr);
}

void Scheme::pop_frame() {
    if (m_frames.empty()) {
        DEBUG_OUTPUT("frames is emtpy");
    }
    else {
        auto env = m_frames.back().env();
        auto expr = m_frames.back().args();
        auto op = _get_intern(env, m_frames.back().op());
        if (op == Intern::op_dynamic_wind) {
            DEBUG("after", caddr(expr));
            auto f = eval(env, caddr(expr));
            apply(env, f, nil);
        }
        m_frames.pop_back();
    }
}

Cell Scheme::restore_from_continuation(ContPtr& cont, const Cell& args) {
    DEBUG("old frames");
    print_frames();
    m_frames = cont->frames();
    DEBUG("new frames");
    print_frames();
    auto env = m_frames.back().env();
    Cell return_arg = eval(env, args);
    while (!m_frames.empty()) {
        m_frames.back().push_arg(return_arg);
        return_arg = eval_frame_based_on_stack();
        pop_frame();
    }
    return return_arg;
}

Cell Scheme::eval_frame_based_on_stack() {
    auto& frame = m_frames.back();
    auto pc = frame.arg_count();
    auto op = frame.op();
    DEBUG("op:", op);
    auto args = frame.args();
    DEBUG("arg:", args);
    auto env = frame.env();
    for (int i = 0; i < pc; ++i) {
        if (is_pair(args)) {
            args = cdr(args);
        }
    }
    while (!is_nil(args)) {
        auto val = eval(env, car(args));
        m_frames.back().push_arg(val);
        args = cdr(args);
    }
    op = eval(env, op);
    if (_get_intern(env, op) == Intern::_begin) {
        return m_frames.back().varg().back();
    }
    auto ret = apply(env, op, m_frames.back().varg());
    return ret;
}

Cell Scheme::apply(const SymenvPtr& env, Intern opcode, const std::vector<Cell>& args) {
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

Cell Scheme::apply(const SymenvPtr& env, const Procedure& proc, const std::vector<Cell>& args) {
    return proc.call(*this, env, args);
}

Cell Scheme::apply(const SymenvPtr& env, const Procedure& proc, const Cell& args, bool is_list) {
    auto [new_env, code] = proc.apply(*this, env, args, is_list);
    DEBUG("code:", code);
    return syntax_begin(new_env, code);
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

Cell Scheme::expand(const Cell& macro, Cell& args) {
    return get<Procedure>(macro).expand(*this, args);
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
                expr = eval_with_continuation(senv, expr);

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
            expr = eval_with_continuation(senv, expr);
            DEBUG("-->", expr);
            expr = none;
        }
    }
    catch (const std::exception& e) {
        if (is_none(expr))
            out << e.what() << '\n';
        else
            out << e.what() << ": " << expr << '\n';
    }
    module_stack.pop();
}

Cell Scheme::syntax_begin(const SymenvPtr& env, Cell args) {
    if (is_nil(args)) {
        return none;
    }
    if (!is_pair(args)) {
        DEBUG("args:", args);
        throw std::invalid_argument("invalid begin syntax");
    }
    Cell ret;
    while (is_pair(args)) {
        DEBUG("eval", car(args));
        ret = eval(env, car(args));
        args = cdr(args);
    }
    return ret;
}

Cell Scheme::syntax_if(const SymenvPtr& env, const Cell& args) {
    if (is_true(eval(env, car(args))))
        return eval(env, cadr(args));

    else if (const Cell& last = cddr(args); !is_nil(last))
        return eval(env, car(last));

    else
        return none;
}

Cell Scheme::syntax_cond(const SymenvPtr& env, Cell args) {
    Cell test = false, expr = nil;
    DEBUG("args:", args);
    // For each clause evaluate <test> condition
    for (/* */; is_pair(args); args = cdr(args)) {
        is_pair(car(args)) || (void(throw std::invalid_argument("invalid cond syntax")), 0);

        if (is_false(test)) {
            test = eval(env, caar(args));

            if (is_true(test)) {
                expr = cdar(args);
                break;
            }
        }
    }
    if (is_true(test)) {
        if (is_nil(expr))
            return test;

        const Cell& first = car(expr);
        // clause: (<test> => <expr> ...)  -> (apply <expr> <test> nil) ...
        if (is_arrow(first) || (is_symbol(first) && is_arrow(eval(env, first)))) {
            !is_else(test) || (void(throw std::invalid_argument("invalid cond syntax")), 0);

            Cons cons[4], argv[2];
            Cell apply_expr = pscm::list(cons, Intern::_apply, none, pscm::list(argv, Intern::_quote, test), nil);

            // For each expression, first replace none in apply_expr and then call eval:
            for (expr = cdr(expr); is_pair(cdr(expr)); expr = cdr(expr)) {
                set_car(cdr(apply_expr), car(expr));
                eval(env, apply_expr);
            }
            // Return last expression to evaluated at the call site to maintain unbound tail-recursion:
            return list(Intern::_apply, car(expr), list(Intern::_quote, test), nil);
        }
        else
            return syntax_begin(env, expr);
    }
    return none;
}

Cell Scheme::syntax_case(const SymenvPtr& env, Cell args) {
    auto key = eval(env, car(args));
    auto clauses = cdr(args);
    while (is_pair(clauses)) {
        auto clause = car(clauses);
        is_pair(clause) || (void(throw std::invalid_argument("invalid case syntax")), 0);
        auto datum_list = car(clause);
        if (!is_pair(datum_list)) {
            if (_get_intern(env, datum_list) == Intern::_else) {
                return syntax_begin(env, cdr(clause));
            }
            throw std::invalid_argument("invalid case syntax");
        }
        while (is_pair(datum_list)) {
            auto datum = car(datum_list);
            if (is_equal(key, datum)) {
                return syntax_begin(env, cdr(clause));
            }
            datum_list = cdr(datum_list);
        }
        clauses = cdr(clauses);
    }
    return none;
}

Cell Scheme::syntax_do(const SymenvPtr& env, Cell args) {
    auto var_list = car(args);
    auto test_expr = cadr(args);
    auto cmd_list = cddr(args);
    auto new_env = newenv(env);
    // init var
    auto var_list_it = var_list;
    while (!is_nil(var_list_it)) {
        auto var = car(var_list_it);
        is_pair(var) || (void(throw std::invalid_argument("invalid do syntax")), 0);
        auto sym = get<Symbol>(car(var));
        new_env->add(sym, eval(env, cadr(var)));
        var_list_it = cdr(var_list_it);
    }
    while (!get<Bool>(eval(new_env, car(test_expr)))) {
        DEBUG("cmd_list:", cmd_list);
        syntax_begin(new_env, cmd_list);
        // update var
        var_list_it = var_list;
        auto tmp_env = Symenv::create();
        while (!is_nil(var_list_it)) {
            auto var = car(var_list_it);
            is_pair(var) || (void(throw std::invalid_argument("invalid do syntax")), 0);
            auto sym = get<Symbol>(car(var));
            auto step = cddr(var);
            if (!is_nil(step)) {
                tmp_env->add(sym, eval(new_env, step));
            }
            var_list_it = cdr(var_list_it);
        }
        for (const auto& [k, v] : tmp_env->cursor()) {
            new_env->add(k, v);
        }
    }
    if (is_nil(test_expr)) {
        return none;
    }
    return syntax_begin(new_env, cdr(test_expr));
}

Cell Scheme::syntax_when(const SymenvPtr& env, Cell args) {
    if (is_true(eval(env, car(args))) && is_pair(args = cdr(args))) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            eval(env, car(args));

        return car(args);
    }
    return none;
}

Cell Scheme::syntax_unless(const SymenvPtr& env, Cell args) {
    if (is_false(eval(env, car(args))) && is_pair(args = cdr(args))) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            eval(env, car(args));

        return car(args);
    }
    return none;
}

Cell Scheme::syntax_let(const SymenvPtr& env, Cell args, bool star) {
    SymenvPtr cur_env = newenv(env);
    Cell cur_args = args;
    if (is_pair(car(args))) {
        auto bindings = car(args);
        auto sub_env = env->create(cur_env);
        for (; !is_nil(bindings) && is_pair(car(bindings)); bindings = cdr(bindings)) {
            auto binding = car(bindings);
            auto var = car(binding);
            auto val = cadr(binding);
            auto sym = get<Symbol>(var);
            auto _env = star ? sub_env : env;
            auto v = eval(_env, val);
            sub_env->add(sym, v);
        }
        cur_args = cdr(args);
        cur_env = sub_env;
    }
    else if (is_symbol(car(args))) {
        // named let
        // (let ⟨variable⟩ ⟨bindings⟩ ⟨body⟩)
        auto variable = get<Symbol>(car(args));
        auto bindings = cadr(args);
        auto body = caddr(args);
        auto new_env = newenv(env);
        auto proc_args = cons(none, nil);
        Cell proc_args_it = proc_args;
        for (; !is_nil(bindings) && is_pair(car(bindings)); bindings = cdr(bindings)) {
            auto binding = car(bindings);
            auto var = car(binding);
            auto val = cadr(binding);
            auto sym = get<Symbol>(var);
            auto v = eval(env, val);
            new_env->add(sym, v);

            set_cdr(proc_args_it, cons(sym, nil));
            proc_args_it = cdr(proc_args_it);
        }
        new_env->add(variable, Procedure(new_env, cdr(proc_args), cons(body, nil)));
        return eval(new_env, body);
    }
    Cell expr = none;
    while (!is_nil(cur_args)) {
        expr = eval(cur_env, car(cur_args));
        cur_args = cdr(cur_args);
    }
    return expr;
}

Cell Scheme::syntax_with_let(const SymenvPtr& env, Cell args) {
    auto cur_env = get<SymenvPtr>(eval(env, car(args)));
    auto cur_args = cdr(args);
    Cell expr = none;
    while (!is_nil(cur_args)) {
        expr = eval(cur_env, cur_args);
        cur_args = cdr(cur_args);
    }
    return expr;
}

Cell Scheme::syntax_with_module(const SymenvPtr& env, Cell args) {
    auto cur_m = eval(env, car(args));
    if (module_table.find(cur_m) == module_table.end()) {
        throw module_error("No modules:", car(args));
    }
    auto cur_env = module_table[cur_m];
    auto cur_args = cdr(args);
    Cell expr = none;
    while (!is_nil(cur_args)) {
        expr = eval(cur_env, cur_args);
        cur_args = cdr(cur_args);
    }
    return expr;
}

Cell Scheme::syntax_and(const SymenvPtr& env, Cell args) {
    Cell res = true;

    if (is_pair(args)) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            if (is_false(res = eval(env, car(args))))
                return res;

        is_nil(cdr(args)) || (void(throw std::invalid_argument("not a proper list")), 0);
        return eval(env, car(args));
    }
    return res;
}

Cell Scheme::syntax_or(const SymenvPtr& env, Cell args) {
    Cell res = false;

    if (is_pair(args)) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            if (is_true(res = eval(env, car(args))))
                return res;

        is_nil(cdr(args)) || (void(throw std::invalid_argument("not a proper list")), 0);
        return eval(env, car(args));
    }
    return res;
}

Cell Scheme::eval_list(const SymenvPtr& env, Cell list, bool is_list) {
    if (!is_pair(list))
        return nil;

    if (is_list) {
        Cell head = cons(eval(env, car(list)), cdr(list));
        list = cdr(list);

        for (Cell tail = head; is_pair(list); tail = cdr(tail), list = cdr(list))
            set_cdr(tail, cons(eval(env, car(list)), cdr(list)));

        return head;
    }
    Cell tail, head;

    if (is_pair(cdr(list)))
        head = cons(eval(env, car(list)), cdr(list));
    else
        head = eval(env, car(list));

    for (tail = head, list = cdr(list); is_pair(list); tail = cdr(tail), list = cdr(list))
        if (is_pair(cdr(list)))
            set_cdr(tail, cons(eval(env, car(list)), cdr(list)));
        else
            set_cdr(tail, eval(env, car(list)));

    is_nil(tail) || is_pair(tail) || (void(throw std::invalid_argument("invalid apply argument list")), 0);
    return head;
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

Cell Scheme::eval_with_continuation(SymenvPtr env, Cell expr) {
    ContPtr c;
    Cell c_args;
    bool need_restore = false;
    while (true) {
        try {
            if (need_restore) {
                return restore_from_continuation(c, c_args);
                need_restore = false;
            }
            else {
                return eval(env, expr);
            }
        }
        catch (const Cell& cell) {
            if (is_cont(car(cell))) {
                c = get<ContPtr>(car(cell));
                c_args = cdr(cell);
                need_restore = true;
                continue;
            }
            DEBUG_OUTPUT("op:", car(cell));
            throw std::runtime_error("unsupported op");
        }
    }
}

Cell Scheme::eval(SymenvPtr env, Cell expr) {
    bool need_pop_frame = true;
    DEBUG("eval:", expr);
    if (is_nil(expr)) {
        return nil;
    }
    if (is_symbol(expr)) {
        auto sym = get<Symbol>(expr);
        return env->get(sym);
    }
    if (!is_pair(expr)) {
        return expr;
    }
    push_frame(env, expr);
    auto op = eval(env, car(expr));
    Cell ret;
    if (is_cont(op)) {
        DEBUG("op", op, "is continuation");
        auto cont = get<ContPtr>(op);
        auto cont_args = eval(env, cdr(expr));
        DEBUG("cont args:", cont_args);
        throw Cell(cons(cont, cons(Intern::_quote, cons(cont_args, nil))));
    }
    else if (is_func(op)) {
        ret = eval_frame_based_on_stack();
    }
    else if (is_proc(op)) {
        if (is_macro(op)) {
            auto f = get<Procedure>(op);
            auto expand_code = f.expand_only(*this, expr);
            DEBUG("expand code:", expand_code);
            pop_frame();
            need_pop_frame = false;
            ret = eval(env, expand_code);
        }
        else {
            ret = eval_frame_based_on_stack();
        }
    }
    else if (is_syntax(op)) {
        const auto& matched = get<SyntaxPtr>(op)->match(cdr(expr));
        auto expand_code = matched.expand_syntax(*this, expr);
        DEBUG("expand code:", expand_code);
        pop_frame();
        need_pop_frame = false;
        ret = eval(env, expand_code);
    }
    else if (is_intern(op)) {
        auto opcode = get<Intern>(op);
        DEBUG("opcode:", opcode);
        if (opcode == Intern::op_callcc) {
            ret = callcc(env, expr);
        }
        else if (opcode == Intern::_apply) {
            Cell args = cdr(expr);
            Cell proc = eval(env, car(args));
            if (is_proc(proc)) {
                if (is_macro(proc)) {
                    auto expand_code = expand(proc, args);
                    ret = eval(env, expand_code);
                }
                else {
                    ret = apply(env, get<Procedure>(proc), cdr(args), false);
                }
            }
            else {
                // proc is either an opcode or function pointer:
                ret = apply(env, proc, eval_args(env, cdr(args), false));
            }
        }
        else {
            auto it = m_op_table.find(opcode);
            if (it == m_op_table.end()) {
                DEBUG("op:", op);
                ret = eval_frame_based_on_stack();
            }
            else {
                ret = it->second(env, cdr(expr));
            }
        }
    }
    else {
        ret = op;
    }
    if (need_pop_frame) {
        pop_frame();
    }

    DEBUG("eval:", expr);
    DEBUG(" --> ", ret);
    return ret;
}

Cell Scheme::syntax_module(const SymenvPtr& senv, const Cell& args) {
    auto module_name = car(args);
    auto it = module_table.find(module_name);
    if (it != module_table.end()) {
        throw module_error("module exist: ", module_name);
    }
    auto cur_env = get_current_module_env();
    auto env = newenv(cur_env);
    module_table[module_name] = env;
    module_stack.push(module_name);
    current_module = module_name;
    if (!is_nil(cdr(args))) {
        auto use = cadr(args);
        if (get<Symbol>(car(use)).value() == L":use") {
            return syntax_use_module(senv, cdr(use));
        }
        throw module_error("module syntax error: ", module_name);
    }
    return none;
}

Cell Scheme::syntax_inherit_module(const SymenvPtr& senv, Cell args) {
    auto cur_env = get_current_module_env();
    while (is_pair(args)) {
        auto env = get_module_env(car(args));
        cur_env->inherit(*env);
        args = cdr(args);
    }
    return none;
}

Cell Scheme::syntax_use_module(const SymenvPtr& senv, Cell args) {
    auto cur_env = get_current_module_env();
    while (is_pair(args)) {
        auto env = get_module_env(car(args));
        cur_env->use(*env);
        args = cdr(args);
    }
    return none;
}

Cell Scheme::syntax_quasiquote(const SymenvPtr& senv, Cell args) {
    return partial_eval(senv, args);
}

Cell Scheme::syntax_define(const SymenvPtr& senv, Cell args, bool is_public) {
    if (is_pair(car(args))) {
        auto name = caar(args);
        if (is_pair(name)) {
            auto f2 = Procedure{ senv, cdar(args), cdr(args) };
            auto f1 = Procedure{ senv, cdr(name), cons(f2, nil) };
            senv->add(get<Symbol>(car(name)), f1, is_public);
        }
        else {
            auto proc = Procedure{ senv, cdar(args), cdr(args) };
            senv->add(get<Symbol>(name), proc, is_public);
        }
    }
    else {
        senv->add(get<Symbol>(car(args)), eval(senv, cadr(args)));
    }
    return none;
}

Cell Scheme::syntax_define_syntax(const SymenvPtr& senv, Cell args) {
    auto keyword = car(args);
    auto transformer_spec = cdr(args);
    auto syntax_rules = car(transformer_spec);
    auto syntax = syntax_syntax_rules(senv, syntax_rules);
    senv->add(get<Symbol>(keyword), syntax);
    return none;
}

Cell Scheme::syntax_syntax_rules(const SymenvPtr& senv, Cell args) {
    auto expr = args;
    if (_get_intern(senv, car(expr)) != Intern::_syntax_rules) {
        DEBUG_OUTPUT("syntax error:", args);
        throw std::runtime_error("syntax error");
    }
    expr = cdr(expr);
    auto literals = car(expr);
    expr = cdr(expr);
    auto syntax_rule = car(expr);
    auto rules = cdr(expr);

    SyntaxPtr syntaxPtr = std::make_shared<Syntax>();
    while (!is_nil(rules)) {
        auto rule = car(rules);
        auto head = cdar(rule);
        auto body = cadr(rule);
        DEBUG("body:", body);
        syntaxPtr->add(head, Procedure{ senv, head, body, true });
        rules = cdr(rules);
    }
    return syntaxPtr;
}

Cell Scheme::syntax_delay(const SymenvPtr& senv, const Cell& args) {
    return Promise(Procedure(senv, nil, args));
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

Cell Scheme::set_current_module(const Cell& module_name) {
    if (module_table.find(module_name) == module_table.end()) {
        throw module_error("No module:", module_name);
    }
    auto ret = current_module;
    current_module = module_name;
    return ret;
}

Intern Scheme::_get_intern(const SymenvPtr& senv, const Cell& cell) {
    if (!is_symbol(cell) || !senv->defined_sym(get<Symbol>(cell))) {
        return Intern::_none_;
    }
    auto val = senv->get(get<Symbol>(cell));
    if (!is_intern(val)) {
        return Intern::_none_;
    }
    return get<Intern>(val);
}

void Scheme::concat_list(Cell& cell, Cell l) {
    Cell ret = cell;
    if (is_nil(l)) {
        return;
    }
    while (is_pair(l)) {
        DEBUG("l:", l);
        set_cdr(cell, cons(car(l), nil));
        l = cdr(l);
        cell = cdr(cell);
    }
    DEBUG("after concat:", ret);
}

void Scheme::partial_eval_sub(const SymenvPtr& senv, const Cell& item, Cell& tail, int nesting) {
    auto opcode = _get_intern(senv, car(item));
    DEBUG("opcode:", opcode, item);
    if (opcode == Intern::_unquote) {
        Cell new_val;
        if (nesting == 0) {
            new_val = eval(senv, cadr(item));
        }
        else {
            new_val = partial_eval(senv, cdr(item), nesting - 1);
            new_val = cons(car(item), new_val);
        }
        set_cdr(tail, cons(new_val, nil));
        tail = cdr(tail);
    }
    else if (opcode == Intern::_unquotesplice) {
        if (nesting == 0) {
            auto l = eval(senv, cadr(item));
            concat_list(tail, l);
        }
        else {
            auto tmp = partial_eval(senv, cadr(item), nesting - 1);
            set_cdr(tail, cons(tmp, nil));
            tail = cdr(tail);
        }
    }
    else if (opcode == Intern::_quasiquote) {
        auto tmp = partial_eval(senv, cadr(item), nesting + 1);
        set_cdr(tail, cons(list(car(item), tmp), nil));
        tail = cdr(tail);
    }
    else {
        auto new_val = partial_eval(senv, item);
        set_cdr(tail, cons(new_val, nil));
        tail = cdr(tail);
    }
}

Cell Scheme::partial_eval(const SymenvPtr& senv, const Cell& cell, int nesting) {
    DEBUG("cell:", cell, "nesting:", nesting);
    if (!is_pair(cell)) {
        return cell;
    }
    Cell ret = cons(none, nil);
    auto tail = ret;
    auto it = cell;
    while (is_pair(it)) {
        auto item = car(it);
        if (is_pair(item)) {
            partial_eval_sub(senv, item, tail, nesting);
        }
        else {
            // handle (... ,body) (... ,@(...))
            auto opcode = _get_intern(senv, item);
            if (opcode == Intern::_unquote) {
                auto new_val = eval(senv, cdr(it));
                set_cdr(tail, new_val);
                it = cdr(it);
            }
            else if (opcode == Intern::_unquotesplice) {
                auto l = eval(senv, cdr(it));
                concat_list(tail, l);
                it = cdr(it);
            }
            else {
                set_cdr(tail, cons(item, nil));
            }
            tail = cdr(tail);
        }
        it = cdr(it);
        DEBUG("-->:", cdr(ret));
    }
    return cdr(ret);
}

Cell Scheme::callcc(const SymenvPtr& senv, const Cell& cell) {
    auto cont = std::make_shared<ContPtr::element_type>(m_frames);
    auto f = eval(senv, cadr(cell));
    Cell expr = list(f, cont);
    return eval(senv, expr);
}

void Scheme::init_op_table() {
    m_op_table[Intern::_quasiquote] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_quasiquote(senv, car(cell));
    };

    m_op_table[Intern::_quote] = [this](const SymenvPtr& senv, const Cell& cell) {
        return car(cell);
    };

    m_op_table[Intern::_setb] = [this](const SymenvPtr& senv, const Cell& cell) {
        auto sym = get<Symbol>(car(cell));
        auto val = eval(senv, cadr(cell));
        senv->set(sym, val);
        return none;
    };

    m_op_table[Intern::_define] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_define(senv, cell);
    };

    m_op_table[Intern::_define_public] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_define(senv, cell, true);
    };

    m_op_table[Intern::_lambda] = [this](const SymenvPtr& senv, const Cell& cell) {
        return Procedure{ senv, car(cell), cdr(cell) };
    };

    m_op_table[Intern::_macro] = [this](const SymenvPtr& senv, const Cell& cell) {
        auto sym = get<Symbol>(caar(cell));
        auto m = Procedure{ senv, cdar(cell), cdr(cell), true };
        senv->add(sym, m);
        return none;
    };

    m_op_table[Intern::_macro_public] = [this](const SymenvPtr& senv, const Cell& cell) {
        auto sym = get<Symbol>(caar(cell));
        auto m = Procedure{ senv, cdar(cell), cdr(cell), true };
        senv->add(sym, m, true);
        return none;
    };

    m_op_table[Intern::_define_syntax] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_define_syntax(senv, cell);
    };

    m_op_table[Intern::_delay] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_delay(senv, cell);
    };

    m_op_table[Intern::_begin] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_begin(senv, cell);
    };

    m_op_table[Intern::_if] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_if(senv, cell);
    };

    m_op_table[Intern::_cond] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_cond(senv, cell);
    };

    m_op_table[Intern::_case] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_case(senv, cell);
    };

    m_op_table[Intern::_do] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_do(senv, cell);
    };

    m_op_table[Intern::_when] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_when(senv, cell);
    };

    m_op_table[Intern::_unless] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_unless(senv, cell);
    };

    m_op_table[Intern::_let] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_let(senv, cell);
    };

    m_op_table[Intern::_let_star] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_let(senv, cell, true);
    };

    m_op_table[Intern::_with_let] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_with_let(senv, cell);
    };

    m_op_table[Intern::_with_module] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_with_module(senv, cell);
    };

    m_op_table[Intern::_and] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_and(senv, cell);
    };

    m_op_table[Intern::_or] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_or(senv, cell);
    };

    m_op_table[Intern::_module] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_module(senv, cell);
    };

    m_op_table[Intern::_inherit_module] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_inherit_module(senv, cell);
    };

    m_op_table[Intern::_use_module] = [this](const SymenvPtr& senv, const Cell& cell) {
        return this->syntax_use_module(senv, cell);
    };
}

void Scheme::print_frames() {
    for (size_t i = 0; i < m_frames.size(); i++) {
        DEBUG(i, m_frames[i].expr());
    }
}
} // namespace pscm
