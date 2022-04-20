/*********************************************************************************/ /**
 * @file scheme.cpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#include <functional>
#include <iomanip>
#include <filesystem>

#include "picoscm/gc.hpp"
#include "picoscm/parser.hpp"
#include "picoscm/primop.hpp"
#include "picoscm/scheme.hpp"
#include "picoscm/port.hpp"
#include "picoscm/syntax.h"

#define DEBUG(...) if (debugging()) DEBUG_OUTPUT(__VA_ARGS__)
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
{
    auto std_env = add_environment_defaults(*this);
    auto cwd = fs::current_path().string();
    module_paths.push_back(string_convert<Char>(cwd));
    current_module = list(symbol("root"));
    module_table[current_module] = Symenv::create(std_env);
    module_stack.push(current_module);
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
    for(const auto& path: module_paths) {
        String module_path = path + module_file  + L".scm";
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
Cell Scheme::apply(const SymenvPtr& env, Intern opcode, const std::vector<Cell>& args)
{
    return pscm::call(*this, env, opcode, args);
}

Cell Scheme::apply(const SymenvPtr& env, const FunctionPtr& proc, const std::vector<Cell>& args)
{
    return (*proc)(*this, env, args);
}

Cell Scheme::apply(const SymenvPtr& env, const Cell& cell, const std::vector<Cell>& args)
{
    if (is_intern(cell))
        return apply(env, get<Intern>(cell), args);
    else
        return apply(env, get<FunctionPtr>(cell), args);
}

std::pair<SymenvPtr, Cell> Scheme::apply(const SymenvPtr& env, const Cell& proc, const Cell& args, bool is_list)
{
    return get<Procedure>(proc).apply(*this, env, args, is_list);
}

Cell Scheme::expand(const Cell& macro, Cell& args)
{
    return get<Procedure>(macro).expand(*this, args);
}

void Scheme::repl(const SymenvPtr& env)
{
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
        } catch (std::exception& e) {
            if (is_none(expr))
                out << e.what() << std::endl;
            else
                out << e.what() << ": " << expr << std::endl;
        }
}

void Scheme::load(const String& filename, const SymenvPtr& env)
{
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
            throw std::ios_base::failure("couldn't open input file: '"s
                + string_convert<char>(filename) + "'"s);

        while (!in.eof()) {
            expr = parser.read(in);
            if (debugging()) DEBUG_OUTPUT(expr);
            expr = eval(senv, expr);
            if (debugging()) DEBUG_OUTPUT("-->", expr);
            expr = none;
        }
    } catch (const std::exception& e) {
        if (is_none(expr))
            out << e.what() << '\n';
        else
            out << e.what() << ": " << expr << '\n';
    }
    module_stack.pop();
}

Cell Scheme::syntax_begin(const SymenvPtr& env, Cell args)
{
    if (is_pair(args)) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            eval(env, car(args));

        return car(args);
    }
    return none;
}

Cell Scheme::syntax_if(const SymenvPtr& env, const Cell& args)
{
    if (is_true(eval(env, car(args))))
        return cadr(args);

    else if (const Cell& last = cddr(args); !is_nil(last))
        return car(last);

    else
        return none;
}

Cell Scheme::syntax_cond(const SymenvPtr& env, Cell args)
{
    Cell test = false, expr = nil;

    // For each clause evaluate <test> condition
    for (/* */; is_pair(args); args = cdr(args)) {
        is_pair(car(args)) || (void(throw std::invalid_argument("invalid cond syntax")), 0);

        if (is_false(test)) {
            test = eval(env, caar(args));

            if (is_true(test))
                expr = cdar(args);
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
            Cell apply_expr = pscm::list(cons, Intern::_apply, none,
                pscm::list(argv, Intern::_quote, test), nil);

            // For each expression, first replace none in apply_expr and then call eval:
            for (expr = cdr(expr); is_pair(cdr(expr)); expr = cdr(expr)) {
                set_car(cdr(apply_expr), car(expr));
                eval(env, apply_expr);
            }
            // Return last expression to evaluated at the call site to maintain unbound tail-recursion:
            return list(Intern::_apply, car(expr), list(Intern::_quote, test), nil);
        } else
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
        eval_list(new_env, cmd_list);
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
        for(const auto& [k, v]: tmp_env->cursor()) {
            new_env->add(k, v);
        }
    }
    if (is_nil(test_expr)) {
        return none;
    }
    return eval_list(new_env, cdr(test_expr));

}


Cell Scheme::syntax_when(const SymenvPtr& env, Cell args)
{
    if (is_true(eval(env, car(args))) && is_pair(args = cdr(args))) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            eval(env, car(args));

        return car(args);
    }
    return none;
}

Cell Scheme::syntax_unless(const SymenvPtr& env, Cell args)
{
    if (is_false(eval(env, car(args))) && is_pair(args = cdr(args))) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            eval(env, car(args));

        return car(args);
    }
    return none;
}

Cell Scheme::syntax_let(const SymenvPtr& env, Cell args, bool star)
{
    SymenvPtr cur_env = env;
    Cell cur_args = args;
    if (is_pair(car(args))) {
        auto bindings = car(args);
        auto sub_env = env->create(env);
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
    } else if (is_symbol(car(args))) {
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

Cell Scheme::syntax_and(const SymenvPtr& env, Cell args)
{
    Cell res = true;

    if (is_pair(args)) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            if (is_false(res = eval(env, car(args))))
                return res;

        is_nil(cdr(args)) || (void(throw std::invalid_argument("not a proper list")), 0);
        return car(args);
    }
    return res;
}

Cell Scheme::syntax_or(const SymenvPtr& env, Cell args)
{
    Cell res = false;

    if (is_pair(args)) {
        for (/* */; is_pair(cdr(args)); args = cdr(args))
            if (is_true(res = eval(env, car(args))))
                return list(Intern::_quote, res);

        is_nil(cdr(args)) || (void(throw std::invalid_argument("not a proper list")), 0);
        return car(args);
    }
    return res;
}

Cell Scheme::eval_list(const SymenvPtr& env, Cell list, bool is_list)
{
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

    is_nil(tail) || is_pair(tail)
        || (void(throw std::invalid_argument("invalid apply argument list")), 0);
    return head;
}

std::vector<Cell> Scheme::eval_args(const SymenvPtr& env, Cell args, bool is_list)
{
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

Cell Scheme::eval(SymenvPtr env, Cell expr)
{
    Cell args, proc;

    for (;;) {
        if (is_nil(expr)) {
            return expr;
        }
        if (is_list(expr) && list_length(expr) == 0) {
            return expr;
        }
        if (is_symbol(expr))
            return env->get(get<Symbol>(expr));

        if (!is_pair(expr))
            return expr;

        if (is_func(proc = eval(env, car(expr))))
            return apply(env, proc, eval_args(env, cdr(expr)));

        if (is_proc(proc)) {
            if (is_macro(proc))
                expr = expand(proc, expr);
            else {
                tie(env, args) = apply(env, proc, cdr(expr));
                expr = syntax_begin(env, args);
            }
            continue;
        }
        if (is_syntax(proc)) {
            auto syntaxPtr = get<SyntaxPtr>(proc);
            const auto& m = syntaxPtr->match(cdr(expr));
            expr = m.expand_syntax(*this, expr);
            ret = expr;
            continue;
        }
        args = cdr(expr);
        if (!is_intern(proc)) return proc;
        auto opcode = get<Intern>(proc);
        if (opcode == Intern::_quasiquote) {
            return syntax_quasiquote(env, car(args));
        }
        switch (opcode) {

        case Intern::_quote:
            return car(args);

        case Intern::_setb:
            env->set(get<Symbol>(car(args)), eval(env, cadr(args)));
            return none;

        case Intern::_define:
            if (is_pair(car(args)))
                env->add(get<Symbol>(caar(args)), Procedure{ env, cdar(args), cdr(args) });
            else
                env->add(get<Symbol>(car(args)), eval(env, cadr(args)));
            return none;
        case Intern::_define_public:
            if (is_pair(car(args)))
                env->add(get<Symbol>(caar(args)), Procedure{ env, cdar(args), cdr(args) }, true);
            else
                env->add(get<Symbol>(car(args)), eval(env, cadr(args)), true);
            return none;

        case Intern::_define_syntax:
            return syntax_define_syntax(env, args);

        case Intern::_lambda:
            return Procedure{ env, car(args), cdr(args) };

        case Intern::_macro:
            env->add(get<Symbol>(caar(args)), Procedure{ env, cdar(args), cdr(args), true });
            return none;
        case Intern::_macro_public:
            env->add(get<Symbol>(caar(args)), Procedure{ env, cdar(args), cdr(args), true }, true);
            return none;
        case Intern::_apply:
            if (is_proc(proc = eval(env, car(args)))) {
                if (is_macro(proc))
                    expr = expand(proc, args);
                else {
                    tie(env, args) = apply(env, proc, cdr(args), false);
                    expr = syntax_begin(env, args);
                }
                break;
            } else // proc is either an opcode or function pointer:
                return apply(env, proc, eval_args(env, cdr(args), false));

        case Intern::_begin:
            expr = syntax_begin(env, args);
            break;

        case Intern::_if:
            expr = syntax_if(env, args);
            break;

        case Intern::_cond:
            expr = syntax_cond(env, args);
            break;

        case Intern::_case:
            expr = syntax_case(env, args);
            break;

        case Intern::_do:
            expr = syntax_do(env, args);
            break;

        case Intern::_when:
            expr = syntax_when(env, args);
            break;

        case Intern::_unless:
            expr = syntax_unless(env, args);
            break;

        case Intern::_let:
            expr = syntax_let(env, args);
            return expr;

        case Intern::_let_star:
            expr = syntax_let(env, args, true);
            break;

        case Intern::_with_let:
            expr = syntax_with_let(env, args);
            break;

        case Intern::_with_module:
            expr = syntax_with_module(env, args);
            break;

        case Intern::_and:
            expr = syntax_and(env, args);
            break;

        case Intern::_or:
            expr = syntax_or(env, args);
            break;
        case Intern::_module:
            expr = syntax_module(env, args);
            break;
        case Intern::_inherit_module:
            expr = syntax_inherit_module(env, args);
            break;
        case Intern::_use_module:
            expr = syntax_use_module(env, args);
            break;
        default:
            return apply(env, opcode, eval_args(env, args));
        }
    }
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
Cell Scheme::syntax_define_syntax(const SymenvPtr& senv, Cell args)
{
    auto keyword = car(args);
    auto transformer_spec = cdr(args);
    auto syntax_rules = car(transformer_spec);
    auto syntax = syntax_syntax_rules(senv, syntax_rules);
    senv->add(get<Symbol>(keyword), syntax);
    return none;
}
Cell Scheme::syntax_syntax_rules(const SymenvPtr& senv, Cell args)
{
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
        syntaxPtr->add(head, Procedure{ senv, head, body, true});
        rules = cdr(rules);
    }
    return syntaxPtr;
}
Cell Scheme::append_module_path(const std::vector<Cell>& vargs) {
    for(const auto& args: vargs) {
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
void Scheme::addenv(const Symbol& sym, const Cell& val)
{
    get_current_module_env()->add(sym, val);
}
void Scheme::addenv(std::initializer_list<std::pair<Symbol, Cell>> args)
{
    get_current_module_env()->add(args);
}
SymenvPtr Scheme::newenv(const SymenvPtr& env)
{
    return Symenv::create(env ? env : get_current_module_env());
}
Cell Scheme::set_current_module(const Cell& module_name)
{
    if (module_table.find(module_name) == module_table.end()) {
        throw module_error("No module:", module_name);
    }
    auto ret = current_module;
    current_module = module_name;
    return ret;
}
Intern Scheme::_get_intern(const SymenvPtr& senv, const Cell& cell)
{
    if (!is_symbol(cell)) {
        return Intern::_none_;
    }
    auto val = senv->get(get<Symbol>(cell));
    if (!is_intern(val)) {
        return Intern::_none_;
    }
    return get<Intern>(val);
}
Cell Scheme::partial_eval(const SymenvPtr& senv, const Cell& cell) {
    auto ret = cell;
    auto it = ret;
    while (is_pair(it)) {
        auto item = car(it);
        if (is_pair(item)) {
            auto opcode = _get_intern(senv, car(item));
            if (opcode == Intern::_unquote) {
                set_car(it, eval(senv, cadr(item)));
            } else if (opcode == Intern::_unquotesplice) {
                auto next_it = cdr(it);
                auto val = eval(senv, cadr(item));
                set_car(it, car(val));
                while (is_pair(cdr(val))) {
                    set_cdr(it, cons(cadr(val), nil));
                    it = cdr(it);
                    val = cdr(val);
                }
                set_cdr(it, next_it);
            } else {
                set_car(it, partial_eval(senv, item));
            }
        }
        it = cdr(it);
    }
    return ret;
}

} // namespace pscm
