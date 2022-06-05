/********************************************************************************/
/**
 * @file scheme.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#include "picoscm/scheme.h"
#include "picoscm/cell.hpp"
#include "picoscm/machine.h"
#include "picoscm/parser.hpp"
#include "picoscm/port.hpp"
#include "picoscm/primop.hpp"
#include <iostream>
#include <list>
#include <string>

namespace fs = std::filesystem;

namespace pscm {
struct SchemeImpl {
    SchemeImpl(Scheme& scm)
        : scm(scm)
        , m_machine(std::make_shared<Machine>(scm)) {
    }

    void init() {
        auto std_env = add_environment_defaults(scm);
        auto cwd = fs::current_path().string();
        module_paths.push_back(string_convert<Char>(cwd));
        current_module = Module(list(symbol(L"root")), Symenv::create(std_env));
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
    }

    Symbol symbol(const String& name) {
        return symtab[name];
    }

    Symbol new_symbol() {
        String name = L"symbol ";
        name.append(std::to_wstring(symtab.size()));
        return symtab[name];
    }

    void repl(const SymenvPtr& env) {
        auto senv = env ? env : current_module.env();
        Cell expr;
        String s;
        try {
            while (true) {
                expr = none;
                s.clear();
                std::wcout << "> ";
                std::getline(std::wcin, s);
                Parser parser(scm, s);
                expr = parser.read();
                auto ret = m_machine->run(senv, expr);
                if (is_none(ret)) {
                    continue;
                }
                if (is_exit(expr)) {
                    return;
                }
                std::wcout << ret << std::endl;
            }
        }
        catch (const std::exception& ex) {
            std::wcout << "ERROR MESSAGE: " << ex.what() << std::endl;
            std::wcout << "RAW STRING   : " << s << std::endl;
            if (is_none(expr)) {
                return;
            }
            std::wcout << "PARSED EXPR  : " << expr << std::endl;
        }
    }

    void load(const String& filename, const SymenvPtr& env) {
        DEBUG_OUTPUT("load:", filename);
        using namespace std::chrono;
        auto t0 = high_resolution_clock::now();
        auto senv = env ? env : current_module.env();
        Cell expr;
        try {
            std::wifstream in(filename, std::ifstream::in);
            using Iter = std::istreambuf_iterator<Char>;
            String content((Iter(in)), Iter());
            Parser parser(scm, content);
            while (!parser.is_finished()) {
                expr = parser.read();
                if (is_eof(expr)) {
                    break;
                }
                // DEBUG_OUTPUT("expr:", expr);
                auto ret = m_machine->run(senv, expr);
            }
        }
        catch (const parse_error& ex) {
            DEBUG_OUTPUT(ex.what());
            std::wcout << filename << ":" << ex.row << std::endl;
            std::wcout << "ERROR MESSAGE: " << ex.what() << std::endl;
            // std::wcout << "RAW STRING   : " << s << std::endl;
            if (is_none(expr)) {
                return;
            }
            std::wcout << "PARSED EXPR  : " << expr << std::endl;
        }
        catch (const std::bad_variant_access& ex) {
            DEBUG_OUTPUT(ex.what());
            std::wcout << filename << std::endl;
            std::wcout << "ERROR MESSAGE: " << ex.what() << std::endl;
            // std::wcout << "RAW STRING   : " << s << std::endl;
            if (is_none(expr)) {
                return;
            }
            std::wcout << "PARSED EXPR  : " << expr << std::endl;
        }
        auto t1 = high_resolution_clock::now();
        duration<double, std::ratio<1, 1>> cost_time = t1 - t0;
        DEBUG_OUTPUT("load", filename, "cost time:", cost_time.count(), "seconds");
    }

    Cell cons(const Cell& a, const Cell& b) {
        return pscm::cons(store, a, b);
    }

    //! Build a cons list of all arguments.
    template <typename T, typename... Args>
    Cons *list(T&& t, Args&&...args) {
        return pscm::list(store, std::forward<T>(t), std::forward<Args>(args)...);
    }

    Cell eval(const SymenvPtr& env, const Cell& expr) {
        auto senv = env ? env : current_module.env();
        return m_machine->run(senv, expr);
    }

    template <typename FunctionT>
    FunctionPtr function(const SymenvPtr& env, const String& name, const FunctionT& f) {
        auto senv = env ? env : current_module.env();
        auto sym = symbol(name);
        auto new_f = Function::create(sym, f);
        senv->add(sym, new_f);
        return new_f;
    }

    Module module(const Cell& module_name) {
        auto it = module_table.find(module_name);
        if (it != module_table.end()) {
            return it->second;
        }
        // load module
        return load_module(module_name, current_module.env());
    }

    Module load_module(const Cell& module_name, const SymenvPtr& env) {
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
            load(module_path, nullptr);
            auto it = module_table.find(module_name);
            if (it == module_table.end()) {
                throw module_error("load module, no module defined", module_name);
            }
            return it->second;
        }
        throw module_error("no module: ", module_name);
    }

    Symtab symtab{ 1024 };
    CellHashMap<Module> module_table;
    std::vector<String> module_paths;
    std::stack<Module> module_stack;
    std::list<Cons> store;
    Module current_module;
    using standard_port = StandardPort<Char>;
    PortPtr m_stdin = std::make_shared<standard_port>(standard_port::in);
    PortPtr m_stdout = std::make_shared<standard_port>(standard_port::out);
    Scheme& scm;
    sptr<Machine> m_machine;
};

Scheme::Scheme()
    : impl(std::make_shared<SchemeImpl>(*this)) {
    impl->init();
}

Symbol Scheme::symbol(const std::string& name) {
    return impl->symbol(string_convert<Char>(name));
}

Symbol Scheme::symbol(const String& name) {
    return impl->symbol(name);
}

Symbol Scheme::symbol() {
    return impl->new_symbol();
}

void Scheme::load(const std::string& filename, const SymenvPtr& env) {
    impl->load(string_convert<Char>(filename), env);
}

void Scheme::load(const String& filename, const SymenvPtr& env) {
    impl->load(filename, env);
}

void Scheme::repl(const SymenvPtr& env) {
    impl->repl(env);
}

Cell Scheme::cons(const Cell& a, const Cell& b) {
    return impl->cons(a, b);
}

Cell Scheme::list(const Cell& a) {
    return impl->list(a);
}

Cell Scheme::list(const Cell& a, const Cell& b) {
    return impl->list(a, b);
}

Cell Scheme::list(const Cell& a, const Cell& b, const Cell& c) {
    return impl->list(a, b, c);
}

Cell Scheme::list(const Cell& a, const Cell& b, const Cell& c, const Cell& d) {
    return impl->list(a, b, c, d);
}

Cell Scheme::eval(const SymenvPtr& env, const Cell& expr) {
    return impl->eval(env, expr);
}

FunctionPtr Scheme::function(const SymenvPtr& env, const std::string& name, const FuncObj& f) {
    return impl->function(env, string_convert<Char>(name), f);
}

void Scheme::addenv(const Symbol& sym, const Cell& cell) {
    impl->current_module.env()->add(sym, cell);
}

Cell Scheme::apply(const SymenvPtr& env, const Cell& cell, const std::vector<Cell>& args) {
    return none;
}

Port<Char>& Scheme::outPort() {
    return *impl->m_stdout;
}

Port<Char>& Scheme::inPort() {
    return *impl->m_stdin;
}

sptr<Machine>& Scheme::machine() {
    return impl->m_machine;
}

Module Scheme::module(const Cell& module_name) {
    return impl->module(module_name);
}

bool Scheme::module_exist(const Cell& module_name) {
    return impl->module_table.find(module_name) != impl->module_table.end();
}

void Scheme::add_module(const Cell& module_name, const Module& module) {
    impl->module_table.insert_or_assign(module_name, module);
    impl->module_stack.push(module);
}

Module& Scheme::current_module() {
    return impl->current_module;
}

Module Scheme::set_current_module(const Cell& cell) {
    if (!is_module(cell)) {
        DEBUG_OUTPUT("args");
        throw module_error("No module:", cell);
    }
    auto ret = impl->current_module;
    impl->current_module = get<Module>(cell);
    return ret;
}

Cell Scheme::append_module_path(const std::vector<Cell>& vargs) {
    for (const auto& args : vargs) {
        auto path = get<StringPtr>(args);
        impl->module_paths.push_back(*path);
    }
    return none;
}
} // namespace pscm
