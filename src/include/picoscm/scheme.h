/********************************************************************************/
/**
 * @file scheme.h
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#ifndef PICOSCHEME_SCHEME_H
#define PICOSCHEME_SCHEME_H
#include "types.hpp"

namespace pscm {
class SchemeImpl;
class Machine;

class module_error : public std::runtime_error {
public:
    module_error(const std::string& msg, const Cell& module_name)
        : runtime_error("module error: " + msg + to_string(module_name)) {
    }

    static std::string to_string(const Cell& module_name) {
        using Port = StringPort<Char>;
        Port os{ Port::out };
        os << module_name;
        return string_convert<char>(os.str());
    }
};

class Scheme {
public:
    Scheme();
    Symbol symbol(const std::string& name);
    Symbol symbol(const String& name);
    Symbol symbol();
    void load(const std::string& filename, const SymenvPtr& env = nullptr);
    void load(const String& filename, const SymenvPtr& env = nullptr);
    void repl(const SymenvPtr& env = nullptr);
    Cell cons(const Cell& a, const Cell& b);
    Cell list(const Cell& a);
    Cell list(const Cell& a, const Cell& b);
    Cell list(const Cell& a, const Cell& b, const Cell& c);
    Cell list(const Cell& a, const Cell& b, const Cell& c, const Cell& e);
    Cell eval(const SymenvPtr& env, const Cell& expr);
    Cell apply(const SymenvPtr& env, const Cell& cell, const std::vector<Cell>& args);
    using FuncObj = std::function<Cell(Scheme& scm, const SymenvPtr& senv, const std::vector<Cell>& args)>;
    FunctionPtr function(const SymenvPtr& env, const std::string& name, const FuncObj& f);
    void addenv(const Symbol& sym, const Cell& cell);
    Port<Char>& outPort();
    Port<Char>& inPort();
    sptr<Machine>& machine();
    Module module(const Cell& module_name);
    bool module_exist(const Cell& module_name);
    void add_module(const Cell& module_name, const Module& module);
    Module& current_module();
    Module set_current_module(const Cell& cell);
    Cell append_module_path(const std::vector<Cell>& vargs);

private:
    sptr<SchemeImpl> impl;
};
} // namespace pscm
#endif // PICOSCHEME_SCHEME_H
