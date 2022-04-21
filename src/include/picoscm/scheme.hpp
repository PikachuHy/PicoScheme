/********************************************************************************/ /**
 * @file scheme.hpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#ifndef SCHEME_HPP
#define SCHEME_HPP

#include <list>
#include <stack>

#include "cell.hpp"
#include "gc.hpp"

namespace pscm {
class Guard {
public:
    Guard(std::function<void()> f): f(std::move(f)) {}
    ~Guard() {
        f();
    }
private:
    std::function<void()> f;
};
class GCollector;
class module_error: public std::runtime_error {
public:
    module_error(const std::string& msg, const Cell& module_name)
        : runtime_error("module error: " + msg + to_string(module_name))
    {

    }
    static std::string to_string(const Cell& module_name) {
        using Port = StringPort<Char>;
        Port os{ Port::out };
        os << module_name;
        return string_convert<char>(os.str());
    }
};

/**
 * Scheme interpreter class.
 */
class Scheme {
public:
    //! Optional connect this scheme interpreter to the environment of another interpreter.
    Scheme(const SymenvPtr& env = nullptr);
    //! Return the current module name
    const Cell& get_current_module() const { return current_module; }
    Cell set_current_module(const Cell& module_name);
    //! Return a shared pointer to the current module environment of this interpreter.
    SymenvPtr& get_current_module_env() { return module_table[get_current_module()]; };
    const SymenvPtr& get_current_module_env() const { return module_table.at(get_current_module()); };
    SymenvPtr get_module_env(const Cell& module_name);
    SymenvPtr load_module(const Cell& module_name, const SymenvPtr& env);
    Cell append_module_path(const std::vector<Cell>& vargs);
    
    //! Insert a new symbol and value or reassign an already bound value of an existing symbol
    //! at the top environment of this scheme interpreter.
    void addenv(const Symbol& sym, const Cell& val);

    //! Insert or reassign zero or more symbol, value pairs into the
    //! top environment of this interpreter.
    void addenv(std::initializer_list<std::pair<Symbol, Cell>> args);

    //! Create a new empty child environment, connected to the argument parent environment
    //! or if null-pointer, connected to the top environment of this interpreter.
    SymenvPtr newenv(const SymenvPtr& env = nullptr);

    void enable_debug() { debug = true; }
    void disable_debug() { debug = false; }
    bool debugging() { return debug; }
    /**
     * Return a pointer to a new cons-cell from the internal cons-cell store.
     * The new cons-cell is initialized by argument car and cdr values. The pointer
     * lifetime is managed by the internal garbage collector of this interpreter.
     *
     * @param  car Variant value to assign to cons-cell car slot.
     * @param  cdr Variant value to assign to cons-cell cdr slot.
     * @return Pointer to a new initialized Cons-cell.
     */
    template <typename CAR, typename CDR>
    Cons* cons(CAR&& car, CDR&& cdr)
    {
        if (store_size + dflt_gccycle_count < store.size()) {
            std::wcerr << "Garbage collector cycle" << std::endl;
            //gc.collect(*this, topenv);
            store_size = store.size();
        }
        return pscm::cons(store, std::forward<CAR>(car), std::forward<CDR>(cdr));
    }

    //! Build a cons list of all arguments.
    template <typename T, typename... Args>
    Cons* list(T&& t, Args&&... args)
    {
        return pscm::list(store, std::forward<T>(t), std::forward<Args>(args)...);
    }

    //! Create a new symbol or return an existing symbol, build from
    //! the argument string.
    template <typename StringT>
    Symbol symbol(const StringT& str)
    {
        return symtab[string_convert<Char>(str)];
    }

    //! Create a new symbol, guarenteed not to exist before.
    Symbol symbol()
    {
        return symbol(std::string{ "symbol " }.append(std::to_string(symtab.size())));
    }

    /**
     * Create a new ::Function object and install it into the argument
     * environment and bound to a symbol build from the argument name string.
     *
     * External function signature:
     *   fun(Scheme& scm, const SymenvPtr& env, const std::vector<Cell>& argv) -> Cell
     *
     * @param env  Environment pointer, where to add this function. If null-pointer,
     *             add to the top-environment of this interpreter.
     * @param name Symbol name of this function.
     * @param fun  External function or functor.
     *
     * @returns A shared pointer to the created ::Function object.
     */
    template <typename StringT, typename FunctionT>
    FunctionPtr function(const SymenvPtr& env, const StringT& name, FunctionT&& fun)
    {
        auto sym = symbol(name);
        auto funptr = Function::create(sym, std::forward<FunctionT>(fun));

        if (env)
            env->add(sym, funptr);
        else
            get_current_module_env()->add(sym, funptr);

        return funptr;
    }

    //! Create a new function and install it into the top-environment of this interpreter.
    template <typename StringT, typename FunctionT>
    FunctionPtr function(const StringT& name, FunctionT&& fun)
    {
        return function(nullptr, name, std::forward<FunctionT>(fun));
    }

    //! Create a new unnamed function object and install it into the argument environment
    //! of if null-pointer, into the top environment of this interpreter.
    template <typename FunctionT>
    FunctionPtr function(const SymenvPtr& env, FunctionT&& fun)
    {
        return function(env, String{ L"λ" }, std::forward<FunctionT>(fun));
    }

    Port<Char>& outPort() const { return *m_stdout; } //!< return a shared-pointer to the default input port
    Port<Char>& inPort() const { return *m_stdin; } //!< return a shared-pointer to the default output port

    //! Start a new read-eval-print loop and use argument environment or if null-pointer
    //! use the top-environment of this interpreter as interaction environment.
    void repl(const SymenvPtr& env = nullptr);

    //! Read scheme expressions from file and evaluate them at the argument
    //! environment or if null-pointer at the top-environment of this interpreter.
    void load(const String& filename, const SymenvPtr& env = nullptr);

    template <typename StringT>
    void load(const StringT& filename, const SymenvPtr& env = nullptr)
    {
        load(string_convert<Char>(filename), env);
    }

    /**
     * Evaluate a scheme expression at the argument symbol environment.
     *
     * @param env Shared pointer to the symbol environment, where to
     *            to evaluate expr.
     * @param expr Scheme expression to evaluate.
     * @return Evaluation result or special symbol @em none for no result.
     */
    Cell eval(SymenvPtr env, Cell expr);
    /**
     * Evaluate a scheme code at the argument symbol environment.
     *
     * @param env Shared pointer to the symbol environment, where to
     *            to evaluate expr.
     * @param code Scheme code to evaluate.
     * @return Evaluation result or special symbol @em none for no result.
     */
    Cell eval_string(SymenvPtr env, const String& code);
    template <typename StringT>
    Cell eval_string(SymenvPtr env, StringT code) {
        return eval(env, string_convert<Char>(code));
    }

    /**
     * Return a new list of evaluated expressions in argument list.
     *
     * @param env Symbol environment, where to evaluate the argument list.
     * @param args Argument list to evaluate.
     * @param is_list true:   procedure call argument list.
     *                false:  apply expression argument list, where the last list item
     *                        must be nil or an argument list itself.
     * @return List of evaluated argument expressions.
     */
    Cell eval_list(const SymenvPtr& env, Cell list, bool is_list = true);

    /**
     * Evaluate argument list into an argument vector.
     *
     * @param senv Symbol environment, where to evaluate the argument list.
     * @param args Argument list to evaluate.
     * @param is_list true:   procedure call argument list.
     *                false:  apply expression argument list, where the last list item
     *                        must be nil or an argument list itself.
     * @return Vector of evaluated arguments.
     */
    std::vector<Cell> eval_args(const SymenvPtr& env, Cell args, bool is_list = true);

    /**
     * Call an external function or procedure opcode.
     *
     * @param senv  The current symbol environment.
     * @param proc  Scheme function opcode as defined by enum class @ref pscm::Intern.
     * @param args  Function argument vector.
     * @return Function result or special symbol @ref pscm::none for a void function.
     */
    Cell apply(const SymenvPtr& env, Intern opcode, const std::vector<Cell>& args);
    Cell apply(const SymenvPtr& env, const FunctionPtr& proc, const std::vector<Cell>& args);
    Cell apply(const SymenvPtr& env, const Cell& cell, const std::vector<Cell>& args);
    Cell apply(const SymenvPtr& env, const Procedure& proc, const Cell& args);
    std::pair<SymenvPtr, Cell> apply(const SymenvPtr& senv, const Cell& proc, const Cell& args, bool is_list = true);

    Cell expand(const Cell& macro, Cell& args);

    /**
     * Evaluate each expression in argument list up the last, which
     * is returned unevaluated. This last expression is evaluated at
     * the call site to support unbound tail-recursion.
     */
    Cell syntax_begin(const SymenvPtr& env, Cell args);

protected:
    Cell syntax_if(const SymenvPtr& env, const Cell& args);

    /**
     * Scheme syntax cond.
     *
     * @verbatim
     * (cond <clause>_1 <clause>_2 ...)
     *
     * <clause> := (<test> <expression> ...)
     *          |  (<test> => <expression> ...)
     *          |  (else  <expression> ...)
     * @endverbatim
     */
    Cell syntax_cond(const SymenvPtr& env, Cell args);

    /**
     * Scheme syntax case.
     *
     * @verbatim
     * (case <key> <clause>_1 <clause>_2 ...)
     *
     * <clause> := ((<datum_1> ...) <expression_1> <expression_2> ...)
     *          |  (else  <expression_1> <expression_2> ...)
     * @endverbatim
     */
    Cell syntax_case(const SymenvPtr& env, Cell args);

    /**
     * Scheme syntax do.
     *
     * @verbatim
     * (do ((<variable_1> <init_1> <step_1>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     * @endverbatim
     */
    Cell syntax_do(const SymenvPtr& env, Cell args);

    Cell syntax_when(const SymenvPtr& env, Cell args);

    Cell syntax_unless(const SymenvPtr& env, Cell args);

    Cell syntax_let(const SymenvPtr& env, Cell args, bool star=false);

    Cell syntax_with_let(const SymenvPtr& env, Cell args);

    Cell syntax_with_module(const SymenvPtr& env, Cell args);

    Cell syntax_and(const SymenvPtr& env, Cell args);

    Cell syntax_or(const SymenvPtr& env, Cell args);

    Cell syntax_module(const SymenvPtr& senv, const Cell& args);

    Cell syntax_inherit_module(const SymenvPtr& senv, Cell args);

    Cell syntax_use_module(const SymenvPtr& senv, Cell args);

    Cell syntax_quasiquote(const SymenvPtr& senv, Cell args);

    /**
     * Scheme syntax define-syntax.
     *
     * @verbatim
     * (define-syntax <keyword> <transformer spec>)
     *
     * <transformer spec> := (syntax-rules <literals> <syntax rule> ...)
     *
     * <literals> := (<pattern> <template>)
     *
     * <pattern> := (<pattern> ...)
     *           |  (<pattern> <pattern> ... . <pattern>)
     *           |  (<pattern> ... <pattern> <ellipsis>)
     *           |  #(<pattern> ...)
     *           |  #(<pattern> ... <pattern> <ellipsis>)
     *
     * <template> := (<element> ...)
     *            |  (<element> <element> ... . <template>)
     *            |  (<element> ...)
     */
    Cell syntax_define_syntax(const SymenvPtr& senv, Cell args);
    Cell syntax_syntax_rules(const SymenvPtr& senv, Cell args);

    /**
     * Scheme syntax delay.
     *
     * @verbatim
     * (delay <expression>)
     *
     * @endverbatim
     */
    Cell syntax_delay(const SymenvPtr& senv, const Cell& args);

    Intern _get_intern(const SymenvPtr& senv, const Cell& cell);

    Cell partial_eval(const SymenvPtr& senv, const Cell& cell, int nesting = 0);

private:
    friend class GCollector;
    static constexpr size_t dflt_bucket_count = 1024; //<! Initial default hash table bucket count.
    static constexpr size_t dflt_gccycle_count = 10000; //<! GC cycle after dflt_gccycle_count cons-cell allocations.

    using standard_port = StandardPort<Char>;
    PortPtr m_stdin = std::make_shared<standard_port>(standard_port::in);
    PortPtr m_stdout = std::make_shared<standard_port>(standard_port::out);

    GCollector gc;
    std::list<Cons> store;
    size_t store_size = 0;

    Symtab symtab{ dflt_bucket_count };
    std::unordered_map<Cell, SymenvPtr, hash<Cell>, cell_equal<Cell>> module_table;
    std::vector<String> module_paths;
    std::stack<Cell> module_stack;
    Cell current_module;
    String cur_file;

    bool debug = false;
};

} // namespace pscm

#endif // SCHEME_HPP
