/********************************************************************************/
/**
 * @file types.hpp
 *
 * @version   0.1
 * @date      2018-
 * @author    Paul Pudewills
 * @copyright MIT License
 *************************************************************************************/
#ifndef TYPES_HPP
#define TYPES_HPP

#include <map>
#include <memory>
#include <regex>
#include <string>
#include <variant>

#include "port.hpp"
#include "symbol.hpp"

namespace pscm {

// clang-format off
template<typename T>
using sptr = std::shared_ptr<T>;

struct Cell;
struct Number;
struct Label;
class  Clock;
class  Procedure;
class  Function;
class  CObj;
class  Syntax;
class  Continuation;
class  Frame;
class  Module;
enum class Intern;
template<typename Cell> struct less;
template<typename Cell> struct hash;
template <typename Cell>
struct cell_equal {
    bool operator()(const Cell& lhs, const Cell& rhs) const {
        return is_equal(lhs, rhs);
    }
};

using None        = std::monostate;
using Nil         = std::nullptr_t;
using Bool        = bool;
using Char        = wchar_t;
using Cons        = std::tuple</*car*/Cell, /*cdr*/Cell, /*gc-mark*/bool>;
using String      = std::basic_string<Char>;
using StringPtr   = std::shared_ptr<String>;
using StringView  = std::basic_string_view<Char>;
using ClockPtr    = std::shared_ptr<Clock>;
using RegexPtr    = std::shared_ptr<std::basic_regex<Char>>;
using MapPtr      = std::shared_ptr<std::multimap<Cell,Cell,less<Cell>>>;
using HashMapPtr  = std::shared_ptr<std::unordered_map<Cell,Cell,hash<Cell>, cell_equal<Cell>>>;
template<typename T>
using CellHashMap = std::unordered_map<Cell,T,hash<Cell>, cell_equal<Cell>>;
using SyntaxPtr   = std::shared_ptr<Syntax>;
using CObjPtr     = std::shared_ptr<CObj>;
using ContPtr     = std::shared_ptr<Continuation>;
using VectorPtr   = std::shared_ptr<std::vector<Cell>>;
using PortPtr     = std::shared_ptr<Port<Char>>;
using FunctionPtr = std::shared_ptr<Function>;
using Symtab      = SymbolTable<String>;
using Symbol      = Symtab::Symbol;
using Symenv      = SymbolEnv<Symbol, Cell, Symbol::hash>;
using SymenvPtr   = std::shared_ptr<Symenv>;
using Keyword     = Symtab::Keyword;
using FrameStack  = std::vector<Frame>;
using Variant = std::variant <

    /* Atom types: */
    None, Nil, Intern, Bool, Char, Number,

    /* Compound value types: */
    Symbol, Keyword, Procedure,

    /* Pointer types: */
    Cons*, StringPtr, VectorPtr, PortPtr, FunctionPtr, SymenvPtr,

    /* Extensions: */
    RegexPtr, ClockPtr, MapPtr, HashMapPtr, CObjPtr, SyntaxPtr, ContPtr,
    Module,

    /* Compiler: */
    Label
>;

static const None none {}; //!< void return symbol
static const Nil  nil  {}; //!< empty list symbol
// clang-format on

enum class Intern {
    _none_,
    _done_,
    _runtime_value_,
    /* Scheme syntax opcodes: */
    _or,
    _and,
    _let,
    _let_star,
    _with_let,
    _with_module,
    _if,
    _cond,
    _else,
    _case,
    _do,
    _arrow,
    _when,
    _unless,
    _define,
    _define_public,
    _define_syntax,
    _syntax_rules,
    _setb,
    _begin,
    _lambda,
    _macro,
    _macro_public,
    _apply,
    _quote,
    _quasiquote,
    _unquote,
    _unquotesplice,
    op_dynamic_wind,
    _expand,
    _values,

    /* Section 6.1: Equivalence predicates */
    op_eq,
    op_eqv,
    op_equal,

    /* Section 6.2: Numbers */
    op_isnum,
    op_iscpx,
    op_isreal,
    op_israt,
    op_isint,
    op_isexact,
    op_isinexact,
    op_isexactint,
    op_ex2inex,
    op_inex2ex,
    op_isodd,
    op_iseven,
    op_numeq,
    op_numlt,
    op_numgt,
    op_numle,
    op_numge,
    op_min,
    op_max,
    op_ispos,
    op_isneg,
    op_zero,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_rem,
    op_floor,
    op_ceil,
    op_trunc,
    op_round,
    op_quotient,
    op_sin,
    op_cos,
    op_tan,
    op_asin,
    op_acos,
    op_atan,
    op_sinh,
    op_cosh,
    op_tanh,
    op_asinh,
    op_acosh,
    op_atanh,
    op_sqrt,
    op_cbrt,
    op_exp,
    op_pow,
    op_square,
    op_log,
    op_log10,
    op_abs,
    op_real,
    op_imag,
    op_arg,
    op_conj,
    op_rect,
    op_polar,
    op_hypot,
    op_strnum,
    op_numstr,

    /* Section 6.3: Booleans */
    op_not,
    op_isbool,
    op_isbooleq,

    /* Section 6.4: Pair and lists */
    op_cons,
    op_car,
    op_cdr,
    op_caar,
    op_cddr,
    op_cadr,
    op_cdar,
    op_caddr,
    op_cdadr,
    op_setcdr,
    op_setcar,
    op_list,
    op_isnil,
    op_ispair,
    op_islist,
    op_mklist,
    op_length,
    op_append,
    op_reverse,
    op_reverseb,
    op_tail,
    op_listref,
    op_listsetb,
    op_listcopy,
    op_list_tail,
    op_list_head,
    op_memq,
    op_memv,
    op_member,
    op_assq,
    op_assv,
    op_assoc,

    /* Section 6.5: Symbols */
    op_issym,
    op_symstr,
    op_strsym,
    op_symeql,
    op_gensym,

    /* Section 6.6: Characters */
    op_ischar,
    op_ischareq,
    op_ischarlt,
    op_ischargt,
    op_ischarle,
    op_ischarge,
    op_ischcieq,
    op_ischcilt,
    op_ischcigt,
    op_ischcile,
    op_ischcige,
    op_isalpha,
    op_isdigit,
    op_iswspace,
    op_isupper,
    op_islower,
    op_charint,
    op_intchar,
    op_digitval,
    op_upcase,
    op_downcase,
    op_foldcase,

    /* Section 6.7: Strings */
    op_mkstr,
    op_str,
    op_strlen,
    op_strref,
    op_strsetb,
    op_isstr,
    op_isstreq,
    op_isstrcieq,
    op_isstrgt,
    op_isstrcigt,
    op_isstrlt,
    op_isstrcilt,
    op_isstrge,
    op_isstrcige,
    op_isstrle,
    op_isstrcile,
    op_strupcase,
    op_strdowncase,
    op_strupcaseb,
    op_strdowncaseb,
    op_strfoldcase,
    op_strappend,
    op_strappendb,
    op_strlist,
    op_liststr,
    op_substr,
    op_strcopy,
    op_strcopyb,
    op_strfillb,

    /* Section 6.8: Vectors */
    op_isvec,
    op_mkvec,
    op_vec,
    op_veclen,
    op_vecref,
    op_vecsetb,
    op_veclist,
    op_listvec,
    op_vecstr,
    op_strvec,
    op_veccopy,
    op_veccopyb,
    op_vecappend,
    op_vecappendb,
    op_vecfillb,

    /* Section 6.9: Bytevectors */

    /* Section 6.10: Control features */
    op_isproc,
    op_map,
    op_strmap,
    op_vecmap,
    op_foreach,
    op_strforeach,
    op_vecforeach,
    op_callcc,
    op_values,
    op_callwval,
    op_dynwind,

    /* Section 6.11: Exceptions */
    op_error,
    op_with_exception,

    /* Section 6.12: Environments and evaluation */
    op_exit,
    op_replenv,
    op_repl,
    op_eval,
    op_gc,
    op_gcdump,
    op_macroexp,

    /* Section 6.13: Input and output */
    op_isport,
    op_isinport,
    op_isoutport,
    op_istxtport,
    op_isbinport,
    op_isinport_open,
    op_isoutport_open,
    op_inport,
    op_outport,
    op_errport,
    op_callw_port,
    op_callw_infile,
    op_callw_outfile,
    op_with_infile,
    op_with_outfile,
    op_open_infile,
    op_open_inbinfile,
    op_open_outfile,
    op_open_outbinfile,
    op_close_port,
    op_close_inport,
    op_close_outport,
    op_open_instr,
    op_open_outstr,
    op_open_inbytevec,
    op_open_outbytevec,
    op_get_outbytevec,
    op_read,
    op_read_char,
    op_peek_char,
    op_readline,
    op_eof,
    op_flush,
    op_iseof,
    op_char_ready,
    op_read_str,
    op_read_u8,
    op_peek_u8,
    op_ready_u8,
    op_read_bytevec,
    op_read_bytevecb,
    op_write,
    op_display,
    op_display_red,
    op_display_green,
    op_write_shared,
    op_write_simple,
    op_newline,
    op_write_char,
    op_write_str,
    op_write_u8,
    op_write_bytevec,

    /* Section 6.14: System Interface */
    op_load,
    op_fileok,
    op_delfile,
    op_cmdline,
    op_exitb,
    op_getenv,
    op_currsec,
    op_currjiffy,
    op_jiffspsec,
    op_features,

    op_gcd,
    op_lcm,

    /* Section extensions: regular expressions */
    op_regex,
    op_regex_match,
    op_regex_search,
    op_regex_replace,

    /* Section extensions: clock */
    op_clock,
    op_clock_tic,
    op_clock_toc,
    op_clock_pause,
    op_clock_resume,

    /* Section extensions: dictionary */
    op_make_dict,
    op_dict_isempty,
    op_dict_size,
    op_dict_clear,
    op_dict_erase,
    op_dict_insert,
    op_dict_find,
    op_dict_count,
    op_dict_equal_range,
    op_dict2list,
    op_list2dict,

    op_make_hash_table,
    op_hash_table_ref,
    op_hash_table_set,
    op_hash_table_remove,
    op_hash_table_get_handle,
    op_hash_table_fold,

    op_usecount,
    op_hash,

    op_is_keyword,
    op_defined,

    op_current_module,
    op_set_current_module,
    op_module_name,
    _module,
    _inherit_module,
    _use_module,
    op_append_module_path,

    op_call_with_output_string,
    op_flush_output,

    op_enable_debug,
    op_disable_debug,

    op_lookup_variable_value,
    op_set_variable_value,
    op_define_variable,
    op_is_false,
    op_is_apply,
    op_make_apply_args,
    op_make_compiled_procedure,
    op_make_compiled_macro,
    op_compiled_procedure_env,
    op_compiled_procedure_entry,
    op_extend_environment,
    op_is_primitive_procedure,
    op_apply_primitive_procedure,
    op_make_dynamic_wind,
    op_pop_dynamic_wind,
    op_module_env,
    op_make_module,
    op_use_module,
    op_inherit_module,

    op_genport,
    op_get_port_string,

    op_gencont,

    op_get_current_environment,
    op_machine_print_trace,

    op_inc,

    op_export,

    op_scm_error,
};
} // namespace pscm
#endif // TYPES_HPP
