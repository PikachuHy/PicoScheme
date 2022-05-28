/********************************************************************************/
/**
 * @file compiler.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#include "picoscm/compiler.h"
#include "picoscm/machine.h"
#include "picoscm/port.hpp"
#include "picoscm/scheme.hpp"
#include "picoscm/syntax.h"

#include "impl/CodeListPrinter.h"
#include "impl/CompiledProcedureImpl.h"
#include "impl/MachineImpl.h"

#include <algorithm>
#include <ostream>

namespace pscm {
const char *compiler_error::what() const noexcept {
    return reason.c_str();
}

bool is_next_linkage(Linkage linkage) {
    return is_type<LinkageEnum>(linkage) && get<LinkageEnum>(linkage) == LinkageEnum::NEXT;
}

bool is_return_linkage(Linkage linkage) {
    return is_type<LinkageEnum>(linkage) && get<LinkageEnum>(linkage) == LinkageEnum::RETURN;
}

compiler_error::compiler_error(const std::string& msg, const InstCode& code) {
    using Port = StringPort<Char>;

    Port os{ Port::out };
    os << " ";
    reason.append(msg);
    reason.append(": ");
    reason.append(string_convert<char>(os.str()));
}
class Machine;

bool CompiledProcedure::operator!=(const CompiledProcedure& proc) const noexcept {
    return *impl != *proc.impl;
}

bool CompiledProcedure::operator==(const CompiledProcedure& proc) const noexcept {
    return !(*impl != *proc.impl);
}

Cell CompiledProcedure::env() const {
    return impl->senv;
}

Cell CompiledProcedure::entry() const {
    return impl->entry();
}

struct CompilerImpl {
    CompilerImpl(Scheme& scm, SymenvPtr env)
        : scm(scm)
        , m(scm) {
        env_stack.push(env);
    }

    bool is_last_expr(Cell expr) {
        return is_nil(cdr(expr));
    }

    Cell first_expr(const Cell& expr) {
        return car(expr);
    }

    Cell rest_expr(Cell expr) {
        return cdr(expr);
    }

    InstSeq compile_sequence(const Cell& expr, Target target, Linkage linkage) {
        if (is_nil(expr)) {
            return compile(none, target, linkage);
        }
        if (is_last_expr(expr)) {
            return compile(first_expr(expr), target, linkage);
        }
        else {
            auto regs = Regs{ Register::ENV, Register::CONTINUE };
            auto seq1 = compile(first_expr(expr), target, LinkageEnum::NEXT);
            auto seq2 = compile_sequence(rest_expr(expr), target, linkage);
            return preserving(regs, seq1, seq2);
        }
    }

    Cell cond_if_body(Cell clause) {
        auto item = cdr(clause);
        if (is_pair(item) && !is_nil(cdr(item))) {
            return scm.cons(Intern::_begin, item);
        }
        else {
            if (is_nil(item)) {
                return none;
            }
            return car(item);
        }
    }

    Cell handle_cond_arrow(const Cell& clause, const Cell& item) {
        auto maybe_arrow = cdr(clause);
        if (is_nil(maybe_arrow)) {
            maybe_arrow = none;
        }
        else {
            maybe_arrow = cadr(clause);
        }

        maybe_arrow = eval_op(maybe_arrow);
        if (is_nil(item)) {
            if (is_arrow(maybe_arrow)) {
                auto sym = scm.symbol();
                Cell new_if = scm.list(Intern::_if, sym, scm.list(caddr(clause), sym));
                return scm.list(Intern::_let, scm.list(scm.list(sym, car(clause))), new_if);
            }
            else {
                return scm.list(Intern::_if, car(clause), cond_if_body(clause));
            }
        }
        else {

            if (is_arrow(maybe_arrow)) {
                /*
                auto sym = scm.symbol("t");
                auto op_if = scm.symbol("if");
                auto op_let = scm.symbol("let");
                 */
                auto sym = scm.symbol();
                auto op_if = Intern::_if;
                auto op_let = Intern::_let;
                Cell new_if = scm.list(op_if, sym, scm.list(caddr(clause), sym), item);
                return scm.list(op_let, scm.list(scm.list(sym, car(clause))), new_if);
            }
            else {
                return scm.list(Intern::_if, car(clause), cond_if_body(clause), item);
            }
        }
    }

    Cell cond_to_if_sub(Cell clauses) {
        if (is_nil(clauses)) {
            return false;
        }
        auto clause = car(clauses);
        auto op = eval_op(car(clause));
        if (is_intern(op) && get<Intern>(op) == Intern::_else) {
            if (is_nil(cdr(clauses))) {
                return cond_if_body(clause);
            }
            throw compiler_error("ELSE clause isn't last: cond->if", clauses);
        }
        else {
            auto item = cond_to_if_sub(cdr(clauses));
            auto new_if = handle_cond_arrow(clause, item);
            return new_if;
        }
    }

    Cell cond_to_if(const Cell& expr) {
        auto clauses = cdr(expr);
        return cond_to_if_sub(clauses);
    }

    Cell and_to_if_sub(const Cell& expr) {
        if (is_nil(cdr(expr))) {
            return car(expr);
        }
        return scm.list(Intern::_if, car(expr), and_to_if_sub(cdr(expr)), false);
        //        return cons(Intern::_if, cons(car(expr), cons(and_to_if_sub(cdr(expr)), false)));
    }

    Cell and_to_if(const Cell& expr) {
        if (is_nil(cdr(expr))) {
            return true;
        }
        return and_to_if_sub(cdr(expr));
    }

    Cell or_to_if_sub(const Cell& expr) {
        if (is_nil(cdr(expr))) {
            return car(expr);
        }
        return scm.list(Intern::_if, car(expr), car(expr), or_to_if_sub(cdr(expr)));
    }

    Cell or_to_if(const Cell& expr) {
        if (is_nil(cdr(expr))) {
            return none;
        }
        return or_to_if_sub(cdr(expr));
    }

    bool is_self_evaluating(const Cell& expr) {
        if (is_nil(expr)) {
            return true;
        }
        if (is_intern(expr)) {
            return true;
        }
        if (is_number(expr)) {
            return true;
        }
        if (is_string(expr)) {
            return true;
        }
        if (is_char(expr)) {
            return true;
        }
        if (is_bool(expr)) {
            return true;
        }
        if (is_none(expr)) {
            return true;
        }
        if (is_proc(expr)) {
            return true;
        }
        if (is_port(expr)) {
            return true;
        }
        return false;
    }

    InstSeq compile_self_evaluating(Cell expr, Target target, const Linkage& linkage) {
        auto seq = CodeList{ Instruction::ASSIGN, target, expr };
        auto inst_seq = make_instruction_sequence({}, { target }, seq);
        return end_with_linkage(linkage, inst_seq);
    }

    Cell text_of_quotation(const Cell& expr) {
        return cadr(expr);
    }

    template <typename StringT>
    bool is_tagged_list(const Cell& expr, StringT tag) {
        return is_tagged_list(expr, string_convert<Char>(tag));
    }

    bool is_tagged_list(const Cell& expr, const String& tag) {
        if (!is_pair(expr)) {
            return false;
        }
        if (!is_symbol(car(expr))) {
            return false;
        }
        auto sym = get<Symbol>(car(expr));
        if (sym.value() == tag) {
            return true;
        }
        return false;
    }

    InstSeq compile_quoted(const Cell& expr, Target target, const Linkage& linkage) {
        auto seq = CodeList{ Instruction::ASSIGN, target, text_of_quotation(expr) };
        auto inst_seq = make_instruction_sequence({}, { target }, seq);
        return end_with_linkage(linkage, inst_seq);
    }

    bool is_variable(const Cell& expr) {
        return is_symbol(expr);
    }

    InstSeq compile_variable(Cell expr, Target target, const Linkage& linkage) {
        auto code = CodeList{ Instruction::ASSIGN, target, Intern::op_lookup_variable_value, expr, Register::ENV };
        auto seq = make_instruction_sequence({ Register::ENV }, { target }, code);
        return end_with_linkage(linkage, seq);
    }

    Cell assignment_variable(const Cell& expr) {
        return cadr(expr);
    }

    Cell assignment_value(const Cell& expr) {
        return caddr(expr);
    }

    InstSeq compile_assignment(const Cell& expr, Target target, Linkage linkage) {
        auto var = assignment_variable(expr);
        auto get_value_code = compile(assignment_value(expr), Register::VAL, LinkageEnum::NEXT);
        auto code = CodeList{ Instruction::PERFORM,
                              Intern::op_set_variable_value,
                              var,
                              Register::VAL,
                              Register::ENV,

                              Instruction::ASSIGN,
                              target,
                              ok };
        auto seq1 = make_instruction_sequence({ Register::ENV, Register::VAL }, { target }, code);
        auto seq2 = preserving({ Register::ENV }, get_value_code, seq1);
        return end_with_linkage(linkage, seq2);
    }

    Cell definition_variable(const Cell& expr) {
        if (is_symbol(cadr(expr))) {
            return cadr(expr);
        }
        else {
            return caadr(expr);
        }
    }

    Cell make_lambda(const Cell& parameters, const Cell& body) {
        return scm.cons(Intern::_lambda, scm.cons(parameters, body));
    }

    InstSeq compile_definition(const Cell& expr, Target target, Linkage linkage, bool is_macro = false) {
        Cell var;
        InstSeq get_value_code;
        if (is_symbol(cadr(expr))) {
            // (define var val)
            var = cadr(expr);
            get_value_code = compile(caddr(expr), Register::VAL, LinkageEnum::NEXT);
        }
        else {
            // (define (var arg) body)
            auto maybe_var = caadr(expr);
            if (is_symbol(maybe_var)) {
                var = maybe_var;
                auto f = make_lambda(cdadr(expr), cddr(expr));
                get_value_code = compile_lambda(f, Register::VAL, LinkageEnum::NEXT, is_macro).second;
            }
            else {
                // DEBUG_OUTPUT(maybe_var);
                // (define ((var arg0) arg1) body)
                // maybe_var => (var arg0)
                // cdadr arg1
                var = car(maybe_var);
                auto ff = make_lambda(cdadr(expr), cddr(expr));
                // DEBUG_OUTPUT(ff);
                auto f = make_lambda(cdr(maybe_var), scm.cons(ff, nil));
                // DEBUG_OUTPUT(f);
                get_value_code = compile_lambda(f, Register::VAL, LinkageEnum::NEXT, is_macro).second;
            }
        }
        auto code = CodeList{ Instruction::PERFORM,
                              Intern::op_define_variable,
                              var,
                              Register::VAL,
                              Register::ENV,

                              Instruction::ASSIGN,
                              target,
                              ok };
        auto seq1 = make_instruction_sequence({ Register::ENV, Register::VAL }, { target }, code);
        auto seq2 = preserving({ Register::ENV }, get_value_code, seq1);
        return end_with_linkage(linkage, seq2);
    }

    Cell if_predicate(const Cell& expr) {
        return cadr(expr);
    }

    Cell if_consequent(const Cell& expr) {
        return caddr(expr);
    }

    Cell if_alternative(const Cell& expr) {
        if (!is_nil(cdddr(expr))) {
            return cadddr(expr);
        }
        else {
            return false;
        }
    }

    InstSeq compile_if(const Cell& expr, Target target, const Linkage& linkage) {
        auto t_branch = make_label(LabelEnum::TRUE_BRANCH);
        auto f_branch = make_label(LabelEnum::FALSE_BRANCH);
        auto after_if = make_label(LabelEnum::AFTER_IF);
        Linkage consequent_linkage = linkage;
        if (is_next_linkage(linkage)) {
            consequent_linkage = after_if;
        }
        auto p_code = compile(if_predicate(expr), Register::VAL, LinkageEnum::NEXT);
        auto c_code = compile(if_consequent(expr), target, consequent_linkage);
        auto a_code = compile(if_alternative(expr), target, linkage);

        auto code0 = CodeList{ Instruction::TEST, Intern::op_is_false, Register::VAL,

                               Instruction::BRANCH, f_branch };
        auto seq0 = make_instruction_sequence({ Register::VAL }, {}, code0);
        InstSeq seq2 = parallel_instruction_sequences(
            append_instruction_sequences(InstSeq{ Instruction::LABEL, t_branch }, c_code),
            append_instruction_sequences(InstSeq{ Instruction::LABEL, f_branch }, a_code));
        auto seq3 = append_instruction_sequences(seq0, seq2, InstSeq{ Instruction::LABEL, after_if });
        return preserving({ Register::ENV, Register::CONTINUE }, p_code, seq3);
    }

    InstSeq tack_on_instruction_sequence(const InstSeq& seq, const InstSeq& body_seq) {
        return make_instruction_sequence(seq.needs, seq.modifies, append(seq.statements, body_seq.statements));
    }

    std::pair<Label, InstSeq> compile_lambda(const Cell& expr, Target target, const Linkage& linkage,
                                             bool is_macro = false) {
        auto proc_entry = make_label(LabelEnum::ENTRY);
        auto after_lambda = make_label(LabelEnum::AFTER_LAMBDA);
        auto lambda_linkage = linkage;
        if (is_next_linkage(linkage)) {
            lambda_linkage = after_lambda;
        }
        auto op = is_macro ? Intern::op_make_compiled_macro : Intern::op_make_compiled_procedure;
        auto code0 = CodeList{ Instruction::ASSIGN, target, op, cadr(expr), proc_entry, Register::ENV };
        auto seq1 = make_instruction_sequence({ Register::ENV }, { target }, code0);
        auto seq2 = end_with_linkage(lambda_linkage, seq1);
        auto seq3 = tack_on_instruction_sequence(seq2, compile_lambda_body(expr, proc_entry));
        auto seq4 = append_instruction_sequences(seq3, InstSeq{ Instruction::LABEL, after_lambda });
        seq4.statements.push_front(Comment{ L"lambda", expr });
        return { proc_entry, seq4 };
    }

    InstSeq compile_lambda_body(const Cell& expr, const Label& proc_entry) {
        auto formals = cadr(expr);
        CodeList inst_seq0 = { Instruction::LABEL,  proc_entry,

                               Instruction::ASSIGN, Register::ENV, Intern::op_compiled_procedure_env, Register::PROC,

                               Instruction::ASSIGN, Register::ENV, Intern::op_extend_environment,     formals,
                               Register::ARGL,      Register::ENV };
        auto inst_seq1 =
            make_instruction_sequence({ Register::ENV, Register::PROC, Register::ARGL }, { Register::ENV }, inst_seq0);
        // create new env
        auto new_env = Symenv::create(cur_env());
        while (is_pair(formals)) {
            auto var = car(formals);
            auto sym = get<Symbol>(var);
            formals = cdr(formals);
            new_env->add(sym, Intern::_runtime_value_);
        }
        push_env(new_env);
        auto inst_seq2 = compile_sequence(cddr(expr), Register::VAL, LinkageEnum::RETURN);
        pop_env();
        return append_instruction_sequences(inst_seq1, inst_seq2);
    }

    Cell eval_op(Cell op) {
        if (is_intern(op)) {
            return op;
        }
        if (!is_symbol(op)) {
            return none;
        }
        auto sym = get<Symbol>(op);
        auto env = cur_env();
        if (env->defined_sym(sym)) {
            auto val = env->get(sym);
            return val;
        }
        return none;
    }

    InstSeq compile_continuation_call(Cell expr, Target target, Linkage linkage) {
        //        DEBUG_OUTPUT("compile call/cc");
        auto after_call = make_label(LabelEnum::AFTER_CALL);
        auto code = CodeList{ Instruction::ASSIGN, Register::CONTINUE, after_call,

                              Instruction::CONT,

                              Instruction::ASSIGN, Register::ARGL,     Intern::op_list, Register::VAL };
        auto seq1 = make_instruction_sequence({ Register::VAL }, { Register::ARGL }, code);
        auto proc_code = compile(cadr(expr), Register::PROC, LinkageEnum::NEXT);
        auto regs = Regs{ Register::PROC, Register::CONTINUE };
        auto seq2 = compile_procedure_call(target, linkage);
        auto seq3 = preserving(regs, seq1, seq2);
        auto seq4 = preserving({ Register::ENV, Register::CONTINUE }, proc_code, seq3);
        auto seq5 = append_instruction_sequences(seq4, InstSeq{ Instruction::LABEL, after_call });
        return end_with_linkage(linkage, seq5);
    }

    Cell let_to_lambda(const Cell& expr) {
        auto body = cddr(expr);
        Cell formal = scm.cons(none, nil);
        Cell actual = scm.cons(none, nil);
        auto formal_it = formal;
        auto actual_it = actual;
        auto it = cadr(expr);
        while (is_pair(it)) {
            auto item = car(it);
            set_cdr(formal_it, scm.cons(car(item), nil));
            set_cdr(actual_it, scm.cons(cadr(item), nil));
            formal_it = cdr(formal_it);
            actual_it = cdr(actual_it);
            it = cdr(it);
        }
        auto f = make_lambda(cdr(formal), body);
        Cell code = scm.cons(f, cdr(actual));
        return code;
    }

    Cell cons(Cell a, Cell b) {
        return scm.cons(std::move(a), std::move(b));
    }

    Cell let_star_to_lambda(const Cell& expr) {
        auto bindings = cadr(expr);
        auto body = cddr(expr);
        // (if null? bindings) `((lambda () . ,body))
        if (is_nil(bindings)) {
            auto code = make_lambda(nil, body);
            return cons(code, nil);
        }
        /*
         * `(let (,(car bindings))
         *   (let* ,(cdr bindings) . body))
         */
        Cell code0 = cons(scm.symbol("let*"), cons(cdr(bindings), body));
        auto code1 = let_star_to_lambda(code0);
        //        DEBUG_OUTPUT("let* ret:", code1);
        auto code = cons(scm.symbol("let"), cons(cons(car(bindings), nil), code1));
        //        DEBUG_OUTPUT("let code:", Cell(code));
        auto code2 = let_to_lambda(code);
        return cons(code2, nil);
    }

    InstSeq compile_application(Cell expr, Target target, Linkage linkage) {
        auto op = eval_op(car(expr));
        if (is_macro(op)) {
            auto proc_macro = get<Procedure>(op);
            auto expand_code = scm.machine()->run(proc_macro, cdr(expr));
            // DEBUG_OUTPUT("expand code:", expand_code);
            return compile(expand_code, target, linkage);
        }
        else if (is_syntax(op)) {
            const auto& matched = get<SyntaxPtr>(op)->match(cdr(expr));
            auto expand_code = matched.expand_syntax(scm, expr);
            // DEBUG_OUTPUT("expand code:", expand_code);
            return compile_sequence(cdr(expand_code), target, linkage);
        }
        else if (is_intern(op)) {
            switch (get<Intern>(op)) {
            case Intern::op_callcc: {
                auto seq = compile_continuation_call(expr, target, linkage);
                seq.statements.push_front(Comment{ L"begin of", expr });
                seq.statements.push_back(Comment{ L"end of", expr });
                return seq;
            }
            case Intern::_let: {
                auto expand_code = let_to_lambda(expr);
                DEBUG_OUTPUT("expand let:", expand_code);
                auto seq = compile(expand_code, target, linkage);
                seq.statements.push_front(Comment{ L"expand", expand_code });
                seq.statements.push_front(Comment{ L"let", expr });
                return seq;
            }
            case Intern::_let_star: {
                auto code = let_star_to_lambda(expr);
                DEBUG_OUTPUT("expand code:", code);
                return compile(code, target, linkage);
            }
            default:
                break;
            }
        }

        auto proc_code = compile(car(expr), Register::PROC, LinkageEnum::NEXT);
        std::vector<InstSeq> operand_codes;
        auto it = cdr(expr);
        while (is_pair(it)) {
            auto inst_seq = compile(car(it), Register::VAL, LinkageEnum::NEXT);
            operand_codes.push_back(inst_seq);
            it = cdr(it);
        }
        auto regs = Regs{ Register::PROC, Register::CONTINUE };
        auto seq1 = construct_arglist(operand_codes);
        auto seq2 = compile_procedure_call(target, linkage);
        auto seq3 = preserving(regs, seq1, seq2);
        return preserving({ Register::ENV, Register::CONTINUE }, proc_code, seq3);
    }

    InstSeq compile_apply(const Cell& expr, Target target, Linkage linkage) {
        auto proc = cadr(expr);
        auto args = caddr(expr);
        // DEBUG_OUTPUT("proc:", proc);
        // DEBUG_OUTPUT("args:", args);
        auto proc_code = compile(proc, Register::PROC, LinkageEnum::NEXT);
        proc_code.statements.push_front(Comment{ L"apply proc:", proc });
        // CodeListPrinter(proc_code, true).print();
        // std::wcout << std::endl;
        auto args_code = compile(args, Register::ARGL, LinkageEnum::NEXT);
        args_code.statements.push_front(Comment{ L"apply proc args:", args });
        // CodeListPrinter(args_code, true).print();

        auto seq2 = compile_procedure_call(target, linkage);
        auto seq3 = preserving({ Register::PROC, Register::CONTINUE }, args_code, seq2);
        return append_instruction_sequences(proc_code, seq3);
    }

    InstSeq compile_values(const Cell& expr, Target target, Linkage linkage) {
        std::vector<InstSeq> operand_codes;
        auto it = cdr(expr);
        while (is_pair(it)) {
            auto inst_seq = compile(car(it), Register::VAL, LinkageEnum::NEXT);
            operand_codes.push_back(inst_seq);
            it = cdr(it);
        }
        auto seq1 = construct_arglist(operand_codes);
        auto seq2 = InstSeq{ Instruction::ASSIGN, Register::VAL, Register::ARGL };
        auto seq3 = InstSeq{ Instruction::GOTO, Register::CONTINUE };
        auto seq4 = append_instruction_sequences(seq1, seq2, seq3);
        seq4.statements.push_front(Comment{ L"values", expr });
        return seq4;
    }

    InstSeq compile_module(const Cell& expr, Target target, Linkage linkage) {
        auto seq = InstSeq{ Instruction::ASSIGN, Register::ENV, Intern::op_make_module, cadr(expr), Register::ENV };
        auto maybe_use = cddr(expr);
        if (!is_nil(maybe_use)) {
            auto use = car(maybe_use);
            if (is_symbol(car(use))) {
                auto sym = get<Symbol>(car(use));
                if (sym.value() == L":use") {
                    auto use_list = cdr(use);
                    while (is_pair(use_list)) {
                        auto use_seq = InstSeq{ Instruction::PERFORM, Intern::op_use_module, car(use_list) };
                        seq = append_instruction_sequences(seq, use_seq);
                        use_list = cdr(use_list);
                    }
                }
            }
        }
        return end_with_linkage(linkage, seq);
    }

    InstSeq compile_use_module(const Cell& expr, Target target, Linkage linkage) {
        scm.syntax_use_module(cur_env(), cdr(expr));
        return {};
    }

    InstSeq compile_inherit_module(const Cell& expr, Target target, Linkage linkage) {
        scm.syntax_inherit_module(cur_env(), cdr(expr));
        return {};
    }

    InstSeq compile_with_module(const Cell& expr, Target target, Linkage linkage) {
        auto seq1 = InstSeq{ Instruction::SAVE, Register::ENV };
        auto seq2 = compile(cadr(expr), Register::VAL, LinkageEnum::NEXT);
        auto seq3 = InstSeq{
            Instruction::ASSIGN,
            Register::ENV,
            Intern::op_module_env,
            Register::VAL,
        };
        auto seq4 = compile_sequence(cddr(expr), target, linkage);
        auto seq5 = InstSeq{ Instruction::RESTORE, Register::ENV };
        return append_instruction_sequences(seq1, seq2, seq3, seq4, seq5);
    }

    InstSeq compile_expand(const Cell& expr, Target target, Linkage linkage) {
        auto expr_to_expand = cadr(expr);
        // DEBUG_OUTPUT(expr_to_expand);
        Cell op = car(expr_to_expand);
        Cell expand_code = expr_to_expand;
        if (is_symbol(op)) {
            op = eval_op(op);
            // DEBUG_OUTPUT(op);
            if (is_macro(op)) {
                auto proc_macro = get<Procedure>(op);
                // DEBUG_OUTPUT(cdr(expr_to_expand));
                expand_code = scm.machine()->run(proc_macro, cdr(expr_to_expand));
            }
        }
        // DEBUG_OUTPUT("expand:", expand_code);
        return compile_self_evaluating(expand_code, target, linkage);
    }

    InstSeq compile_procedure_call(Target target, const Linkage& linkage) {
        auto primitive_branch = make_label(LabelEnum::PRIMITIVE_BRANCH);
        auto compiled_branch = make_label(LabelEnum::COMPILED_BRANCH);
        auto after_call = make_label(LabelEnum::AFTER_CALL);
        auto compiled_linkage = linkage;
        if (is_next_linkage(linkage)) {
            compiled_linkage = after_call;
        }
        auto code0 = CodeList{ Instruction::TEST, Intern::op_is_primitive_procedure, Register::PROC,

                               Instruction::BRANCH, primitive_branch };
        auto seq0 = make_instruction_sequence({ Register::PROC }, {}, code0);
        auto seq1 = compile_proc_apply(target, compiled_linkage);
        auto code1 = CodeList{ Instruction::ASSIGN, target, Intern::op_apply_primitive_procedure, Register::PROC,
                               Register::ARGL };
        auto seq2 = make_instruction_sequence({ Register::PROC, Register::ARGL }, { target }, code1);
        auto seq3 = end_with_linkage(linkage, seq2);
        auto seq4 = parallel_instruction_sequences(
            append_instruction_sequences(InstSeq{ Instruction::LABEL, compiled_branch }, seq1),
            append_instruction_sequences(InstSeq{ Instruction::LABEL, primitive_branch }, seq3));
        return append_instruction_sequences(seq0, seq4, InstSeq{ Instruction::LABEL, after_call });
    }

    InstSeq compile_proc_apply(Target target, Linkage linkage) {
        std::vector<Register> all_regs = { Register::ENV, Register::PROC, Register::VAL, Register::ARGL,
                                           Register::CONTINUE };
        if (target == Register::VAL && !is_return_linkage(linkage)) {
            auto link = get<Label>(linkage);
            auto code = CodeList{
                Instruction::ASSIGN, Register::CONTINUE, link,

                Instruction::ASSIGN, Register::VAL,      Intern::op_compiled_procedure_entry,
                Register::PROC,

                Instruction::GOTO,   Register::VAL,
            };
            return make_instruction_sequence({ Register::PROC }, all_regs, code);
        }
        else if (target != Register::VAL && !is_return_linkage(linkage)) {
            auto proc_return = make_label(LabelEnum::PROC_RETURN);
            auto link = get<Label>(linkage);
            auto code = CodeList{ Instruction::ASSIGN,
                                  Register::CONTINUE,
                                  proc_return,

                                  Instruction::ASSIGN,
                                  Register::VAL,
                                  Intern::op_compiled_procedure_entry,
                                  Register::PROC,

                                  Instruction::GOTO,
                                  Register::VAL,

                                  Instruction::LABEL,
                                  proc_return,

                                  Instruction::ASSIGN,
                                  target,
                                  Register::VAL,

                                  Instruction::GOTO,
                                  link };
            return make_instruction_sequence({ Register::PROC }, all_regs, code);
        }
        else if (target == Register::VAL && is_return_linkage(linkage)) {
            auto code =
                CodeList{ Instruction::ASSIGN, Register::VAL, Intern::op_compiled_procedure_entry, Register::PROC,

                          Instruction::GOTO,   Register::VAL };
            return make_instruction_sequence({ Register::PROC, Register::CONTINUE }, all_regs, code);
        }
        else {
            DEBUG_OUTPUT("target:", target);
            throw std::runtime_error("return linkage, target not val: COMPILE");
        }
    }

    InstSeq construct_arglist(const std::vector<InstSeq>& operand_codes) {
        //        DEBUG_OUTPUT("operand");
        //        for(const auto& operand: operand_codes) {
        //            DEBUG_OUTPUT("1.");
        //            CodeListPrinter(operand).print();
        //        }
        //        DEBUG_OUTPUT("---");
        if (operand_codes.empty()) {
            return make_instruction_sequence({}, { Register::ARGL }, { Instruction::ASSIGN, Register::ARGL, nil });
        }
        auto code0 = CodeList{ Instruction::ASSIGN, Register::ARGL, Intern::op_list, Register::VAL };
        auto seq0 = make_instruction_sequence({ Register::VAL }, { Register::ARGL }, code0);
        auto code_to_get_last_arg = append_instruction_sequences(operand_codes.back(), seq0);
        auto it = operand_codes.rbegin();
        it++;
        while (it != operand_codes.rend()) {
            auto code1 =
                CodeList{ Instruction::ASSIGN, Register::ARGL, Intern::op_cons, Register::VAL, Register::ARGL };
            auto seq1 = make_instruction_sequence({ Register::VAL, Register::ARGL }, { Register::ARGL }, code1);
            auto code_for_next_arg = preserving({ Register::ARGL }, *it, seq1);
            it++;
            code_to_get_last_arg = preserving({ Register::ENV }, code_to_get_last_arg, code_for_next_arg);
        }
        return code_to_get_last_arg;
    }

    InstSeq empty_instruction_sequence() {
        return {};
    }

    InstSeq make_instruction_sequence(Regs needs, Regs modifies, CodeList statements) {
        return { std::move(needs), std::move(modifies), std::move(statements) };
    }

    bool is_needs_register(const InstSeq& seq, Register reg) {
        std::vector<int> a;
        auto it = std::find(seq.needs.begin(), seq.needs.end(), reg);
        return it != seq.needs.end();
    }

    bool is_modifies_register(const InstSeq& seq, Register reg) {
        auto it = std::find(seq.modifies.begin(), seq.modifies.end(), reg);
        return it != seq.modifies.end();
    }

    InstSeq compile_linkage(Linkage linkage) {
        if (is_type<LinkageEnum>(linkage)) {
            auto link = get<LinkageEnum>(linkage);
            if (link == LinkageEnum::RETURN) {
                auto code = CodeList{ Instruction::GOTO, Register::CONTINUE };
                return make_instruction_sequence({ Register::CONTINUE }, {}, code);
            }
            else if (link == LinkageEnum::NEXT) {
                return empty_instruction_sequence();
            }
            else {
                DEBUG_OUTPUT("Linkage:", link);
                throw std::runtime_error("unknown linkage");
            }
        }
        else {
            auto label = get<Label>(linkage);
            return make_instruction_sequence({}, {}, { Instruction::GOTO, label });
        }
    }

    InstSeq end_with_linkage(const Linkage& linkage, const InstSeq& inst_seq) {
        return preserving({ Register::CONTINUE }, inst_seq, compile_linkage(linkage));
    }

    /**
     * if seq1 modifies the register and seq2 actually needs the register's original contents,
     * then `preserving` wraps a `save` and a `restore` of the register
     * around the seq1 before append the sequences.
     * Otherwise, `preserving` simply returns the appended instruction sequences.
     * @param regs a set of registers
     * @param seq1 the first sequence
     * @param seq2 the second sequence
     * @return
     */
    InstSeq preserving(const Regs& regs, const InstSeq& seq1, const InstSeq& seq2) {
        if (regs.empty()) {
            return append_instruction_sequences(seq1, seq2);
        }
        CodeList statements = seq1.statements;
        Regs needs = seq1.needs;
        Regs modifies = seq1.modifies;
        for (auto reg : regs) {
            if (is_needs_register(seq2, reg) && is_modifies_register(seq1, reg)) {
                needs = list_union(Regs{ reg }, needs);
                modifies = list_difference(modifies, Regs{ reg });
                statements.push_front(Instruction::SAVE, reg);
                statements.push_back(Instruction::RESTORE, reg);
            }
        }
        auto new_seq = make_instruction_sequence(needs, modifies, statements);
        return append_instruction_sequences(new_seq, seq2);
    }

    InstSeq parallel_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2) {
        auto needs = list_union(seq1.needs, seq2.needs);
        auto modifies = list_union(seq1.modifies, seq2.modifies);
        auto statements = append(seq1.statements, seq2.statements);
        return make_instruction_sequence(needs, modifies, statements);
    }

    template <typename T>
    T list_union(T s1, T s2) {
        T ret;
        std::set_union(s1.begin(), s1.end(), s2.begin(), s2.end(), std::back_inserter(ret));
        return ret;
    }

    template <typename T>
    T list_difference(T s1, T s2) {
        T ret;
        std::set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(), std::inserter(ret, ret.begin()));
        return ret;
    }

    CodeList append(const CodeList& a, const CodeList& b) {
        if (a.empty()) {
            return b;
        }
        if (b.empty()) {
            return a;
        }
        return CodeList().merge(a).merge(b);
    }

    InstSeq append_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2) {
        auto needs = list_union(seq1.needs, list_difference(seq2.needs, seq1.modifies));
        auto modifies = list_union(seq1.modifies, seq2.modifies);
        return make_instruction_sequence(needs, modifies, append(seq1.statements, seq2.statements));
    }

    template <typename... Args>
    InstSeq append_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2, Args&&...args) {
        auto seq = append_instruction_sequences(seq1, seq2);
        return append_instruction_sequences(seq, std::forward<Args>(args)...);
    }

    InstSeq compile_intern(Intern op, Cell expr, Target target, Linkage linkage) {
        InstSeq seq;
        switch (get<Intern>(op)) {
        case Intern::_quote: {
            seq = compile_quoted(expr, target, linkage);
            break;
        }
        case Intern::_unquote: {
            seq = compile(cdr(expr), target, linkage);
            break;
        }
        case Intern::_setb: {
            seq = compile_assignment(expr, target, linkage);
            break;
        }
        case Intern::_define: {
            seq = compile_definition(expr, target, linkage);
            break;
        }
        case Intern::_macro: {
            seq = compile_definition(expr, target, linkage, true);
            break;
        }
        case Intern::_define_syntax: {
            scm.syntax_define_syntax(cur_env(), cdr(expr));
            seq = {};
            break;
        }
        case Intern::_if: {
            seq = compile_if(expr, target, linkage);
            break;
        }
        case Intern::_lambda: {
            seq = compile_lambda(expr, target, linkage).second;
            break;
        }
        case Intern::_begin: {
            seq = compile_sequence(cdr(expr), target, linkage);
            break;
        }
        case Intern::_cond: {
            auto new_if = cond_to_if(expr);
            // DEBUG_OUTPUT("cond->if:", new_if);
            seq = compile(new_if, target, linkage);
            break;
        }
        case Intern::_and: {
            auto new_if = and_to_if(expr);
            // DEBUG_OUTPUT("and->if:", new_if);
            seq = compile(new_if, target, linkage);
            break;
        }
        case Intern::_or: {
            auto new_if = or_to_if(expr);
            // DEBUG_OUTPUT("or->if:", new_if);
            seq = compile(new_if, target, linkage);
            break;
        }
        case Intern::_apply: {
            seq = compile_apply(expr, target, linkage);
            break;
        }
        case Intern::_values: {
            seq = compile_values(expr, target, linkage);
            break;
        }
        case Intern::_module: {
            seq = compile_module(expr, target, linkage);
            break;
        }
        case Intern::_use_module: {
            seq = compile_use_module(expr, target, linkage);
            break;
        }
        case Intern::_inherit_module: {
            seq = compile_inherit_module(expr, target, linkage);
            break;
        }
        case Intern::_with_module: {
            seq = compile_with_module(expr, target, linkage);
            break;
        }
        case Intern::_expand: {
            seq = compile_expand(expr, target, linkage);
            break;
        }
        default: {
            seq = compile_application(expr, target, linkage);
        }
        }
        return seq;
    }

    InstSeq compile(Cell expr, Target target, Linkage linkage) {
        // DEBUG_OUTPUT("compile expr:", expr);
        InstSeq seq;
        if (is_self_evaluating(expr)) {
            seq = compile_self_evaluating(expr, target, linkage);
        }
        else if (is_variable(expr)) {
            seq = compile_variable(expr, target, linkage);
        }
        else {
            auto op = eval_op(car(expr));
            if (is_intern(op)) {
                seq = compile_intern(get<Intern>(op), expr, target, linkage);
            }
            else if (is_pair(expr)) {
                seq = compile_application(expr, target, linkage);
            }
            else {
                DEBUG_OUTPUT("Unknown expression type: COMPILE", expr);
                throw std::runtime_error("compile fail");
            }
        }
        return seq;
    }

    Int new_label_number() {
        return ++label_counter;
    }

    Label make_label(LabelEnum label) {
        return { label, new_label_number() };
    }

    SymenvPtr cur_env() {
        return env_stack.top();
    }

    void push_env(SymenvPtr env) {
        env_stack.push(env);
    }

    void pop_env() {
        env_stack.pop();
    }

    CodeList code_list;
    CellHashMap<Int> label_map;
    Scheme& scm;
    std::stack<SymenvPtr> env_stack;
    Machine m;
    Cell ok = true;
    static Int label_counter;
};

Int CompilerImpl::label_counter = 0;

Compiler::Compiler(Scheme& scm, const SymenvPtr& env)
    : impl(std::make_shared<CompilerImpl>(scm, env)) {
}

CompiledCode Compiler::compile(const Cell& cell) {
    // DEBUG_OUTPUT("compile code:", cell);
    auto seq = impl->compile(cell, Register::VAL, LinkageEnum::RETURN);
    return { seq };
}

std::pair<Label, InstSeq> Compiler::compile_lambda(const Cell& cell, bool is_macro) {
    return impl->compile_lambda(cell, Register::VAL, LinkageEnum::RETURN, is_macro);
}

CompiledCode::CompiledCode(const InstSeq& seq) {
    code = std::make_shared<InstSeq>(seq);
}

CompiledCode::CompiledCode() {
    code = std::make_shared<InstSeq>();
}
} // namespace pscm
