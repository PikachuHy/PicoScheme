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
#include "magic_enum.hpp"
#include "picoscm/port.hpp"
#include "picoscm/scheme.hpp"
#include <algorithm>
#include <ostream>

namespace pscm {
const char *compiler_error::what() const noexcept {
    return reason.c_str();
}

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

bool is_next_linkage(Linkage linkage) {
    return is_type<LinkageEnum>(linkage) && get<LinkageEnum>(linkage) == LinkageEnum::NEXT;
}

bool is_return_linkage(Linkage linkage) {
    return is_type<LinkageEnum>(linkage) && get<LinkageEnum>(linkage) == LinkageEnum::RETURN;
}

template <typename OStream>
void print_inst(OStream& os, Instruction inst) {
    auto s = string_convert<Char>(std::string(magic_enum::enum_name(inst)));
    os << s;
    int fix_len = 4;
    for (int i = s.size(); i < fix_len; ++i) {
        os << " ";
    }
}

template <typename OStream>
OStream& operator<<(OStream& os, Instruction inst) {
    print_inst(os, inst);
    return os;
}

template <typename OStream>
OStream& operator<<(OStream& os, LinkageEnum link) {
    os << string_convert<Char>(std::string(magic_enum::enum_name(link)));
    return os;
}

template <typename OStream>
void print_register(OStream& os, Register reg) {
    switch (reg) {
    case Register::VAL:
        os << "val";
        break;
    case Register::EXP:
        os << "exp";
        break;
    case Register::FUN:
        os << "fun";
        break;
    case Register::PROC:
        os << "proc";
        break;
    case Register::ENV:
        os << "env";
        break;
    case Register::CONTINUE:
        os << "continue";
        break;
    case Register::ARGL:
        os << "argl";
        break;
    }
}

template <typename OStream>
void print_label(OStream& os, const Label& label) {
    switch (label.type) {
    case LabelEnum::TRUE_BRANCH:
        os << "true-branch";
        break;
    case LabelEnum::FALSE_BRANCH:
        os << "false-branch";
        break;
    case LabelEnum::AFTER_IF:
        os << "after-if";
        break;
    case LabelEnum::PROC_RETURN:
        os << "proc-return";
        break;
    case LabelEnum::PRIMITIVE_BRANCH:
        os << "primitive-branch";
        break;
    case LabelEnum::COMPILED_BRANCH:
        os << "compiled-branch";
        break;
    case LabelEnum::AFTER_CALL:
        os << "after-call";
        break;
    case LabelEnum::ENTRY:
        os << "entry";
        break;
    case LabelEnum::AFTER_LAMBDA:
        os << "after-lambda";
        break;
    }
    os << label.num;
}

template <typename OStream>
OStream& operator<<(OStream& os, const Label& label) {
    print_label(os, label);
    return os;
}

template <typename OStream>
void print_operand(OStream& os, const Operand& operand) {
    std::visit(overloads{ [&os](Register reg) {
                             print_register(os, reg);
                         },
                          [&os](const Cell& cell) {
                              os << cell;
                          },
                          [&os](const Label& label) {
                              print_label(os, label);
                          }

               },
               operand);
}

template <typename OStream>
OStream& operator<<(OStream& os, const Operand& operand) {
    print_operand(os, operand);
    return os;
}

template <typename OStream>
OStream& operator<<(OStream& os, const Register& reg) {
    print_register(os, reg);
    return os;
}

template <typename OStream>
void print_inst_code(OStream& os, const InstCode& code) {
    std::visit(overloads{ [&os](const Instruction& arg) {
                             print_inst(os, arg);
                         },
                          [&os](const Operand& arg) {
                              print_operand(os, arg);
                          },
                          [&os](const Label& arg) {
                              print_label(os, arg);
                          }

               },
               code);
}

template <typename OStream>
OStream& operator<<(OStream& os, const InstCode& code) {
    print_inst_code(os, code);
    return os;
}

compiler_error::compiler_error(const std::string& msg, Cell cell) {
    using Port = StringPort<Char>;
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

struct CompiledProcedureImpl {
    CompiledProcedureImpl(Machine& m, Label label, SymenvPtr senv)
        : m(m)
        , label(label)
        , senv(senv) {
    }

    Machine& m;
    Label label;
    SymenvPtr senv;

    bool operator==(const CompiledProcedureImpl& rhs) const {
        return label == rhs.label && senv == rhs.senv;
    }

    bool operator!=(const CompiledProcedureImpl& rhs) const {
        return !(rhs == *this);
    }

    Int entry() const;
};

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

class CodeListPrinter {
public:
    CodeListPrinter(const CodeList& code_list)
        : code_list(code_list) {
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

    void print_code(InstCode code) {
        print_inst_code(std::wcout, code);
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

struct CompilerPrivate {
    CompilerPrivate(Scheme& scm)
        : scm(scm)
        , m(scm) {
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

    Cell cond_to_if(const Cell& expr) {
        return none;
    }

    bool is_self_evaluating(const Cell& expr) {
        if (is_number(expr)) {
            return true;
        }
        if (is_string(expr)) {
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

    bool is_quoted(const Cell& expr) {
        return is_tagged_list(expr, "quote");
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

    bool is_assignment(const Cell& expr) {
        return is_tagged_list(expr, "set!");
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

    bool is_definition(const Cell& expr) {
        return is_tagged_list(expr, "define");
    }

    Cell definition_variable(const Cell& expr) {
        if (is_symbol(cadr(expr))) {
            return cadr(expr);
        }
        else {
            return caadr(expr);
        }
    }

    Cell make_lambda(const Cell& parameters, Cell body) {
        return scm.cons(scm.symbol("lambda"), scm.cons(parameters, body));
    }

    Cell definition_value(const Cell& expr) {
        if (is_symbol(cadr(expr))) {
            return caddr(expr);
        }
        else {
            return make_lambda(cdadr(expr), cddr(expr));
        }
    }

    InstSeq compile_definition(const Cell& expr, Target target, Linkage linkage) {
        auto var = definition_variable(expr);
        auto get_value_code = compile(definition_value(expr), Register::VAL, LinkageEnum::NEXT);
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

    bool is_if(const Cell& expr) {
        return is_tagged_list(expr, "if");
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
            consequent_linkage = LinkageEnum::NEXT;
        }
        auto p_code = compile(if_predicate(expr), Register::VAL, LinkageEnum::NEXT);
        auto c_code = compile(if_consequent(expr), target, consequent_linkage);
        auto a_code = compile(if_alternative(expr), target, linkage);

        auto code0 = CodeList{ Instruction::TEST, Intern::op_is_false, Register::VAL,

                               Instruction::BRANCH, f_branch };
        auto seq0 = make_instruction_sequence({ Register::VAL }, {}, code0);
        InstSeq seq2 =
            parallel_instruction_sequences(append_instruction_sequences(InstSeq(Instruction::LABEL, t_branch), c_code),
                                           append_instruction_sequences(InstSeq(Instruction::LABEL, f_branch), a_code));
        auto seq3 = append_instruction_sequences(seq0, seq2, InstSeq(Instruction::LABEL, after_if));
        return preserving({ Register::ENV, Register::CONTINUE }, p_code, seq3);
    }

    bool is_lambda(const Cell& expr) {
        return is_tagged_list(expr, "lambda");
    }

    InstSeq tack_on_instruction_sequence(const InstSeq& seq, const InstSeq& body_seq) {
        return make_instruction_sequence(seq.needs, seq.modifies, append(seq.statements, body_seq.statements));
    }

    InstSeq compile_lambda(Cell expr, Target target, const Linkage& linkage) {
        auto proc_entry = make_label(LabelEnum::ENTRY);
        auto after_lambda = make_label(LabelEnum::AFTER_LAMBDA);
        auto lambda_linkage = linkage;
        if (is_next_linkage(linkage)) {
            lambda_linkage = after_lambda;
        }
        auto code0 =
            CodeList{ Instruction::ASSIGN, target, Intern::op_make_compiled_procedure, proc_entry, Register::ENV };
        auto seq1 = make_instruction_sequence({ Register::ENV }, { target }, code0);
        auto seq2 = end_with_linkage(lambda_linkage, seq1);
        auto seq3 = tack_on_instruction_sequence(seq2, compile_lambda_body(expr, proc_entry));
        return append_instruction_sequences(seq3, InstSeq(Instruction::LABEL, after_lambda));
    }

    Cell lambda_parameters(Cell expr) {
        return cadr(expr);
    }

    Cell lambda_body(const Cell& expr) {
        return cddr(expr);
    }

    InstSeq compile_lambda_body(const Cell& expr, const Label& proc_entry) {
        auto formals = lambda_parameters(expr);
        CodeList inst_seq0 = { Instruction::LABEL,  proc_entry,

                               Instruction::ASSIGN, Register::ENV, Intern::op_compiled_procedure_env, Register::PROC,

                               Instruction::ASSIGN, Register::ENV, Intern::op_extend_environment,     formals,
                               Register::ARGL,      Register::ENV };
        auto inst_seq1 =
            make_instruction_sequence({ Register::ENV, Register::PROC, Register::ARGL }, { Register::ENV }, inst_seq0);
        auto inst_seq2 = compile_sequence(lambda_body(expr), Register::VAL, LinkageEnum::RETURN);
        return append_instruction_sequences(inst_seq1, inst_seq2);
    }

    bool is_begin(const Cell& expr) {
        return is_tagged_list(expr, "began");
    }

    bool is_cond(const Cell& expr) {
        return is_tagged_list(expr, "cond");
    }

    bool is_application(const Cell& expr) {
        return is_pair(expr);
    }

    InstSeq compile_application(Cell expr, Target target, Linkage linkage) {
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
            append_instruction_sequences(InstSeq(Instruction::LABEL, compiled_branch), seq1),
            append_instruction_sequences(InstSeq(Instruction::LABEL, primitive_branch), seq3));
        return append_instruction_sequences(seq0, seq4, InstSeq(Instruction::LABEL, after_call));
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
        if (operand_codes.empty()) {
            return make_instruction_sequence({}, { Register::ARGL }, { Instruction::ASSIGN, Register::ARGL, nil });
        }
        auto code0 = CodeList{ Instruction::ASSIGN, Register::ARGL, Intern::op_list, Register::VAL };
        auto seq0 = make_instruction_sequence({ Register::VAL }, { Register::ARGL }, code0);
        auto code_to_get_last_arg = append_instruction_sequences(operand_codes.back(), seq0);
        int i = operand_codes.size() - 2;
        auto code1 = CodeList{ Instruction::ASSIGN, Register::ARGL, Intern::op_cons, Register::VAL, Register::ARGL };
        auto seq1 = make_instruction_sequence({ Register::VAL, Register::ARGL }, { Register::ARGL }, code1);
        while (i >= 0) {
            auto code_for_next_arg = preserving({ Register::ARGL }, operand_codes[i], seq1);
            i--;
            code_to_get_last_arg = preserving({ Register::ENV }, code_to_get_last_arg, code_for_next_arg);
        }
        return code_to_get_last_arg;
    }

    InstSeq empty_instruction_sequence() {
        return {};
    }

    InstSeq make_instruction_sequence(Regs needs, Regs modifies, std::vector<InstCode> statements) {
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

    InstSeq preserving(const Regs& regs, const InstSeq& seq1, const InstSeq& seq2) {
        if (regs.empty()) {
            return append_instruction_sequences(seq1, seq2);
        }
        CodeList pre;
        CodeList post;
        CodeList statements;
        Regs needs = seq1.needs;
        Regs modifies = seq1.modifies;
        for (auto reg : regs) {
            if (is_needs_register(seq2, reg) && is_modifies_register(seq1, reg)) {
                needs = list_union(Regs{ reg }, needs);
                modifies = list_difference(modifies, Regs{ reg });
                pre.push_back(reg);
                pre.push_back(Instruction::SAVE);
                post.push_back(Instruction::RESTORE);
                post.push_back(reg);
            }
        }
        statements.reserve(pre.size() + statements.size() + post.size());
        std::copy(pre.rbegin(), pre.rend(), std::back_inserter(statements));
        std::copy(seq1.statements.begin(), seq1.statements.end(), std::back_inserter(statements));
        std::copy(post.begin(), post.end(), std::back_inserter(statements));
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
        CodeList ret;
        ret.reserve(a.size() + b.size());
        std::copy(a.begin(), a.end(), std::back_inserter(ret));
        std::copy(b.begin(), b.end(), std::back_inserter(ret));
        return ret;
    }

    InstSeq append_instruction_sequences(Label label, const InstSeq& seq) {
        auto seq1 = make_instruction_sequence({}, {}, { label });
        return append_instruction_sequences(seq1, seq);
    }

    InstSeq append_instruction_sequences(const InstSeq& seq, Label label) {
        auto seq2 = make_instruction_sequence({}, {}, { label });
        return append_instruction_sequences(seq, seq2);
    }

    InstSeq append_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2) {
        auto needs = list_union(seq1.needs, list_difference(seq2.needs, seq1.modifies));
        auto modifies = list_union(seq1.modifies, seq2.modifies);
        return make_instruction_sequence(needs, modifies, append(seq1.statements, seq2.statements));
    }

    InstSeq append_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2, const InstSeq& seq3) {
        auto seq = append_instruction_sequences(seq1, seq2);
        seq = append_instruction_sequences(seq, seq3);
        return seq;
    }

    InstSeq append_instruction_sequences(const InstSeq& seq1, const InstSeq& seq2, const Label& label) {
        auto seq = append_instruction_sequences(seq1, seq2);
        seq.statements.push_back(label);
        return seq;
    }

    Cell begin_actions(Cell expr) {
        return cdr(expr);
    }

    InstSeq compile(Cell expr, Target target, Linkage linkage) {
        if (is_self_evaluating(expr)) {
            return compile_self_evaluating(expr, target, linkage);
        }
        else if (is_quoted(expr)) {
            return compile_quoted(expr, target, linkage);
        }
        else if (is_variable(expr)) {
            return compile_variable(expr, target, linkage);
        }
        else if (is_assignment(expr)) {
            return compile_assignment(expr, target, linkage);
        }
        else if (is_definition(expr)) {
            return compile_definition(expr, target, linkage);
        }
        else if (is_if(expr)) {
            return compile_if(expr, target, linkage);
        }
        else if (is_lambda(expr)) {
            return compile_lambda(expr, target, linkage);
        }
        else if (is_begin(expr)) {
            return compile_sequence(begin_actions(expr), target, linkage);
        }
        else if (is_cond(expr)) {
            return compile(cond_to_if(expr), target, linkage);
        }
        else if (is_application(expr)) {
            return compile_application(expr, target, linkage);
        }
        else {
            DEBUG_OUTPUT("Unknown expression type: COMPILE", expr);
            throw std::runtime_error("compile fail");
        }
    }

    Int new_label_number() {
        return ++label_counter;
    }

    Label make_label(LabelEnum label) {
        return { label, new_label_number() };
    }

    CodeList code_list;
    CellHashMap<Int> label_map;
    Scheme& scm;
    Machine m;
    Cell ok = true;
    Int label_counter = 0;
};

Compiler::Compiler(Scheme& scm)
    : c(std::make_shared<CompilerPrivate>(scm)) {
}
class Machine;

class CodeRunner {
public:
    CodeRunner(Machine& m, const CodeList& code_list)
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
    Machine& m;
    const CodeList& code_list;
    int i;
};

void Machine::run(const CodeList& code_list, const SymenvPtr& env) {
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
}

void Machine::print_reg() const {
    DEBUG_OUTPUT("machine registers:");
    for (const auto& [k, v] : reg) {
        std::wcout << k << " --> " << v << std::endl;
    }
}

void Machine::fill_label_map(const CodeList& code_list) {
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
                    DEBUG_OUTPUT("inst:");
                    print_inst_code(std::wcout, code);
                    std::wcout << std::endl;
                    throw std::runtime_error("inst error");
                }
            }
        }
        i++;
    }
}

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
                print_inst_code(std::wcout, code_list[j]);
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
            print_operand(std::wcout, v);
            DEBUG_OUTPUT("error:");
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

bool is_eof(Cell cell) {
    Cell eof = -1;
    return cell == eof;
}

CompiledCode Compiler::compile(Scheme& scm, const SymenvPtr& env, Cell cell) {
    DEBUG_OUTPUT("code:", cell);
    if (is_eof(cell)) {
        return {};
    }
    auto seq = c->compile(cell, Register::VAL, LinkageEnum::NEXT);
    auto p = CodeListPrinter(seq.statements);
    p.print();
    c->m.run(seq.statements, env);
    return {};
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
} // namespace pscm