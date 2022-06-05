/********************************************************************************/
/**
 * @file label.h
 *
 * @version   0.1
 * @date      2018-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_LABEL_H
#define PICOSCHEME_LABEL_H
#include "number.hpp"

namespace pscm {

enum class LabelEnum {
    TRUE_BRANCH,
    FALSE_BRANCH,
    AFTER_IF,
    PROC_RETURN,
    PRIMITIVE_BRANCH,
    COMPILED_BRANCH,
    AFTER_CALL,
    ENTRY,
    AFTER_LAMBDA,
    APPLY_ENTRY,
    APPLY_CALL,
    AFTER_APPLY
};

struct Label {
    LabelEnum type;
    Int num;

    bool operator==(const Label& rhs) const {
        return type == rhs.type && num == rhs.num;
    }

    bool operator!=(const Label& rhs) const {
        return !(rhs == *this);
    }

    struct hash {
        using argument_type = Label;
        using result_type = std::size_t;

        result_type operator()(const Label& label) const noexcept {
            return std::hash<Int>{}(label.num);
        }
    };
};

template <typename CharT>
std::basic_ostream<CharT>& operator<<(std::basic_ostream<CharT>& os, const Label& label) {
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
    case LabelEnum::APPLY_ENTRY:
        os << "apply-entry";
        break;
    case LabelEnum::APPLY_CALL:
        os << "apply-call";
        break;
    case LabelEnum::AFTER_APPLY:
        os << "after-apply";
        break;
    }
    os << label.num;
    return os;
}
} // namespace pscm
#endif // PICOSCHEME_LABEL_H
