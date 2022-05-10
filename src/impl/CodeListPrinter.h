//
// Created by PikachuHy on 2022/5/10.
//

#ifndef PICOSCHEME_CODELISTPRINTER_H
#define PICOSCHEME_CODELISTPRINTER_H
#include "picoscm/compiler.h"

namespace pscm {

class CodeListPrinter {
public:
    CodeListPrinter(const sptr<InstSeq>& seq)
        : code_list(seq->statements) {
    }

    CodeListPrinter(const InstSeq& seq)
        : code_list(seq.statements) {
    }

    void print() {
        DEBUG_OUTPUT("print code list, size: ", code_list.size());
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

    void print_args(int n) {
        while (n > 0) {
            i++;
            n--;
            std::wcout << " ";
            print_code(code_list[i]);
        }
    }

    void print_op();

    void print_code(const InstCode& code) {
        std::wcout << code;
    }

    void print_assign();

    void print_inst();

    void print_code_list();

private:
    const CodeList& code_list;
    int i;
};

} // namespace pscm
#endif // PICOSCHEME_CODELISTPRINTER_H
