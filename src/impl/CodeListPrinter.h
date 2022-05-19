//
// Created by PikachuHy on 2022/5/10.
//

#ifndef PICOSCHEME_CODELISTPRINTER_H
#define PICOSCHEME_CODELISTPRINTER_H
#include "picoscm/compiler.h"
#include <fstream>

namespace pscm {

class CodeListPrinter {
public:
    CodeListPrinter(const sptr<InstSeq>& seq)
        : code_list(seq->statements) {
        init_stream();
    }

    CodeListPrinter(const InstSeq& seq)
        : code_list(seq.statements) {
        init_stream();
    }

    CodeListPrinter(const CodeList& code_list, int pos)
        : code_list(code_list)
        , start_pos(pos) {
        i = start_pos;
        init_stream();
        max_width = num_width(pos + code_list.size()) + 2;
    }

    ~CodeListPrinter() {
        stream.close();
    }

    void init_stream() {
        stream.open("inst.log", std::ios::out | std::ios::app);
    }

    void print() {
        //        DEBUG_OUTPUT("print code list, size: ", code_list.size());
        reset();
        /*
        for(auto code: code_list) {
            print_code(code);
            std::wcout << std::endl;
        }
        DEBUG_OUTPUT("print format");
         */
        print_code_list();
        stream.flush();
    }

private:
    void reset() {
        i = start_pos + 1;
    }

    void print_args(int n) {
        while (n > 0) {
            i++;
            n--;
            print(" ");
            print_code(code_list[i]);
        }
    }

    int num_width(int n) {
        return std::to_string(n).size();
    }

    void print_pos() {
        auto s = std::to_wstring(i);
        print(s);
        for (int j = s.size(); j < max_width; ++j) {
            print(" ");
        }
    }

    void print_op();

    void print_code(const InstCode& code) {
        //        std::wstringstream ss;
        //        ss << code;
        stream << code;
    }

    void print_assign();

    void print_inst();

    void print_code_list();

    template <class T>
    void print(const T& t) {
        stream << t;
    }

    void print_endl() {
        stream << "\n";
        stream.flush();
    }

private:
    const CodeList& code_list;
    int i;
    int start_pos;
    int max_width;
    std::wofstream stream;
};

} // namespace pscm
#endif // PICOSCHEME_CODELISTPRINTER_H
