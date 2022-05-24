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
        max_width = num_width(code_list.size()) + 2;
    }

    CodeListPrinter(const InstSeq& seq, bool print_to_console = false)
        : code_list(seq.statements)
        , print_to_console(print_to_console)
        , start_pos(0) {
        init_stream();
        max_width = num_width(code_list.size()) + 2;
    }

    CodeListPrinter(const CodeList& code_list, std::size_t pos)
        : code_list(code_list)
        , start_pos(pos) {
        init_stream();
        max_width = num_width(pos + code_list.size()) + 2;
    }

    ~CodeListPrinter() {
        if (print_to_console) {
        }
        else {
            stream.close();
        }
    }

    void init_stream() {
        if (print_to_console) {
        }
        else {
            stream.open("inst.log", std::ios::out | std::ios::app);
        }
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
        flush();
    }

private:
    void reset() {
        i = 0;
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
        auto s = std::to_wstring(i + start_pos);
        print(s);
        for (int j = s.size(); j < max_width; ++j) {
            print(" ");
        }
    }

    void print_op();

    void print_code(const InstCode& code) {
        //        std::wstringstream ss;
        //        ss << code;
        print(code);
    }

    void print_assign();

    void print_inst();

    void print_code_list();

    template <class T>
    void print(const T& t) {
        if (print_to_console) {
            std::wcout << t;
        }
        else {
            stream << t;
        }
    }

    void print_endl() {
        print("\n");
        flush();
    }

    void flush() {
        if (print_to_console) {
            std::wcout.flush();
        }
        else {
            stream.flush();
        }
    }

private:
    const CodeList& code_list;
    std::size_t i;
    std::size_t start_pos;
    int max_width;
    std::wofstream stream;
    bool print_to_console = false;
};

} // namespace pscm
#endif // PICOSCHEME_CODELISTPRINTER_H
