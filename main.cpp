//
// Created by PikachuHy on 2022/4/5.
//
#include <fstream>
#include <picoscm/pscm>
using namespace pscm;
int main(int argc, char* argv[]) {
    std::ofstream ofstream;
    ofstream.open("inst.log", std::ios::out);
    ofstream.close();
    ofstream.open("run.log", std::ios::out);
    ofstream.close();
    Scheme scm;
    //    scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/init.scm");
    //    scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/eval.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/g.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/f.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/e.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/d.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/a.scm");
    scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/b.scm");
    //     scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/1.scm");
    //    scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/0.scm");
    //      scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/test/r4rstest.scm");
    //  scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/test/r5rs-tests.scm");
    // scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/test/call_cc-test.scm");

    if (argc > 1) {
        scm.load(argv[1]);
    }
    else {
        scm.repl();
    }
    return 0;
}