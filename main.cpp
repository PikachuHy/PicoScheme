//
// Created by PikachuHy on 2022/4/5.
//
#include <picoscm/pscm>
using namespace pscm;
int main(int argc, char* argv[]) {
    Scheme scm;
    //   scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/init.scm");
    // scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/a.scm");
    // scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/b.scm");
    // scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/1.scm");
    scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/cmake-build-debug/0.scm");
    //  scm.load("/Users/pikachu/texmacs_dev/pr/PicoScheme/test/r4rstest.scm");
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