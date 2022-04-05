//
// Created by PikachuHy on 2022/4/5.
//
#include <pscm>
using namespace pscm;
int main(int argc, char* argv[]) {
  Scheme scm;
  if (argc > 1) {
    scm.load(argv[1]);
  } else {
    scm.repl();
  }
  return 0;
}