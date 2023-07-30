#include <nvboard.h>
#include <Vabcoder.h>
#include <verilated.h>
#include "abcoder_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vabcoder* abcoder = new Vabcoder(contextp);

  nvboard_bind_all_pins(abcoder);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    abcoder->eval();
  }

  abcoder->final();
  delete abcoder;
  
  return 0;
}
