#include <nvboard.h>
#include <Vafsftreg.h>
#include <verilated.h>
#include "afsftreg_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vafsftreg* afsftreg = new Vafsftreg(contextp);

  nvboard_bind_all_pins(afsftreg);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    afsftreg->eval();
  }

  afsftreg->final();
  delete afsftreg;
  
  return 0;
}
