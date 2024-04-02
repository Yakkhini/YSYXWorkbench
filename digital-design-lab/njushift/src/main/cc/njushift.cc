#include <nvboard.h>
#include <Vnjushift.h>
#include <verilated.h>
#include "njushift_bind.h"

static void single_cycle(Vnjushift* top) {
  top->clock = 0; top->eval();
  top->clock = 1; top->eval();
}

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vnjushift* njushift = new Vnjushift(contextp);

  nvboard_bind_all_pins(njushift);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    single_cycle(njushift);
    njushift->eval();
  }

  njushift->final();
  delete njushift;
  
  return 0;
}
