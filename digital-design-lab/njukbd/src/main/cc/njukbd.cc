#include <nvboard.h>
#include <Vnjukbd.h>
#include <verilated.h>
#include "njukbd_bind.h"

static void single_cycle(Vnjukbd* top) {
  top->clock = 0; top->eval();
  top->clock = 1; top->eval();
}

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vnjukbd* njukbd = new Vnjukbd(contextp);

  nvboard_bind_all_pins(njukbd);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    single_cycle(njukbd);
    njukbd->eval();
  }

  njukbd->final();
  delete njukbd;
  
  return 0;
}
