#include <nvboard.h>
#include <Vnjualu.h>
#include <verilated.h>
#include "njualu_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vnjualu* njualu = new Vnjualu(contextp);

  nvboard_bind_all_pins(njualu);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    njualu->eval();
  }

  njualu->final();
  delete njualu;
  
  return 0;
}
