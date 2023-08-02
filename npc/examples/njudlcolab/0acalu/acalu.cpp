#include <nvboard.h>
#include <Vacalu.h>
#include <verilated.h>
#include "acalu_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vacalu* acalu = new Vacalu(contextp);

  nvboard_bind_all_pins(acalu);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    acalu->eval();
  }

  acalu->final();
  delete acalu;
  
  return 0;
}
