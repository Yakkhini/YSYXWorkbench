#include <nvboard.h>
#include <Vagkbd.h>
#include <verilated.h>
#include "agkbd_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vagkbd* top = new Vagkbd(contextp);

  nvboard_bind_all_pins(top);
  nvboard_init();
  while (!contextp->gotFinish()) {
    top->clk = 1; top->eval();
    top->clk = 0; top->eval();
    nvboard_update();
  }

  top->final();
  delete top;
  
  return 0;
}
