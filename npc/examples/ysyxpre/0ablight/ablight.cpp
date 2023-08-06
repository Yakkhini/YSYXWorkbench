#include <nvboard.h>
#include <Vablight.h>
#include <verilated.h>
#include "ablight_bind.h"

void single_cycle(Vablight* top) {
  top->clk = 0; top->eval();
  top->clk = 1; top->eval();
}

void reset(Vablight* top, int n) {
  top->rst = 1;
  while (n -- > 0) single_cycle(top);
  top->rst = 0;
}

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vablight* top = new Vablight(contextp);

  nvboard_bind_all_pins(top);
  nvboard_init();
  reset(top, 10);
  while (!contextp->gotFinish()) {
    nvboard_update();
    single_cycle(top);
  }

  top->final();
  delete top;
  
  return 0;
}
