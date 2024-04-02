#include <nvboard.h>
#include <Vysyxprelight.h>
#include <unistd.h>
#include <verilated.h>
#include "ysyxprelight_bind.h"

void single_cycle(Vysyxprelight* top) {
  top->clock = 0; top->eval();
  top->clock = 1; top->eval();
}

void reset(Vysyxprelight* top, int n) {
  top->reset = 1;
  while (n -- > 0) single_cycle(top);
  top->reset = 0;
}

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vysyxprelight* top = new Vysyxprelight(contextp);

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
