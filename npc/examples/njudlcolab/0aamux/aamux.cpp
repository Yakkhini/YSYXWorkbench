#include <nvboard.h>
#include <Vaamux.h>
#include <verilated.h>
#include "aamux_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vaamux* aamux = new Vaamux(contextp);

  nvboard_bind_all_pins(aamux);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    aamux->eval();
  }

  aamux->final();
  delete aamux;
  
  return 0;
}
