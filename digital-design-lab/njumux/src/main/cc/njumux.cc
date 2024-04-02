#include <nvboard.h>
#include <Vnjumux.h>
#include <verilated.h>
#include "njumux_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vnjumux* njumux = new Vnjumux(contextp);

  nvboard_bind_all_pins(njumux);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    njumux->eval();
  }

  njumux->final();
  delete njumux;
  
  return 0;
}
