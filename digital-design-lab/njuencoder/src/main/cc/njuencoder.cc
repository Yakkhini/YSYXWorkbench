#include <nvboard.h>
#include <Vnjuencoder.h>
#include <verilated.h>
#include "njuencoder_bind.h"

int main(int argc, char**argv) {
  VerilatedContext* contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vnjuencoder* njuencoder = new Vnjuencoder(contextp);

  nvboard_bind_all_pins(njuencoder);
  nvboard_init();
  while (!contextp->gotFinish()) {
    nvboard_update();
    njuencoder->eval();
  }

  njuencoder->final();
  delete njuencoder;
  
  return 0;
}
