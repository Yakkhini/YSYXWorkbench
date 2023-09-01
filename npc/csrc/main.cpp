#include <Vsriz.h>
#include <Vsriz__Dpi.h>
#include <cstdint>
#include <stdio.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

static uint32_t mem[32];

int pmem_read(int pc) {
  printf("Enter pmem_read function.");
  return 0;
}

void mem_init() {}

int main(int argc, char **argv) {
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vsriz *top = new Vsriz(contextp);
  printf("Hello, SuanChou Processor Core!\n");

  mem_init();

  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 5);
  tfp->open("waveform.vcd");
  int sim_time = 30000000;

  while (contextp->time() < sim_time && !contextp->gotFinish()) {
    contextp->timeInc(1);
    top->clk = 0;
    top->eval();
    tfp->dump(contextp->time());
    contextp->timeInc(1);
    top->clk = 1;
    top->eval();
    tfp->dump(contextp->time());
  }

  tfp->close();

  top->final();
  delete top;

  return 0;
}
