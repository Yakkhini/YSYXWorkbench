#include <Vsriz.h>
#include <Vsriz__Dpi.h>
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

static uint32_t MEM[] = {0x3e800093, 0x7d008113, 0xc1810193,
                         0x83018213, 0x3e820293, 0x00430313};

static char *NPC_HOME = getenv("NPC_HOME");

int pmem_read(int pc) {
  printf("Enter pmem_read function.");
  return 0;
}

void mem_init() {}

int main(int argc, char **argv) {
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vsriz *top = new Vsriz(contextp);

  char wavefile_name[80];
  strcpy(wavefile_name, NPC_HOME);
  strcat(wavefile_name, "/build/waveform.vcd");
  printf("Hello, SuanChou Processor Core!\n");
  printf("Wave Path: %s\n", wavefile_name);

  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 5);

  tfp->open(wavefile_name);
  int sim_time = 10000000;

  contextp->timeInc(1);
  top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  top->rst = 1;
  top->clk = 1;
  top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  top->rst = 0;
  top->clk = 0;
  top->eval();

  while (contextp->time() < sim_time && !contextp->gotFinish()) {
    contextp->timeInc(1);
    top->clk = 1;
    top->eval();
    tfp->dump(contextp->time());
    contextp->timeInc(1);
    top->clk = 0;
    top->eval();
    tfp->dump(contextp->time());
  }

  tfp->close();

  top->final();
  delete top;

  return 0;
}
