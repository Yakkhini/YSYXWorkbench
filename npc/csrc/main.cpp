#include <common.h>
#include <getopt.h>
#include <memory/paddr.h>
#include <monitor.h>
#include <stdio.h>
#include <stdlib.h>

static char *NPC_HOME = getenv("NPC_HOME");
static bool HALT = false;

int inst_fetch(int pc) {
  uint32_t inst = paddr_read(pc, 4);
  if (inst == 0x00100073) {
    HALT = true;
  }

  printf("0x%08X: Fetch instruction 0x%08X\n", pc, inst);

  return inst;
}

void single_clock(Vsriz *top, VerilatedContext *contextp, VerilatedVcdC *tfp) {
  contextp->timeInc(1);
  top->clk = 1;
  top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  top->clk = 0;
  top->eval();
  tfp->dump(contextp->time());
}

void reset(Vsriz *top, VerilatedContext *contextp, VerilatedVcdC *tfp) {
  top->rst = 1;
  contextp->timeInc(1);
  top->eval();
  tfp->dump(contextp->time());
  single_clock(top, contextp, tfp);
  top->rst = 0;
}

int main(int argc, char **argv) {

  parse_args(argc, argv);
  load_img();

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
  int sim_time = 100;

  reset(top, contextp, tfp);

  while (contextp->time() < sim_time && !contextp->gotFinish() &&
         HALT == false) {
    single_clock(top, contextp, tfp);
  }

  tfp->close();

  top->final();
  delete top;

  return 0;
}
