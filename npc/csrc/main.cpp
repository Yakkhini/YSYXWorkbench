#include <Vsriz.h>
#include <Vsriz__Dpi.h>
#include <cstdint>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

static uint32_t MEM[] = {
    0x3e800093, 0x3e810093, 0x3e810193, 0x7d018213, 0x3e820293,
    0x3e828313, 0x7d008113, 0x00430313, 0x3e800093, 0x3e810093,
    0x3e810193, 0x7d018213, 0x3e820293, 0x3e828313, 0x7d008113,
    0x00430313, 0x3e800093, 0x3e810093, 0x3e810193, 0x7d018213,
    0x3e820293, 0x3e828313, 0x7d008113, 0x00430313, 0x00100073};

static char *NPC_HOME = getenv("NPC_HOME");

int pmem_read(int pc) {
  if (pc == 0) {
    return 0;
  }
  uint32_t mem_posi = (pc - 0x80000000) / 0x4;
  if (mem_posi > 32) {
    return 0;
  }
  return MEM[mem_posi];
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

  while (contextp->time() < sim_time && !contextp->gotFinish()) {
    single_clock(top, contextp, tfp);
  }

  tfp->close();

  top->final();
  delete top;

  return 0;
}
