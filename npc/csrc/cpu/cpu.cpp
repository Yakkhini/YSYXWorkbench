#include <common.h>
#include <cpu/cpu.h>

static char *NPC_HOME = getenv("NPC_HOME");
static Vsriz *top;
static VerilatedVcdC *tfp;
static VerilatedContext *contextp;

void single_clock() {
  contextp->timeInc(1);
  top->clk = 1;
  top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  top->clk = 0;
  top->eval();
  tfp->dump(contextp->time());
}

void reset() {
  top->rst = 1;
  contextp->timeInc(1);
  top->eval();
  tfp->dump(contextp->time());
  single_clock();
  top->rst = 0;
}

void cpu_init(int argc, char **argv) {
  contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  top = new Vsriz(contextp);

  char wavefile_name[80];
  strcpy(wavefile_name, NPC_HOME);
  strcat(wavefile_name, "/build/waveform.vcd");
  Log("Welcome to SuanChou Processor Core Verilating Model.");
  Log("Wave Path: %s.", wavefile_name);

  Verilated::traceEverOn(true);
  tfp = new VerilatedVcdC;
  top->trace(tfp, 5);

  tfp->open(wavefile_name);
  int sim_time = 100;

  reset();
}

void cpu_exit() {
  tfp->close();
  top->final();
  delete top;
}