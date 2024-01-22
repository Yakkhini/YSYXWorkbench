#include <common.h>
#include <cpu/cpu.h>
#include <memory/paddr.h>

static char *NPC_HOME = getenv("NPC_HOME");
static Vsriz *top;
static VerilatedVcdC *tfp;
static VerilatedContext *contextp;
static int current_pc;

static bool HALT = false;
static bool ABORT = false;
void finish();
void halt(int code) {
  if (code) {
    ABORT = true;
  }
  HALT = true;

  finish();
}

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

void cpu_exec(int n) {
  if (HALT) {
    Log("Program already finished!");
    return;
  }
  switch (n) {
  case -1:
    while (HALT == false) {
      single_clock();
    }
    break;
  default:
    for (int i = 0; i < n; i++) {
      if (HALT) {
        break;
      }
      single_clock();
    }
    break;
  }
}

int inst_fetch(int pc) {
  current_pc = pc;
  uint32_t inst = paddr_read(pc, 4);

  Log("0x%08X: Fetch instruction 0x%08X", pc, inst);

  return inst;
}

void finish() {
  if (ABORT) {
    Log("SRIZ: " ANSI_FMT("ABORT", ANSI_FG_RED) ANSI_FG_BLUE
        " at pc = 0x%08X " ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED),
        current_pc);
    return;
  }

  Log("SRIZ: " ANSI_FMT("QUIT", ANSI_FG_GREEN) ANSI_FG_BLUE
      " at pc = 0x%08X " ANSI_FMT("HIT BAD TRAP", ANSI_FG_GREEN),
      current_pc);
}

void cpu_exit() {
  tfp->close();
  top->final();
  delete top;
}