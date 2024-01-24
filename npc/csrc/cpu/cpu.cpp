#include "Vsriz__Syms.h"
#include <common.h>
#include <cpu/cpu.h>
#include <cpu/disasm.h>
#include <memory/paddr.h>

CPU cpu;

static char *NPC_HOME = getenv("NPC_HOME");
static VerilatedVcdC *tfp;
static VerilatedContext *contextp;

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

void cpu_sync();
void single_clock() {
  contextp->timeInc(1);
  cpu.top->clk = 1;
  cpu.top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  cpu.top->clk = 0;
  cpu.top->eval();
  tfp->dump(contextp->time());

  cpu_sync();
}

void reset() {
  cpu.top->rst = 1;
  contextp->timeInc(1);
  cpu.top->eval();
  tfp->dump(contextp->time());
  single_clock();
  cpu.top->rst = 0;
}

void cpu_init(int argc, char **argv) {
  disasm_init();

  contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  cpu.top = new Vsriz(contextp);

  char wavefile_name[80];
  strcpy(wavefile_name, NPC_HOME);
  strcat(wavefile_name, "/build/waveform.vcd");
  Log("Welcome to SuanChou Processor Core Verilating Model.");
  Log("Wave Path: %s.", wavefile_name);

  Verilated::traceEverOn(true);
  tfp = new VerilatedVcdC;
  cpu.top->trace(tfp, 5);

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

void cpu_sync() {
  memcpy(cpu.regs, cpu.top->sriz->resgister_file->rf.m_storage, 32 * 4);
  cpu.next_pc = cpu.top->sriz->pc_reg->pcin;
}

int inst_fetch(int pc) {
  cpu.pc = pc;
  uint32_t inst = paddr_read(pc, 4);

  disassembler(pc, inst);

  return inst;
}

void finish() {
  if (ABORT) {
    Log("SRIZ: " ANSI_FMT("ABORT", ANSI_FG_RED) ANSI_FG_BLUE
        " at pc = 0x%08X " ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED),
        cpu.pc);
    return;
  }

  Log("SRIZ: " ANSI_FMT("QUIT", ANSI_FG_GREEN) ANSI_FG_BLUE
      " at pc = 0x%08X " ANSI_FMT("HIT BAD TRAP", ANSI_FG_GREEN),
      cpu.pc);
}

void cpu_exit() {
  tfp->close();
  cpu.top->final();
  delete cpu.top;

  disasm_exit();
}