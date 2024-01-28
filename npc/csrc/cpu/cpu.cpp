#include "Vsriz__Syms.h"
#include "sdb.h"
#include <common.h>
#include <cpu/cpu.h>
#include <cpu/difftest.h>
#include <cpu/disasm.h>
#include <cpu/ftrace.h>
#include <memory/vaddr.h>

CPU cpu;
NPCState npc_state = SRIZ_INIT;

static char *NPC_HOME = getenv("NPC_HOME");
static VerilatedVcdC *tfp;
static VerilatedContext *contextp;

void finish();
void halt(int code) {
  if (npc_state == SRIZ_RUNNING || npc_state == SRIZ_PAUSE) {
    switch (code) {
    case 0:
      npc_state = SRIZ_HALT;
      break;
    default:
      npc_state = SRIZ_ABORT;
      break;
    }
  }
}

int return_status() {
  switch (npc_state) {
  case SRIZ_HALT:
    return 0;
  case SRIZ_ABORT:
    return 1;
  default:
    return 2;
  }
}

void cpu_sync();
void cpu_check();
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
  cpu_check();
}

void reset() {
  contextp->timeInc(1);
  cpu.top->rst = 1;
  cpu.top->clk = 1;
  cpu.top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  cpu.top->clk = 0;
  cpu.top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  cpu.top->clk = 1;
  cpu.top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  cpu.top->rst = 0;
  cpu.top->clk = 0;
  cpu.top->eval();
  tfp->dump(contextp->time());

  cpu_sync();
  cpu_check();

  Log("SRIZ reset done.");
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
}

void cpu_exec(int n) {
  switch (npc_state) {
  case SRIZ_INIT:
    reset();
    npc_state = SRIZ_RUNNING;
    return;
  case SRIZ_HALT:
    Log("Program already finished!");
    return;
  case SRIZ_ABORT:
    Log("Program already aborted!");
    return;
  default:
    break;
  }

  npc_state = SRIZ_RUNNING;

  switch (n) {
  case -1:
    while (npc_state == SRIZ_RUNNING) {
      single_clock();
    }
    break;
  default:
    for (int i = 0; i < n; i++) {
      if (npc_state != SRIZ_RUNNING) {
        break;
      }
      single_clock();
    }
    break;
  }
}

void cpu_sync() {
  memcpy(cpu.regs, cpu.top->sriz->resgister_file->rf.m_storage, 32 * 4);
  if (cpu.top->sriz->resgister_file->wen) {
    cpu.regs[cpu.top->sriz->resgister_file->waddr] =
        cpu.top->sriz->resgister_file->regin;
  }
  cpu.pc_prev = cpu.pc;
  cpu.pc = cpu.top->sriz->pc_reg->pcin;
  cpu.inst = cpu.top->sriz->inst;
}

void cpu_check() {
  disassembler();
  if (cpu.top->sriz->IDU->lut->hit == 0) {
    Log("ERROR INST NOT SUPPORT: LUT HIT FAILED at pc = 0x%08X", cpu.pc_prev);
    npc_state = SRIZ_ABORT;
  }
  ftrace_check();
  mtrace();
  difftest_step(cpu.pc_prev, cpu.pc);
  check_wp();
  finish();
}

int inst_fetch(int pc) { return vaddr_ifetch(pc); }

void finish() {
  switch (npc_state) {
  case SRIZ_ABORT:
    Log("SRIZ: " ANSI_FMT("ABORT", ANSI_FG_RED) ANSI_FG_BLUE
        " at pc = 0x%08X " ANSI_FMT("HIT BAD TRAP", ANSI_FG_RED),
        cpu.pc);
    return;
  case SRIZ_HALT:
    Log("SRIZ: " ANSI_FMT("QUIT", ANSI_FG_GREEN) ANSI_FG_BLUE
        " at pc = 0x%08X " ANSI_FMT("HIT GOOD TRAP", ANSI_FG_GREEN),
        cpu.pc);
    return;
  default:
    return;
  }
}

void cpu_exit() {
  tfp->close();
  cpu.top->final();
  delete cpu.top;

  disasm_exit();
}