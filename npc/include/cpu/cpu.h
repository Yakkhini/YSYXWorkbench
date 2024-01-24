#ifndef __CPU_H__
#define __CPU_H__

#include <common.h>

typedef struct cpu {
  Vsriz *top;
  word_t regs[32];
  word_t pc = 0x80000000;
  word_t pc_prev;
  word_t inst;
} CPU;

extern CPU cpu;

void single_clock();
void reset();
void cpu_init(int argc, char **argv);
void cpu_exec(int n);
void cpu_exit();

#endif