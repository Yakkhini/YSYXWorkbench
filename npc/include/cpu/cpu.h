#ifndef __CPU_H__
#define __CPU_H__

#include <common.h>

enum NPCState { SRIZ_INIT, SRIZ_RUNNING, SRIZ_PAUSE, SRIZ_HALT, SRIZ_ABORT };

typedef struct cpu {
  Vsriz *top;
  word_t regs[32];
  word_t pc = 0x80000000;
  word_t pc_prev;
  word_t inst;
} CPU;

extern NPCState npc_state;
extern CPU cpu;

void single_clock();
void reset();
void cpu_init(int argc, char **argv);
void cpu_exec(int n);
void cpu_exit();

void isa_reg_display();
word_t isa_reg_str2val(const char *s, bool *success);
int return_status();

#endif