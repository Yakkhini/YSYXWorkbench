#ifndef __CPU_H__
#define __CPU_H__

void halt();

void single_clock();
void reset();
void cpu_init(int argc, char **argv);
void cpu_exec(int n);
void cpu_exit();

#endif