#include "common.h"
#ifndef __CPU_FTRACE_H__

void enable_ftrace();
void ftrace_check(vaddr_t source_addr, vaddr_t target_addr);

#endif