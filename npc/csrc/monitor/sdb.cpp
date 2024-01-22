#include <common.h>
#include <cpu/cpu.h>
#include <sdb.h>

void sdb_mainloop() { cpu_exec(-1); }
