#include <common.h>
#include <cpu/cpu.h>
#include <getopt.h>
#include <memory/paddr.h>
#include <monitor.h>
#include <sdb.h>
#include <stdio.h>
#include <stdlib.h>

int inst_fetch(int pc) {
  uint32_t inst = paddr_read(pc, 4);
  if (inst == 0x00100073) {
    halt();
  }

  Log("0x%08X: Fetch instruction 0x%08X", pc, inst);

  return inst;
}

int main(int argc, char **argv) {

  monitor_init(argc, argv);
  cpu_init(argc, argv);

  sdb_mainloop();

  cpu_exit();

  return 0;
}
