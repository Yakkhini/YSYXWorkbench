#include <common.h>
#include <cpu/cpu.h>
#include <getopt.h>
#include <memory/paddr.h>
#include <monitor.h>
#include <stdio.h>
#include <stdlib.h>

static bool HALT = false;

int inst_fetch(int pc) {
  uint32_t inst = paddr_read(pc, 4);
  if (inst == 0x00100073) {
    HALT = true;
  }

  Log("0x%08X: Fetch instruction 0x%08X", pc, inst);

  return inst;
}

int main(int argc, char **argv) {

  monitor_init(argc, argv);
  cpu_init(argc, argv);

  while (HALT == false) {
    single_clock();
  }

  cpu_exit();

  return 0;
}
