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

  printf("0x%08X: Fetch instruction 0x%08X\n", pc, inst);

  return inst;
}

int main(int argc, char **argv) {

  parse_args(argc, argv);
  load_img();

  cpu_init(argc, argv);

  while (HALT == false) {
    single_clock();
  }

  cpu_exit();

  return 0;
}
