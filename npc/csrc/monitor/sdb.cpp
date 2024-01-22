#include <common.h>
#include <cpu/cpu.h>
#include <sdb.h>

static bool HALT = false;

void sdb_mainloop() {
  while (HALT == false) {
    single_clock();
  }
}

void halt() { HALT = true; }