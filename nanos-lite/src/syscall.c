#include "syscall.h"
#include <common.h>
void do_syscall(Context *c) {
  uintptr_t a[5];
  a[0] = c->GPR1;
  a[1] = c->GPR2;
  a[2] = c->GPR3;
  a[3] = c->GPR4;
  a[4] = c->GPRx;

  switch (a[0]) {
  case SYS_exit:
    Log("SYS_exit");
    halt(0);
    break;
  case SYS_yield:
    Log("SYS_yield");
    yield();
    a[4] = 0;
    break;
  default:
    panic("Unhandled syscall ID = %d", a[0]);
  }
}
