#include "syscall.h"
#include <common.h>

void do_syscall(Context *c) {
  char *syscall_names[] = {
      "SYS_exit",  "SYS_yield",  "SYS_open",   "SYS_read",   "SYS_write",
      "SYS_kill",  "SYS_getpid", "SYS_close",  "SYS_lseek",  "SYS_brk",
      "SYS_fstat", "SYS_time",   "SYS_signal", "SYS_execve", "SYS_fork",
      "SYS_link",  "SYS_unlink", "SYS_wait",   "SYS_times",  "SYS_gettimeofday",
  };

  uintptr_t a[5];
  a[0] = c->GPR1;
  a[1] = c->GPR2;
  a[2] = c->GPR3;
  a[3] = c->GPR4;
  a[4] = c->GPRx;

  switch (a[0]) {
  case SYS_exit:
    halt(0);
    break;
  case SYS_yield:
    yield();
    a[4] = 0;
    break;
  default:
    panic("Unhandled syscall ID = %d", a[0]);
  }

  Log("Handling syscall ID = %d (%s), arguments = %d, %d, %d, %d, "
      "return value = %d.",
      a[0], syscall_names[a[0]], a[1], a[2], a[3], a[4]);
}
