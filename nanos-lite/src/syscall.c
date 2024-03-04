#include "syscall.h"
#include "config.h"
#include <common.h>

void do_syscall(Context *c) {
#if CONFIG_STRACE
  char *syscall_names[] = {
      "SYS_exit",  "SYS_yield",  "SYS_open",   "SYS_read",   "SYS_write",
      "SYS_kill",  "SYS_getpid", "SYS_close",  "SYS_lseek",  "SYS_brk",
      "SYS_fstat", "SYS_time",   "SYS_signal", "SYS_execve", "SYS_fork",
      "SYS_link",  "SYS_unlink", "SYS_wait",   "SYS_times",  "SYS_gettimeofday",
  };
#endif

  uintptr_t type = c->GPR1;
  uintptr_t ret = c->GPRx;

  uintptr_t a[3];
  a[0] = c->GPR2;
  a[1] = c->GPR3;
  a[2] = c->GPR4;

#if CONFIG_STRACE
  Log("Handling syscall ID = %d (%s), arguments = %d, %d, %d, %d", type,
      syscall_names[type], a[0], a[1], a[2]);
#endif

  switch (type) {
  case SYS_exit:
    halt(a[0]);
    break;
  case SYS_yield:
    yield();
    ret = 0;
    break;
  default:
    panic("Unhandled syscall ID = %d", type);
  }

  c->GPRx = ret;

#if CONFIG_STRACE
  Log("Return value = %d", ret);
#endif
}
