#include "syscall.h"
#include "config.h"
#include <common.h>
#include <fs.h>
#include <stdint.h>
#include <string.h>

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
  Log("Handling syscall ID = %d (%s), arguments = %d, %d, %d", type,
      syscall_names[type], a[0], a[1], a[2]);
  if (type == SYS_open) {
    Log("File name = %s", a[0]);
  }

  if (type == SYS_read || type == SYS_write || type == SYS_close ||
      type == SYS_lseek) {
    char *strace_file_name = file_name(a[0]);
    Log("File name = %s", strace_file_name);
  }
#endif

  switch (type) {
  case SYS_exit:
    halt(a[0]);
    break;
  case SYS_yield:
    yield();
    ret = 0;
    break;
  case SYS_open:
    ret = fs_open((const char *)a[0], 0, 0);
    break;
  case SYS_write: // a[0] = fd, a[1] = buf, a[2] = count
    ret = fs_write(a[0], (const void *)a[1], a[2]);
    break;
  case SYS_read: // a[0] = fd, a[1] = buf, a[2] = count
    ret = fs_read(a[0], (void *)a[1], a[2]);
    break;
  case SYS_close:
    ret = fs_close(a[0]);
    break;
  case SYS_lseek:
    ret = fs_lseek(a[0], a[1], a[2]);
    break;
  case SYS_brk:
    memset((void *)a[2], 0, a[1]);
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
