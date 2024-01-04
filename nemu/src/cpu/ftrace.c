#include "common.h"
#include <cpu/ftrace.h>

static int ftrace_is_enabled = false;

void enable_ftrace() {
  ftrace_is_enabled = true; // Maybe depends on TRACE or other options?

  return;
}
