#include <common.h>
#include <cpu/ftrace.h>

static int ftrace_is_enabled = false;

void ftrace_init(char *path) {
  ftrace_is_enabled = true; // Maybe depends on TRACE or other options?

  elf_parse(path);

  ftrace_link_table_build();

  return;
}

bool get_ftrace_enable() { return ftrace_is_enabled; }