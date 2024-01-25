#include <common.h>
#include <cpu/cpu.h>
#include <monitor.h>
#include <sdb.h>

int main(int argc, char **argv) {

  monitor_init(argc, argv);
  cpu_init(argc, argv);

  sdb_mainloop();

  cpu_exit();

  return return_status();
}
