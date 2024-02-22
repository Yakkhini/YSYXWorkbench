#include <common.h>
#include <cpu/difftest.h>
#include <device/device.h>

word_t mmio_read(paddr_t addr, int len) {
  word_t ret = 0;

  switch (addr) {
  case CONFIG_RTC_MMIO:
    timer_update();
    ret = npc_time.usec;
    break;
  case CONFIG_RTC_MMIO + 4:
    ret = npc_time.sec;
    break;
  default:
    break;
  }

#if CONFIG_DIFFTEST
  difftest_skip_ref();
#endif

  return ret;
}

void mmio_write(paddr_t addr, int len, word_t data) {
  switch (addr) {
  case CONFIG_SERIAL_MMIO:
    putchar((char)data);
    break;
  default:
    break;
  }

#if CONFIG_DIFFTEST
  difftest_skip_ref();
#endif

  return;
}

void device_init() {
  timer_init();
  return;
}