#include <common.h>
#include <device/device.h>

word_t mmio_read(paddr_t addr, int len) { return 0; }

void mmio_write(paddr_t addr, int len, word_t data) {
  switch (addr) {
  case CONFIG_SERIAL_MMIO:
    putchar((char)(data & 0xff));
    break;
  default:
    break;
  }

  return;
}