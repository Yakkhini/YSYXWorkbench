#include <memory/host.h>
#include <memory/paddr.h>

static uint8_t MEM[MSISE] __attribute((aligned(4096))) = {};

uint8_t *guest_to_host(paddr_t paddr) { return MEM + paddr - MBASE; }
paddr_t host_to_guest(uint8_t *haddr) { return haddr - MEM + MBASE; }

static word_t pmem_read(paddr_t addr, int len) {
  word_t ret = host_read(guest_to_host(addr), len);
  return ret;
}

static void pmem_write(paddr_t addr, int len, word_t data) {
  host_write(guest_to_host(addr), len, data);
}

word_t paddr_read(paddr_t addr, int len) {
  // IFDEF(CONFIG_MTRACE, Log("Read memory 0x%X for %i len.", addr, len));
  if (in_pmem(addr))
    return pmem_read(addr, len);
  // IFDEF(CONFIG_DEVICE, return mmio_read(addr, len));
  return 0;
}

void paddr_write(paddr_t addr, int len, word_t data) {
  // IFDEF(CONFIG_MTRACE, Log("Write memory 0x%X for %i len. Data: 0x%X", addr,
  // len, data));
  if (in_pmem(addr)) {
    pmem_write(addr, len, data);
    return;
  }
  // IFDEF(CONFIG_DEVICE, mmio_write(addr, len, data); return);
  // out_of_bound(addr);
}
