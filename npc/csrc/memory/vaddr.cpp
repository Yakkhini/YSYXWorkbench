#include <common.h>
#include <memory/paddr.h>
#include <memory/vaddr.h>

word_t vaddr_ifetch(vaddr_t addr) { return paddr_read(addr, 4); }

int vaddr_read(int addr, const svBitVecVal *len) {

  switch (*len) {
    int ret;
  case 1:
    ret = paddr_read(addr, 1);
    Log("MTRACE: addr 0x%08x read byte 0x%08x", addr, ret);
    return ret;
  case 2:
    ret = paddr_read(addr, 2);
    Log("MTARCE: addr 0x%08x read half word 0x%08x", addr, ret);
    return ret;
  case 3:
    ret = paddr_read(addr, 4);
    Log("MTRACE: addr 0x%08x read word 0x%08x", addr, ret);
    return ret;
  default:
    Log("Invalid memory access type");
    return 0;
  }
}

void vaddr_write(int addr, const svBitVecVal *len, int data) {
  if (len[0] == 1 && len[1] == 0) {
    paddr_write(addr, 1, data);
  } else if (len[0] == 0 && len[1] == 1) {
    paddr_write(addr, 2, data);
  } else if (len[0] == 1 && len[1] == 1) {
    paddr_write(addr, 4, data);
  }
}