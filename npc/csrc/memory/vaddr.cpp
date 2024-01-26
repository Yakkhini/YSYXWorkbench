#include <common.h>
#include <memory/paddr.h>
#include <memory/vaddr.h>

word_t vaddr_ifetch(vaddr_t addr) { return paddr_read(addr, 4); }

int vaddr_read(int addr, const svBitVecVal *len) {
  if (len[0] == 1 && len[1] == 0) {
    return paddr_read(addr, 1);
  } else if (len[0] == 0 && len[1] == 1) {
    return paddr_read(addr, 2);
  } else {
    return paddr_read(addr, 4);
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