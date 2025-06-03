#include <am.h>
#include <klib-macros.h>
#include <npc.h>

extern char _heap_start;
int main(const char *args);

extern char _pmem_start;
#define PMEM_SIZE (128 * 1024 * 1024)
#define PMEM_END ((uintptr_t)&_pmem_start + PMEM_SIZE)

Area heap = RANGE(&_heap_start, PMEM_END);
#ifndef MAINARGS
#define MAINARGS ""
#endif
static const char mainargs[] = MAINARGS;

void putch(char ch) { outb(SERIAL_PORT, ch); }

// opcode + 3 * ptr
void simd_send_proto(uintptr_t opcode, uintptr_t ptr3, uintptr_t ptr2, uintptr_t ptr1) {
  outl(VIRTUAL_SIMD_ADDR + 0x04, ptr3);
  outl(VIRTUAL_SIMD_ADDR + 0x08, ptr2);
  outl(VIRTUAL_SIMD_ADDR + 0x0C, ptr1);

  // Also use opcode to trigger the operation
  outl(VIRTUAL_SIMD_ADDR, opcode);
}

void halt(int code) {
  npc_trap(code);
  while (1)
    ;
}

void _trm_init() {
  int ret = main(mainargs);
  halt(ret);
}
