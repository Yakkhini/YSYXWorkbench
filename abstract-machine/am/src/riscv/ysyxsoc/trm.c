#include <am.h>
#include <klib-macros.h>
#include <klib.h>
#include <ysyxsoc.h>

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

void putch(char ch) {
  while ((inb(SERIAL_PORT + 5) & 0B00100000) == 0)
    ;
  outb(SERIAL_PORT, ch);
}

void halt(int code) {
  npc_trap(code);
  while (1)
    ;
}

extern char _sram_start, _rwdata_load_start, _rwdata_load_end;

void _trm_init() {
  // Bootloader
  // The malloc just to adjust the heap start address

  uint32_t rwdata_size = (uintptr_t)&_rwdata_load_end - (uintptr_t)&_rwdata_load_start;
  memcpy(&_sram_start, &_rwdata_load_start, rwdata_size);

  // Initialize UART
  // Line Control Register: Offset 3
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR
  outb(SERIAL_PORT + 3, 0B10000011); // ENABLE DLAB
  outb(SERIAL_PORT + 1, 0x00);       // Set Baud rate to 9600, MSB first
  outb(SERIAL_PORT + 0, 0x0C);       // Set Baud rate to 9600, LSB next
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR & DISABLE DLAB

  printf("Bootload Finish. Source start address: 0x%08X, Source end address: 0x%08X, Dest start address: 0x%08X, size: %ld\n",
         &_rwdata_load_start, &_rwdata_load_end, &_sram_start, rwdata_size);

  int ret = main(mainargs);
  halt(ret);
}
