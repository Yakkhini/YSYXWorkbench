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

extern char _data_size, _data_load_start;

void _trm_init() {
  // Bootloader
  // The malloc just to adjust the heap start address
  void *lma_start = malloc((uintptr_t)&_data_size);
  lma_start = &_heap_start;
  memcpy(lma_start, &_data_load_start, (size_t)&_data_size);

  // Initialize UART
  // Line Control Register: Offset 3
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR
  outb(SERIAL_PORT + 3, 0B10000011); // ENABLE DLAB
  outb(SERIAL_PORT + 1, 0x00);       // Set Baud rate to 9600, MSB first
  outb(SERIAL_PORT + 0, 0x0C);       // Set Baud rate to 9600, LSB next
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR & DISABLE DLAB

  int ret = main(mainargs);
  halt(ret);
}
