#include "include/ysyxsoc.h"
#include <am.h>
#include <klib-macros.h>
#include <klib.h>
#include <stdint.h>
#include <ysyxsoc.h>

extern char _heap_start, _heap_end;
int main(const char *args);
void _trm_init();

Area heap;
#ifndef MAINARGS
#define MAINARGS ""
#endif
static const char mainargs[] = MAINARGS;

void putch(char ch) {
  while ((inb(UART_ADDR + 5) & 0B00100000) == 0)
    ;
  outb(UART_ADDR, ch);
}

void halt(int code) {
  npc_trap(code);
  while (1)
    ;
}

extern char _ssbl_load_start, _ssbl_load_end;
extern char _ssbl_dest_start;

extern char _rwdata_load_start, _rwdata_load_end;
extern char _rxdata_load_start, _rxdata_load_end;
extern char _rxdata_dest_start, _rwdata_dest_start;

void ssbl();

__attribute__((section(".fsbl"))) void fsbl() {
  // First Stage Bootloader
  uint32_t ssbl_size =
      (uintptr_t)&_ssbl_load_end - (uintptr_t)&_ssbl_load_start;
  uint32_t *src = (uint32_t *)&_ssbl_load_start;
  uint32_t *dest = (uint32_t *)&_ssbl_dest_start;
  while (ssbl_size > 0) {
    *dest = *src;
    dest++;
    src++;
    ssbl_size -= 4;
  }

  ssbl();
}

__attribute__((section(".ssbl"))) void *
bootloader_memcpy(void *out, const void *in, size_t n) {
  uint32_t size = n;
  uint32_t *src = (uint32_t *)in;
  uint32_t *dest = (uint32_t *)out;
  while (size > 0) {
    *dest = *src;
    dest++;
    src++;
    size -= 4;
  }

  return out;
}

__attribute__((section(".ssbl"))) void ssbl() {

  // Second Stage Bootloader
  uint32_t rxdata_size =
      (uintptr_t)&_rxdata_load_end - (uintptr_t)&_rxdata_load_start;
  uint32_t rwdata_size =
      (uintptr_t)&_rwdata_load_end - (uintptr_t)&_rwdata_load_start;
  bootloader_memcpy(&_rxdata_dest_start, &_rxdata_load_start, rxdata_size);
  bootloader_memcpy(&_rwdata_dest_start, &_rwdata_load_start, rwdata_size);

  heap.start = &_heap_start;
  heap.end = &_heap_end;

  _trm_init();
}

void _trm_init() {

  // Initialize UART
  // Line Control Register: Offset 3
  outb(UART_ADDR + 3, 0B00000011); // RESET LCR
  outb(UART_ADDR + 3, 0B10000011); // ENABLE DLAB
  outb(UART_ADDR + 1, 0x00);       // Set Baud rate to 9600, MSB first
  outb(UART_ADDR + 0, 0x0C);       // Set Baud rate to 9600, LSB next
  outb(UART_ADDR + 3, 0B00000011); // RESET LCR & DISABLE DLAB

  uint32_t rxdata_size =
      (uintptr_t)&_rxdata_load_end - (uintptr_t)&_rxdata_load_start;
  uint32_t rwdata_size =
      (uintptr_t)&_rwdata_load_end - (uintptr_t)&_rwdata_load_start;
  printf("RX Bootload Finish. Source start address: 0x%08X, Source end "
         "address: 0x%08X, Dest start address: 0x%08X, size: %ld\n",
         &_rxdata_load_start, &_rxdata_load_end, &_rxdata_dest_start,
         rxdata_size);
  printf("RW Bootload Finish. Source start address: 0x%08X, Source end "
         "address: 0x%08X, Dest start address: 0x%08X, size: %ld\n",
         &_rwdata_load_start, &_rwdata_load_end, &_rwdata_dest_start,
         rwdata_size);
  printf("Heap range: [0x%08X, 0x%08X)\n", heap.start, heap.end);

  int ret = main(mainargs);
  halt(ret);
}
