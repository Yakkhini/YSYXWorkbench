#include <am.h>
#include <klib-macros.h>
#include <klib.h>
#include <ysyxsoc.h>

extern char _heap_start, _stack_pointer;
int main(const char *args);
void _trm_init();

Area heap;
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

__attribute__ ((section (".bootloader")))
void *bootloader_memcpy(void *out, const void *in, size_t n) {
  uintptr_t offset = 0;
  while (offset < n) {
    ((char *)out)[offset] = ((char *)in)[offset];
    offset++;
  }

  return out;
}

extern char _psram_start, _rwdata_load_start, _rwdata_load_end;
extern char _sram_start, _rxdata_load_start, _rxdata_load_end;

__attribute__ ((section (".bootloader")))
void bootloader() {

  // Bootloader
  uint32_t rxdata_size = (uintptr_t)&_rxdata_load_end - (uintptr_t)&_rxdata_load_start;
  uint32_t rwdata_size = (uintptr_t)&_rwdata_load_end - (uintptr_t)&_rwdata_load_start;
  bootloader_memcpy(&_sram_start, &_rxdata_load_start, rxdata_size);
  bootloader_memcpy(&_psram_start, &_rwdata_load_start, rwdata_size);

  heap.start = &_heap_start;
  heap.end = &_stack_pointer;

  _trm_init();

}

void _trm_init() {

  // Initialize UART
  // Line Control Register: Offset 3
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR
  outb(SERIAL_PORT + 3, 0B10000011); // ENABLE DLAB
  outb(SERIAL_PORT + 1, 0x00);       // Set Baud rate to 9600, MSB first
  outb(SERIAL_PORT + 0, 0x0C);       // Set Baud rate to 9600, LSB next
  outb(SERIAL_PORT + 3, 0B00000011); // RESET LCR & DISABLE DLAB

  uint32_t rxdata_size = (uintptr_t)&_rxdata_load_end - (uintptr_t)&_rxdata_load_start;
  uint32_t rwdata_size = (uintptr_t)&_rwdata_load_end - (uintptr_t)&_rwdata_load_start;
  printf("RX Bootload Finish. Source start address: 0x%08X, Source end address: 0x%08X, Dest start address: 0x%08X, size: %ld\n",
         &_rxdata_load_start, &_rxdata_load_end, &_sram_start, rxdata_size);
  printf("RW Bootload Finish. Source start address: 0x%08X, Source end address: 0x%08X, Dest start address: 0x%08X, size: %ld\n",
         &_rwdata_load_start, &_rwdata_load_end, &_psram_start, rwdata_size);
  printf("Heap range: [0x%08X, 0x%08X)\n", heap.start, heap.end);

  int ret = main(mainargs);
  halt(ret);
}
