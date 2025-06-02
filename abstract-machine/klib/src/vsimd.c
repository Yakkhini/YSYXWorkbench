#include <am.h>
#include <klib-macros.h>
#include <klib.h>

// opcode + 3 * ptr
void simd_send_proto(uint8_t opcode, uint8_t ptr3, uint8_t ptr2, uint8_t ptr1) {
  uint32_t data = 0;
  data |= (uint32_t)opcode << 24;
  data |= (uint32_t)ptr3 << 16;
  data |= (uint32_t)ptr2 << 8;
  data |= (uint32_t)ptr1;

  // Send instruction to the virtual SIMD device
  send_vsimd(data);
}
