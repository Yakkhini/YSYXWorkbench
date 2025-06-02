#include <am.h>
#include <klib-macros.h>
#include <klib.h>

#define VSIMD_SETZERO_OPCODE 0x00
#define VSIMD_LOADDUP_OPCODE 0x01
#define VSIMD_MUL_ADD_OPCODE 0x02

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

// setzero operation
void simd_setzero(uintptr_t ptr3, uintptr_t ptr2, uintptr_t ptr1) {
  simd_send_proto(VSIMD_SETZERO_OPCODE, ptr3, ptr2, ptr1);
}

// loaddup operation: ptr2 is empty, ptr1 is the value to load
void simd_loaddup(uintptr_t dest, uintptr_t src) {
  simd_send_proto(VSIMD_LOADDUP_OPCODE, dest, 0x00, src);
}

void simd_mul_add(uintptr_t dest, uintptr_t src1, uintptr_t src2) {
  // src1 is the value to multiply, src2 is the value to add
  simd_send_proto(VSIMD_MUL_ADD_OPCODE, dest, src1, src2);
}
