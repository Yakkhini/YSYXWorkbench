#include <am.h>
#include <klib-macros.h>
#include <klib.h>

#define VSIMD_SETZERO_OPCODE 0x01
#define VSIMD_LOADDUP_OPCODE 0x02
#define VSIMD_LOAD_OPCODE 0x03
#define VSIMD_MUL_ADD_OPCODE 0x04

// setzero operation
void simd_setzero(uintptr_t ptr3, uintptr_t ptr2, uintptr_t ptr1) {
  simd_send_proto(VSIMD_SETZERO_OPCODE, ptr3, ptr2, ptr1);
}

// load operation
void simd_load(uintptr_t dest, uintptr_t src) {
  simd_send_proto(VSIMD_LOAD_OPCODE, dest, 0x00, src);
}

// loaddup operation: ptr2 is empty, ptr1 is the value to load and duplicate
void simd_loaddup(uintptr_t dest, uintptr_t src) {
  simd_send_proto(VSIMD_LOADDUP_OPCODE, dest, 0x00, src);
}

void simd_mul_add(uintptr_t dest, uintptr_t src1, uintptr_t src2) {
  // src1 is the value to multiply, src2 is the value to add
  simd_send_proto(VSIMD_MUL_ADD_OPCODE, dest, src1, src2);
}
