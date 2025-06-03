#include <am.h>
#include <klib-macros.h>
#include <klib.h>

#define VSIMD_SETZERO_OPCODE 0x01
#define VSIMD_LOADDUP_OPCODE 0x02
#define VSIMD_LOAD_OPCODE 0x03
#define VSIMD_MUL_ADD_OPCODE 0x04
#define VSIMD_MEMSET_OPCODE 0x05
#define VSIMD_MEMMOVE_OPCODE 0x06
#define VSIMD_MEMCPY_OPCODE 0x07

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

void simd_memset(uintptr_t dest, uintptr_t value, size_t size) {
  // value is the value to set, size is the number of bytes to set
  simd_send_proto(VSIMD_MEMSET_OPCODE, dest, value, size);
}

void simd_memmove(uintptr_t dest, uintptr_t src, size_t size) {
  // src is the source address, dest is the destination address, size is the number of bytes to move
  simd_send_proto(VSIMD_MEMMOVE_OPCODE, dest, src, size);
}

void simd_memcpy(uintptr_t dest, uintptr_t src, size_t size) {
  // src is the source address, dest is the destination address, size is the number of bytes to copy
  simd_send_proto(VSIMD_MEMCPY_OPCODE, dest, src, size);
}
