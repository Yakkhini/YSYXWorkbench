#include <v-simd/v-simd.h>

/*
 * Device address space specification:
 *
 * `0x00`: Operation code for VSIMD
 * `0x01`: ptr[0] - Pointer to the first value
 * `0x02`: ptr[1] - Pointer to the second value
 * `0x03`: ptr[2] - Pointer to the third value
 *
 * */
#define DEVICE_BASE 0xa2000000

enum VSIMDState {
  VSIMD_IDLE = 0x00,
  VSIMD_SETZERO = 0x01,
  VSIMD_LOADDUP = 0x02,
  VSIMD_LOAD = 0x03,
  VSIMD_MUL_ADD = 0x04,
};

typedef struct {
  enum VSIMDState state;
  uintptr_t ptr[3];
  fixedpt vreg[3];
} VSIMD;

VSIMD vsimd;

// Inner functions
void vsimd_execute();

void vsimd_init() {
  vsimd.state = VSIMD_IDLE;
  for (int i = 0; i < 3; i++) {
    vsimd.ptr[i] = fixedpt_fromint(0);
    vsimd.vreg[i] = fixedpt_fromint(0);
  }
}

void vsimd_receiver(paddr_t addr, int len, word_t data) {
  switch (addr - DEVICE_BASE) {
  case 0x00: // Opecode
    vsimd.state = (enum VSIMDState)data;
    vsimd_execute();
    break;
  case 0x01: // ptr[0]
    vsimd.ptr[0] = data;
    vsimd.vreg[0] = fixedpt_fromint(paddr_read(vsimd.ptr[0], len));
    Log("VSIMD ptr[0] set to %p", (void *)vsimd.ptr[0]);
    break;
  case 0x02: // ptr[1]
    vsimd.ptr[1] = data;
    vsimd.vreg[1] = fixedpt_fromint(paddr_read(vsimd.ptr[1], len));
    Log("VSIMD ptr[1] set to %p", (void *)vsimd.ptr[1]);
    break;
  case 0x03: // ptr[2]
    vsimd.ptr[2] = data;
    vsimd.vreg[2] = fixedpt_fromint(paddr_read(vsimd.ptr[2], len));
    Log("VSIMD ptr[2] set to %p", (void *)vsimd.ptr[2]);
    break;
  default:
    Log("VSIMD receiver: Invalid address 0x%08x", addr);
    assert(0);
    break;
  }
}

word_t vsimd_sender(paddr_t addr, int len) {
  Log("VSIMD do not need to send data. Check the Implementation");
  assert(0);

  return 0;
}

void vsimd_execute() {
  Log("VSIMD execute: %d", vsimd.state);
  Log("VSIMD ptr[0]: %p, ptr[1]: %p, ptr[2]: %p", (void *)vsimd.ptr[0],
      (void *)vsimd.ptr[1], (void *)vsimd.ptr[2]);
  // Log("VSIMD vreg[0]: %d, vreg[1]: %d, vreg[2]: %d",
  //     vsimd.vreg[0], vsimd.vreg[1], vsimd.vreg[2]);

  Log("VSIMD exectute function not implemented yet");
  assert(0);
}
