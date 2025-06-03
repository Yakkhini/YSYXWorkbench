#include <v-simd/v-simd.h>

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

void vsimd_init() {
  vsimd.state = VSIMD_IDLE;
  for (int i = 0; i < 3; i++) {
    vsimd.ptr[i] = fixedpt_fromint(0);
    vsimd.vreg[i] = fixedpt_fromint(0);
  }
}

void vsimd_receiver(paddr_t addr, int len, word_t data) {
  Log("Not Implemented Yet");
  assert(0);
}

word_t vsimd_sender(paddr_t addr, int len) {
  Log("Not Implemented Yet");
  assert(0);

  return 0;
}
