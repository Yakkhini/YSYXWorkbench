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
  paddr_t ptr[3];
  fixedpt vreg[3];
} VSIMD;

VSIMD vsimd;
fixedpt fixedpt_none = fixedpt_fromint(0);

// Inner functions
void vsimd_execute();
void vsimd_setzero();
void vsimd_load();
fixedpt *vsimd_ptr_to_fixedpt(paddr_t ptr);

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
  case 0x04: // ptr[0]
    vsimd.ptr[0] = data;
    vsimd.vreg[0] = *(fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[0]);
    Log("VSIMD ptr[0] set to 0x%08x", vsimd.ptr[0]);
    break;
  case 0x08: // ptr[1]
    vsimd.ptr[1] = data;
    vsimd.vreg[1] = *(fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[1]);
    Log("VSIMD ptr[1] set to 0x%08x", vsimd.ptr[1]);
    break;
  case 0x0C: // ptr[2]
    vsimd.ptr[2] = data;
    vsimd.vreg[2] = *(fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[2]);
    Log("VSIMD ptr[2] set to 0x%08x", vsimd.ptr[2]);
    break;
  default:
    Log("VSIMD receiver: Invalid address 0x%08x", addr);
    assert(0);
    break;
  }
}

fixedpt *vsimd_ptr_to_fixedpt(paddr_t ptr) {
  Log("VSIMD pointer to fixedpt: 0x%08x", ptr);
  if (ptr == 0) {
    return &fixedpt_none;
  }

  if (in_pmem(ptr)) {
    return (fixedpt *)guest_to_host(ptr);
  } else {
    Log("VSIMD pointer to fixedpt: Invalid address 0x%08x", ptr);
    assert(0);
    return 0;
  }
}

word_t vsimd_sender(paddr_t addr, int len) {
  Log("VSIMD do not need to send data. Check the Implementation");
  assert(0);

  return 0;
}

void vsimd_execute() {
  Log("VSIMD execute: %d", vsimd.state);
  Log("VSIMD ptr[0]: 0x%08x, ptr[1]: 0x%08x, ptr[2]: 0x%08x", vsimd.ptr[0],
      vsimd.ptr[1], vsimd.ptr[2]);
  Log("VSIMD vreg[0]: %s, vreg[1]: %s, vreg[2]: %s",
      fixedpt_cstr(vsimd.vreg[0], -1), fixedpt_cstr(vsimd.vreg[1], -1),
      fixedpt_cstr(vsimd.vreg[2], -1));

  switch (vsimd.state) {
  case VSIMD_SETZERO:
    vsimd_setzero();
    break;
  case VSIMD_LOAD:
    vsimd_load();
    break;
  default:
    Log("VSIMD exectute function not implemented yet, state: %d", vsimd.state);
    assert(0);
    break;
  }
}

void vsimd_setzero() {
  for (int i = 0; i < 3; i++) {
    if (vsimd.ptr[i] == 0) {
      continue;
    }
    *(fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[i]) = fixedpt_fromint(0);
    *((fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[i]) + 1) = fixedpt_fromint(0);
  }
}

// [2] to [0]
void vsimd_load() {
  Log("VSIMD load %s to 0x%08x", fixedpt_cstr(vsimd.vreg[2], -1), vsimd.ptr[0]);
  *(fixedpt *)vsimd_ptr_to_fixedpt(vsimd.ptr[0]) = vsimd.vreg[2];
}
