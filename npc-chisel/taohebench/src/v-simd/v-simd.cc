#include <v-simd/v-simd.h>

#define DEVICE_BASE 0xa2000000
#define DEVICE_SIZE 0x1000000

static uint8_t VSIMD_MEM[DEVICE_SIZE] __attribute((aligned(4096))) = {};

void vsimd_receiver(paddr_t addr, int len, word_t data) {
  Log("Not Implemented Yet");
  assert(0);
}

word_t vsimd_sender(paddr_t addr, int len) {
  Log("Not Implemented Yet");
  assert(0);

  return 0;
}
