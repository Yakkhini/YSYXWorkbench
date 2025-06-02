#include <v-simd/v-simd.h>

#define DEVICE_BASE 0xa2000000
#define DEVICE_SIZE 0x1000000

static uint8_t VSIMD_MEM[DEVICE_SIZE] __attribute((aligned(4096))) = {};
