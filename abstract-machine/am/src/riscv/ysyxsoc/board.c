#include <am.h>
#include <ysyxsoc.h>

uint8_t seg_map[16] = {0b00000010, 0b10011111, 0b00100101, 0b00001101,
                       0b10011001, 0b01001001, 0b01000001, 0b00011111,
                       0b00000001, 0b00011001, 0b00010001, 0b11000001,
                       0b01100011, 0b10000101, 0b01100001, 0b01110001};

void __am_set_light(AM_SOC_LEDS_T *am_leds) {
  outw(LED_ADDR, am_leds->value);
  return;
}

void __am_read_switch(AM_SOC_SWITCHES_T *am_switches) {
  am_switches->value = inw(SWITCH_ADDR);
  return;
}

void __am_set_seg(AM_SOC_7SEGS_T *am_7segs) {
  uint8_t byte1 = seg_map[(am_7segs->value >> 12) & 0xf];
  uint8_t byte2 = seg_map[(am_7segs->value >> 8) & 0xf];
  uint8_t byte3 = seg_map[(am_7segs->value >> 4) & 0xf];
  uint8_t byte4 = seg_map[am_7segs->value & 0xf];
  uint32_t seg_value = (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4;
  outl(SEG_ADDR, seg_value);
  return;
}
