#include <am.h>
#include <ysyxsoc.h>

void __am_set_light(AM_SOC_LEDS_T *am_leds) {
  outw(LED_ADDR, am_leds->value);
  return;
}

void __am_read_switch(AM_SOC_SWITCHES_T *am_switches) {
  am_switches->value = inw(SWITCH_ADDR);
  return;
}

void __am_set_seg(AM_SOC_7SEGS_T am_7segs) {
  outl(SEG_ADDR, am_7segs.value);
  return;
}
