#include <am.h>
#include <nemu.h>

static uint64_t boot_time;

void __am_timer_init() {
  boot_time = (inl(RTC_ADDR)) + ((uint64_t)inl(RTC_ADDR + 4) << 32);
}

void __am_timer_uptime(AM_TIMER_UPTIME_T *uptime) {
  uint64_t now = (inl(RTC_ADDR)) + ((uint64_t)inl(RTC_ADDR + 4) << 32);
  uptime->us = now - boot_time;
}

void __am_timer_rtc(AM_TIMER_RTC_T *rtc) {
  rtc->second = 0;
  rtc->minute = 0;
  rtc->hour = 0;
  rtc->day = 0;
  rtc->month = 0;
  rtc->year = 1900;
}
