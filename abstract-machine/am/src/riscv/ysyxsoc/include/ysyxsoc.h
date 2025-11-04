#ifndef __AM_YSYXSOC_H__
#define __AM_YSYXSOC_H__

#include ISA_H // the macro `ISA_H` is defined in CFLAGS
               // it will be expanded as "x86/x86.h", "mips/mips32.h", ...

#define npc_trap(code) asm volatile("mv a0, %0; ebreak" : : "r"(code))

#define GPIO_BASE 0x10002000

#define RTC_ADDR (0x02000000)
#define UART_ADDR (0x10000000)
#define KBD_ADDR (0x10011000)
#define VGA_ADDR (0x21000000)
#define VGA_FB_ADDR (VGA_ADDR + 0x000000)
#define VGA_CTL_ADDR (VGA_ADDR + 0x1a0000)

#define LED_ADDR (GPIO_BASE + 0x0000)
#define SWITCH_ADDR (GPIO_BASE + 0x0004)
#define SEG_ADDR (GPIO_BASE + 0x0008)

#endif
