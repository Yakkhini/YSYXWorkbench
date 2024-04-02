#include "Vnjushift.h"
#include "constrs.h"
#include <nvboard.h>

void nvboard_bind_all_pins(Vnjushift *top) {
  nvboard_bind_pin(&top->reset, BIND_RATE_SCR, BIND_DIR_IN, 1, RST);
  nvboard_bind_pin(&top->io_trigger, BIND_RATE_SCR, BIND_DIR_IN, 1, BTNC);
  nvboard_bind_pin(&top->io_seed, BIND_RATE_SCR, BIND_DIR_IN, 16, SW15, SW14,
                   SW13, SW12, SW11, SW10, SW9, SW8, SW7, SW6, SW5, SW4, SW3,
                   SW2, SW1, SW0);
  nvboard_bind_pin(&top->io_outData, BIND_RATE_SCR, BIND_DIR_OUT, 16, LD15,
                   LD14, LD13, LD12, LD11, LD10, LD9, LD8, LD7, LD6, LD5, LD4,
                   LD3, LD2, LD1, LD0);
  nvboard_bind_pin(&top->io_dataHex8, BIND_RATE_SCR, BIND_DIR_OUT, 32, SEG3A,
                   SEG3B, SEG3C, SEG3D, SEG3E, SEG3F, SEG3G, DEC3P, SEG2A,
                   SEG2B, SEG2C, SEG2D, SEG2E, SEG2F, SEG2G, DEC2P, SEG1A,
                   SEG1B, SEG1C, SEG1D, SEG1E, SEG1F, SEG1G, DEC1P, SEG0A,
                   SEG0B, SEG0C, SEG0D, SEG0E, SEG0F, SEG0G, DEC0P);
}
