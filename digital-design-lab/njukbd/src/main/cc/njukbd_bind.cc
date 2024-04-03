#include "Vnjukbd.h"
#include "constrs.h"
#include <nvboard.h>

void nvboard_bind_all_pins(Vnjukbd *top) {
  nvboard_bind_pin(&top->io_ps2Clk, BIND_RATE_RT, BIND_DIR_IN, 1, PS2_CLK);
  nvboard_bind_pin(&top->io_ps2Data, BIND_RATE_RT, BIND_DIR_IN, 1, PS2_DAT);
  nvboard_bind_pin(&top->io_hex8, BIND_RATE_SCR, BIND_DIR_OUT, 48,

                   SEG5A, SEG5B, SEG5C, SEG5D, SEG5E, SEG5F, SEG5G, DEC5P,
                   SEG4A, SEG4B, SEG4C, SEG4D, SEG4E, SEG4F, SEG4G, DEC4P,
                   SEG3A, SEG3B, SEG3C, SEG3D, SEG3E, SEG3F, SEG3G, DEC3P,
                   SEG2A, SEG2B, SEG2C, SEG2D, SEG2E, SEG2F, SEG2G, DEC2P,
                   SEG1A, SEG1B, SEG1C, SEG1D, SEG1E, SEG1F, SEG1G, DEC1P,
                   SEG0A, SEG0B, SEG0C, SEG0D, SEG0E, SEG0F, SEG0G, DEC0P);
}
