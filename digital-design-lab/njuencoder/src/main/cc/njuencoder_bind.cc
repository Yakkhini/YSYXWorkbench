#include <nvboard.h>
#include "Vnjuencoder.h"
#include "constrs.h"

void nvboard_bind_all_pins(Vnjuencoder* top) {
	nvboard_bind_pin( &top->io_oneHot8, BIND_RATE_SCR, BIND_DIR_IN , 8, SW7, SW6, SW5, SW4, SW3, SW2, SW1, SW0);
	nvboard_bind_pin( &top->io_enable, BIND_RATE_SCR, BIND_DIR_IN , 1, SW15);
	nvboard_bind_pin( &top->io_ledNum, BIND_RATE_SCR, BIND_DIR_OUT, 3, LD2, LD1, LD0);
	nvboard_bind_pin( &top->io_ledEN, BIND_RATE_SCR, BIND_DIR_OUT, 1, LD15);
	nvboard_bind_pin( &top->io_hex8Num, BIND_RATE_SCR, BIND_DIR_OUT, 8, SEG0A, SEG0B, SEG0C, SEG0D, SEG0E, SEG0F, SEG0G, DEC0P);
}
