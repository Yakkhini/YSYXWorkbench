#include <nvboard.h>
#include "Vnjualu.h"
#include "constrs.h"

void nvboard_bind_all_pins(Vnjualu* top) {
	nvboard_bind_pin( &top->io_numX, BIND_RATE_SCR, BIND_DIR_IN , 4, SW15, SW14, SW13, SW12);
	nvboard_bind_pin( &top->io_numY, BIND_RATE_SCR, BIND_DIR_IN , 4, SW11, SW10, SW9, SW8);
	nvboard_bind_pin( &top->io_enable, BIND_RATE_SCR, BIND_DIR_IN , 1, BTNC);
	nvboard_bind_pin( &top->io_controlSignal, BIND_RATE_SCR, BIND_DIR_IN , 3, SW2, SW1, SW0);
	nvboard_bind_pin( &top->io_resultLED, BIND_RATE_SCR, BIND_DIR_OUT, 4, LD15, LD14, LD13, LD12);
	nvboard_bind_pin( &top->io_resultHex8, BIND_RATE_SCR, BIND_DIR_OUT, 8, SEG0A, SEG0B, SEG0C, SEG0D, SEG0E, SEG0F, SEG0G, DEC0P);
}
