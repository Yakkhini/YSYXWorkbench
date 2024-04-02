#include <nvboard.h>
#include "Vysyxprelight.h"
#include "constrs.h"

void nvboard_bind_all_pins(Vysyxprelight* top) {
	nvboard_bind_pin( &top->io_led, BIND_RATE_SCR, BIND_DIR_OUT, 16, LD15, LD14, LD13, LD12, LD11, LD10, LD9, LD8, LD7, LD6, LD5, LD4, LD3, LD2, LD1, LD0);
	nvboard_bind_pin(&top->reset, BIND_RATE_SCR, BIND_DIR_IN, 1, RST);
	nvboard_bind_pin(&top->clock, BIND_RATE_SCR, BIND_DIR_IN, 1, BTNC);
}
