#include <nvboard.h>
#include "Vagkbd.h"

void nvboard_bind_all_pins(Vagkbd* top) {
	nvboard_bind_pin( &top->ps2_clk, BIND_RATE_RT , BIND_DIR_IN , 1, PS2_CLK);
	nvboard_bind_pin( &top->ps2_data, BIND_RATE_RT , BIND_DIR_IN , 1, PS2_DAT);
}
