#include <nvboard.h>
#include "Vacalu.h"

void nvboard_bind_all_pins(Vacalu* top) {
	nvboard_bind_pin( &top->x0, BIND_RATE_SCR, BIND_DIR_IN , 4, SW15, SW14, SW13, SW12);
	nvboard_bind_pin( &top->x1, BIND_RATE_SCR, BIND_DIR_IN , 4, SW11, SW10, SW9, SW8);
	nvboard_bind_pin( &top->ctr, BIND_RATE_SCR, BIND_DIR_IN , 3, SW2, SW1, SW0);
	nvboard_bind_pin( &top->result, BIND_RATE_SCR, BIND_DIR_OUT, 4, LD15, LD14, LD13, LD12);
	nvboard_bind_pin( &top->flg, BIND_RATE_SCR, BIND_DIR_OUT, 1, LD0);
}
