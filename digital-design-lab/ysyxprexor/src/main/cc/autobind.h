#include <nvboard.h>
#include "Vysyxprexor.h"

void nvboard_bind_all_pins(Vysyxprexor* top) {
	nvboard_bind_pin( &top->io_a, BIND_RATE_SCR, BIND_DIR_IN , 1, SW0);
	nvboard_bind_pin( &top->io_b, BIND_RATE_SCR, BIND_DIR_IN , 1, SW1);
	nvboard_bind_pin( &top->IO_f, BIND_RATE_SCR, BIND_DIR_OUT, 1, LD0);
}
