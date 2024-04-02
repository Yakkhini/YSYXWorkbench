#include <nvboard.h>
#include "Vnjumux.h"
#include "constrs.h"

void nvboard_bind_all_pins(Vnjumux* top) {
	nvboard_bind_pin( &top->io_y, BIND_RATE_SCR, BIND_DIR_IN , 2, SW1, SW0);
	nvboard_bind_pin( &top->io_x0, BIND_RATE_SCR, BIND_DIR_IN , 2, SW3, SW2);
	nvboard_bind_pin( &top->io_x1, BIND_RATE_SCR, BIND_DIR_IN , 2, SW5, SW4);
	nvboard_bind_pin( &top->io_x2, BIND_RATE_SCR, BIND_DIR_IN , 2, SW7, SW6);
	nvboard_bind_pin( &top->io_x3, BIND_RATE_SCR, BIND_DIR_IN , 2, SW9, SW8);
	nvboard_bind_pin( &top->io_f, BIND_RATE_SCR, BIND_DIR_OUT, 2, LD1, LD0);
}
