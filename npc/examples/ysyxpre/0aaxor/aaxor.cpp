#include <nvboard.h>
#include <Vaaxor.h>
#include "verilated.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(int argc, char** argv) {
    VerilatedContext* contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    Vaaxor* top = new Vaaxor{contextp};
    nvboard_bind_pin( &top->a, BIND_RATE_SCR, BIND_DIR_IN , 1, SW0);
    nvboard_bind_pin( &top->b, BIND_RATE_SCR, BIND_DIR_IN , 1, SW1);
    nvboard_bind_pin( &top->f, BIND_RATE_SCR, BIND_DIR_OUT, 1, LD0);    
    nvboard_init();
    while (true) {
        nvboard_update();
        top->eval();
    }

    nvboard_quit();
}