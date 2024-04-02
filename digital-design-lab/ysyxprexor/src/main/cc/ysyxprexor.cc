#include <nvboard.h>
#include <Vysyxprexor.h>
#include "verilated.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(int argc, char** argv) {
    VerilatedContext* contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);
    Vysyxprexor* top = new Vysyxprexor{contextp};
    nvboard_bind_pin( &top->io_a, BIND_RATE_SCR, BIND_DIR_IN , 1, SW0);
    nvboard_bind_pin( &top->io_b, BIND_RATE_SCR, BIND_DIR_IN , 1, SW1);
    nvboard_bind_pin( &top->io_f, BIND_RATE_SCR, BIND_DIR_OUT, 1, LD0);    
    nvboard_init();
    while (true) {
        nvboard_update();
        top->eval();
    }

    nvboard_quit();
}