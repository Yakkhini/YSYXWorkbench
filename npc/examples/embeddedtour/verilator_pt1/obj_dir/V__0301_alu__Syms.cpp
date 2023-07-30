// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Symbol table implementation internals

#include "V__0301_alu__Syms.h"
#include "V__0301_alu.h"
#include "V__0301_alu___024root.h"
#include "V__0301_alu___024unit.h"

// FUNCTIONS
V__0301_alu__Syms::~V__0301_alu__Syms()
{
}

V__0301_alu__Syms::V__0301_alu__Syms(VerilatedContext* contextp, const char* namep, V__0301_alu* modelp)
    : VerilatedSyms{contextp}
    // Setup internal state of the Syms class
    , __Vm_modelp{modelp}
    // Setup module instances
    , TOP{this, namep}
{
    // Configure time unit / time precision
    _vm_contextp__->timeunit(-12);
    _vm_contextp__->timeprecision(-12);
    // Setup each module's pointers to their submodules
    // Setup each module's pointer back to symbol table (for public functions)
    TOP.__Vconfigure(true);
}
