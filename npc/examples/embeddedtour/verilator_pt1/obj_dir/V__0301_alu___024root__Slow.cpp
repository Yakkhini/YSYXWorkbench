// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See V__0301_alu.h for the primary calling header

#include "verilated.h"

#include "V__0301_alu__Syms.h"
#include "V__0301_alu___024root.h"

void V__0301_alu___024root___ctor_var_reset(V__0301_alu___024root* vlSelf);

V__0301_alu___024root::V__0301_alu___024root(V__0301_alu__Syms* symsp, const char* v__name)
    : VerilatedModule{v__name}
    , vlSymsp{symsp}
 {
    // Reset structure values
    V__0301_alu___024root___ctor_var_reset(this);
}

void V__0301_alu___024root::__Vconfigure(bool first) {
    if (false && first) {}  // Prevent unused
}

V__0301_alu___024root::~V__0301_alu___024root() {
}
