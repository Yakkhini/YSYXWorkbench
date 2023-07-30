// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See V__0301_alu.h for the primary calling header

#ifndef VERILATED_V__0301_ALU___024UNIT_H_
#define VERILATED_V__0301_ALU___024UNIT_H_  // guard

#include "verilated.h"

class V__0301_alu__Syms;

class V__0301_alu___024unit final : public VerilatedModule {
  public:

    // ENUMS (that were declared public)
    enum operation_t {
        add = 1U,
        sub = 2U,
        nop = 0U
    };

    // INTERNAL VARIABLES
    V__0301_alu__Syms* const vlSymsp;

    // CONSTRUCTORS
    V__0301_alu___024unit(V__0301_alu__Syms* symsp, const char* v__name);
    ~V__0301_alu___024unit();
    VL_UNCOPYABLE(V__0301_alu___024unit);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
} VL_ATTR_ALIGNED(VL_CACHE_LINE_BYTES);


#endif  // guard
