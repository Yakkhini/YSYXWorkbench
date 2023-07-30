// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Symbol table internal header
//
// Internal details; most calling programs do not need this header,
// unless using verilator public meta comments.

#ifndef VERILATED_V__0301_ALU__SYMS_H_
#define VERILATED_V__0301_ALU__SYMS_H_  // guard

#include "verilated.h"

// INCLUDE MODEL CLASS

#include "V__0301_alu.h"

// INCLUDE MODULE CLASSES
#include "V__0301_alu___024root.h"
#include "V__0301_alu___024unit.h"

// SYMS CLASS (contains all model state)
class V__0301_alu__Syms final : public VerilatedSyms {
  public:
    // INTERNAL STATE
    V__0301_alu* const __Vm_modelp;
    bool __Vm_activity = false;  ///< Used by trace routines to determine change occurred
    uint32_t __Vm_baseCode = 0;  ///< Used by trace routines when tracing multiple models
    VlDeleter __Vm_deleter;
    bool __Vm_didInit = false;

    // MODULE INSTANCE STATE
    V__0301_alu___024root          TOP;

    // CONSTRUCTORS
    V__0301_alu__Syms(VerilatedContext* contextp, const char* namep, V__0301_alu* modelp);
    ~V__0301_alu__Syms();

    // METHODS
    const char* name() { return TOP.name(); }
} VL_ATTR_ALIGNED(VL_CACHE_LINE_BYTES);

#endif  // guard
