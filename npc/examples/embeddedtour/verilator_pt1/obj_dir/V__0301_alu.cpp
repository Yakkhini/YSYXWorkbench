// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "V__0301_alu.h"
#include "V__0301_alu__Syms.h"
#include "verilated_vcd_c.h"

//============================================================
// Constructors

V__0301_alu::V__0301_alu(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new V__0301_alu__Syms(contextp(), _vcname__, this)}
    , clk{vlSymsp->TOP.clk}
    , rst{vlSymsp->TOP.rst}
    , op_in{vlSymsp->TOP.op_in}
    , a_in{vlSymsp->TOP.a_in}
    , b_in{vlSymsp->TOP.b_in}
    , in_valid{vlSymsp->TOP.in_valid}
    , out{vlSymsp->TOP.out}
    , out_valid{vlSymsp->TOP.out_valid}
    , __PVT____024unit{vlSymsp->TOP.__PVT____024unit}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
}

V__0301_alu::V__0301_alu(const char* _vcname__)
    : V__0301_alu(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

V__0301_alu::~V__0301_alu() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void V__0301_alu___024root___eval_debug_assertions(V__0301_alu___024root* vlSelf);
#endif  // VL_DEBUG
void V__0301_alu___024root___eval_static(V__0301_alu___024root* vlSelf);
void V__0301_alu___024root___eval_initial(V__0301_alu___024root* vlSelf);
void V__0301_alu___024root___eval_settle(V__0301_alu___024root* vlSelf);
void V__0301_alu___024root___eval(V__0301_alu___024root* vlSelf);

void V__0301_alu::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate V__0301_alu::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    V__0301_alu___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    vlSymsp->__Vm_activity = true;
    vlSymsp->__Vm_deleter.deleteAll();
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        V__0301_alu___024root___eval_static(&(vlSymsp->TOP));
        V__0301_alu___024root___eval_initial(&(vlSymsp->TOP));
        V__0301_alu___024root___eval_settle(&(vlSymsp->TOP));
    }
    // MTask 0 start
    VL_DEBUG_IF(VL_DBG_MSGF("MTask0 starting\n"););
    Verilated::mtaskId(0);
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    V__0301_alu___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
    Verilated::endOfThreadMTask(vlSymsp->__Vm_evalMsgQp);
    Verilated::endOfEval(vlSymsp->__Vm_evalMsgQp);
}

//============================================================
// Events and timing
bool V__0301_alu::eventsPending() { return false; }

uint64_t V__0301_alu::nextTimeSlot() {
    VL_FATAL_MT(__FILE__, __LINE__, "", "%Error: No delays in the design");
    return 0;
}

//============================================================
// Utilities

const char* V__0301_alu::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void V__0301_alu___024root___eval_final(V__0301_alu___024root* vlSelf);

VL_ATTR_COLD void V__0301_alu::final() {
    V__0301_alu___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* V__0301_alu::hierName() const { return vlSymsp->name(); }
const char* V__0301_alu::modelName() const { return "V__0301_alu"; }
unsigned V__0301_alu::threads() const { return 1; }
std::unique_ptr<VerilatedTraceConfig> V__0301_alu::traceConfig() const {
    return std::unique_ptr<VerilatedTraceConfig>{new VerilatedTraceConfig{false, false, false}};
};

//============================================================
// Trace configuration

void V__0301_alu___024root__trace_init_top(V__0301_alu___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD static void trace_init(void* voidSelf, VerilatedVcd* tracep, uint32_t code) {
    // Callback from tracep->open()
    V__0301_alu___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<V__0301_alu___024root*>(voidSelf);
    V__0301_alu__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    if (!vlSymsp->_vm_contextp__->calcUnusedSigs()) {
        VL_FATAL_MT(__FILE__, __LINE__, __FILE__,
            "Turning on wave traces requires Verilated::traceEverOn(true) call before time 0.");
    }
    vlSymsp->__Vm_baseCode = code;
    tracep->scopeEscape(' ');
    tracep->pushNamePrefix(std::string{vlSymsp->name()} + ' ');
    V__0301_alu___024root__trace_init_top(vlSelf, tracep);
    tracep->popNamePrefix();
    tracep->scopeEscape('.');
}

VL_ATTR_COLD void V__0301_alu___024root__trace_register(V__0301_alu___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD void V__0301_alu::trace(VerilatedVcdC* tfp, int levels, int options) {
    if (tfp->isOpen()) {
        vl_fatal(__FILE__, __LINE__, __FILE__,"'V__0301_alu::trace()' shall not be called after 'VerilatedVcdC::open()'.");
    }
    if (false && levels && options) {}  // Prevent unused
    tfp->spTrace()->addModel(this);
    tfp->spTrace()->addInitCb(&trace_init, &(vlSymsp->TOP));
    V__0301_alu___024root__trace_register(&(vlSymsp->TOP), tfp->spTrace());
}
