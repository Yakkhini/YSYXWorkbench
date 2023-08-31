/* verilator lint_off DECLFILENAME */
/* verilator lint_off UNUSEDSIGNAL */
module sriz(
  input [31:0] inst,
  input reg [31:0] pc
);
  ysyx_22060042_IFU IFU ();
  ysyx_22060042_IFU IDU ();
  ysyx_22060042_IFU EXU ();
endmodule

module ysyx_22060042_IFU();
endmodule

module ysyx_22060042_IDU();
endmodule

module ysyx_22060042_EXU();
endmodule
