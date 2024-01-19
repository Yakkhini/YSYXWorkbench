module ysyx_23060042_IFU (
    input clk,
    input [31:0] pc,
    output reg [31:0] inst
);

  import "DPI-C" function int inst_fetch(int pc);
  assign inst = inst_fetch(pc);

endmodule
