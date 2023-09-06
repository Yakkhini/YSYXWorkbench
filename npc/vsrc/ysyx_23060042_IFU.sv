module ysyx_23060042_IFU (
    input clk,
    input [31:0] pc,
    output reg [31:0] inst
);

  import "DPI-C" function int pmem_read(int pc);
  always @(posedge clk) begin
    inst <= pmem_read(pc);
  end

endmodule
