module ysyx_23060042_IFU (
    input rst,
    input [31:0] pc,
    output bit [31:0] inst
);

  import "DPI-C" function int inst_fetch(int pc);

  always_comb begin
    if (rst) begin
      inst = 0;
    end else begin
      inst = inst_fetch(pc);
    end
  end

endmodule
