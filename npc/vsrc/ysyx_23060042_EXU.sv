module ysyx_23060042_EXU (
    input  [31:0] rdata1,
    input  [31:0] rdata2,
    input  [11:0] imm12,
    output [31:0] wdata
);

  assign wdata = rdata1 + {20'b0, imm12};

endmodule
