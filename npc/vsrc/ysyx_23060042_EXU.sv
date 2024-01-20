module ysyx_23060042_EXU (
    input  [31:0] rdata1,
    input  [31:0] rdata2,
    input  [11:0] i_imm,
    output [31:0] wdata
);

  ysyx_23060042_ALU ALU (
      .data1(rdata1),
      .data2({20'b0, i_imm}),
      .out  (wdata)
  );

endmodule
