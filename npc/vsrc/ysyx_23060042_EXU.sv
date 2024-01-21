module ysyx_23060042_EXU (
    input [31:0] pc,
    input [31:0] rdata1,
    input [31:0] rdata2,
    input [31:0] imm,
    input Pcren,
    output [31:0] wdata
);

  wire [31:0] src1;

  MuxKeyWithDefault #(2, 1, 32) src1_mux (
      .out(src1),
      .key(Pcren),
      .default_out(32'h00000000),
      .lut({1'b0, rdata1, 1'b1, pc})
  );

  ysyx_23060042_ALU ALU (
      .data1(src1),
      .data2(imm),
      .out  (wdata)
  );

endmodule
