module ysyx_23060042_ALU (
    input  [ 2:0] AluOp,
    input  [31:0] data1,
    input  [31:0] data2,
    output [31:0] out
);

  MuxKeyWithDefault #(2, 3, 32) alu_mux (
      .out(out),
      .key(AluOp),
      .default_out(32'h00000000),
      .lut({3'b000, data1 + data2, 3'b001, data1 - data2})
  );

endmodule
