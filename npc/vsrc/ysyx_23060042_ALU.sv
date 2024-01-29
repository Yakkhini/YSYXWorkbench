module ysyx_23060042_ALU (
    input  [ 2:0] AluOp,
    input         UnsignArithen,
    input  [31:0] data1,
    input  [31:0] data2,
    output [31:0] out
);

  bit signed [31:0] srdata;
  MuxKey #(2, 1, 32) sr_submux (
      .out(srdata),
      .key(UnsignArithen),
      .lut({1'b0, data1 >> (data2 & 32'h0000001f), 1'b1, $signed(data1) >>> (data2 & 32'h0000001f)})
  );

  MuxKey #(8, 3, 32) alu_mux (
      .out(out),
      .key(AluOp),
      .lut({
        3'b000,
        data1 + data2,
        3'b001,
        data1 - data2,
        3'b010,
        data1 << (data2 & 32'h0000001f),
        3'b011,
        srdata,
        3'b100,
        data1 | data2,
        3'b101,
        data1 ^ data2,
        3'b110,
        data1 & data2,
        3'b111,
        {31'b0, data1 < data2}
      })
  );

endmodule
