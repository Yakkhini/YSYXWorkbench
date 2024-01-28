module ysyx_23060042_ALU (
    input  [ 2:0] AluOp,
    input  [31:0] data1,
    input  [31:0] data2,
    output [31:0] out
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
        data1 << data2,
        3'b011,
        data1 >> data2,
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
