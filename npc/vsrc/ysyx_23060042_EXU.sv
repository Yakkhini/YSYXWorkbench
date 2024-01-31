module ysyx_23060042_EXU (
    input rst,
    input [31:0] pc,
    input [2:0] AluOp,
    input [31:0] rdata1,
    input [31:0] rdata2,
    input [31:0] mrdata,
    output [31:0] wdata,
    input [31:0] imm,
    input Pcren,
    input IMMen,
    input UnsignArithen,
    input Brken,
    input BrchOP,
    input [1:0] Mren,
    input [31:0] a0,
    output Brchen
);

  bit [31:0] data1;
  bit [31:0] data2;
  bit [31:0] alu_result;

  MuxKeyWithDefault #(2, 1, 32) data1_mux (
      .out(data1),
      .key(Pcren),
      .default_out(32'h00000000),
      .lut({1'b0, rdata1, 1'b1, pc})
  );

  MuxKey #(2, 1, 32) data2_mux (
      .out(data2),
      .key({IMMen}),
      .lut({1'b0, rdata2, 1'b1, imm})
  );

  ysyx_23060042_ALU ALU (
      .AluOp(AluOp),
      .UnsignArithen(UnsignArithen),
      .data1(data1),
      .data2(data2),
      .out(alu_result)
  );

  MuxKeyWithDefault #(4, 3, 1) brch_mux (
      .out(Brchen),
      .key(AluOp),
      .default_out(1'b0),
      .lut({
        3'b000,
        (rdata1 == rdata2) & BrchOP,
        3'b001,
        (rdata1 != rdata2) & BrchOP,
        3'b100,
        Brch_greater_less_en,
        3'b101,
        Brch_greater_less_en
      })
  );

  bit Brch_greater_less_en;
  MuxKeyWithDefault #(4, 4, 1) brch_greater_less_mux (
      .out(Brch_greater_less_en),
      .key({AluOp, UnsignArithen}),
      .default_out(1'b0),
      .lut({
        4'b1000,
        ($signed(rdata1) < $signed(rdata2)) & BrchOP,
        4'b1010,
        ($signed(rdata1) >= $signed(rdata2)) & BrchOP,
        4'b1001,
        ($unsigned(rdata1) < $unsigned(rdata2)) & BrchOP,
        4'b1011,
        ($unsigned(rdata1) >= $unsigned(rdata2)) & BrchOP
      })
  );

  MuxKeyWithDefault #(3, 2, 32) wdata_mux (
      .out(wdata),
      .key({Mren[1] | Mren[0], BrchOP}),
      .default_out(32'h00000000),
      .lut({2'b00, alu_result, 2'b10, mrdata, 2'b01, pc + imm})
  );

  // synopsys translate_off
  import "DPI-C" function void halt(input int code);

  always_comb begin
    if (Brken & !rst) begin
      halt(a0);
    end
  end
  // synopsys translate_on

endmodule
