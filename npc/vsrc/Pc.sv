module Pc #(
    int WIDTH = 1,
    int RESET_VAL = 0
) (
    input clk,
    input rst,
    input [WIDTH-1:0] din,
    output reg [WIDTH-1:0] dout,
    input wen,
    input Pcjen,
    input Brken
);

  wire [31:0] pcin  /*verilator public*/;

  MuxKeyWithDefault #(2, 1, 32) pcin_mux (
      .out(pcin),
      .key(Pcjen | Brken),
      .default_out(32'h00000000),
      .lut({1'b0, dout + 4, 1'b1, din})
  );

  always @(posedge clk) begin
    if (rst) dout <= RESET_VAL;
    else if (wen) dout <= pcin;
  end
endmodule
