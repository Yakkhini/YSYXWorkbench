/* verilator lint_off UNUSEDSIGNAL */
module acalu(
  input clk,
  input [3:0] x0,
  input [3:0] x1,
  input [2:0] ctr,
  output reg [3:0] result,
  output reg flg
);

  wire [3:0] sub;
  assign sub = x0 + (~x1) + 4'd1;

  always @(*) begin
    result = 4'd0; flg = 0;
    case (ctr)
      3'd0 : result = x0 + x1;
      3'd1 : result = sub;
      3'd2 : result = ~x0;
      3'd3 : result = x0&x1;
      3'd4 : result = x0|x1;
      3'd5 : result = x0^x1;
      3'd6 : flg = sub[3] + ((x0[3]==~x1[3])&&(x0[3]!=sub[3]));
      3'd7 : flg = ~(|sub);
    endcase
  end
endmodule

