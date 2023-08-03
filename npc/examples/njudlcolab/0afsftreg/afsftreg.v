module afsftreg (
  input [7:0] seed,
  input clk,
  input rst,
  output reg [7:0] bitreg,
  output reg [7:0] hex1,
  output reg [7:0] hex0
);
  always @(posedge clk) begin
    if (rst) begin bitreg <= seed; end
    else begin
      bitreg <= {bitreg[0]^bitreg[2]^bitreg[3]^bitreg[4], bitreg[7:1]};
    end
  end

  toseg instance_1 (bitreg[5:3], hex1);
  toseg instance_2 (bitreg[2:0], hex0);
endmodule

/* verilator lint_off DECLFILENAME */

module toseg(
  input [2:0] d,
  output [7:0] h_out
);
  reg [7:0] h;
  always @(d) begin
    case (d)
      3'd0 : h = 8'b11111101;
      3'd1 : h = 8'b01100000;
      3'd2 : h = 8'b11011010;
      3'd3 : h = 8'b11110010;
      3'd4 : h = 8'b01100110;
      3'd5 : h = 8'b10110110;
      3'd6 : h = 8'b10111110;
      3'd7 : h = 8'b11100000;
    endcase
  end

  assign h_out = ~h;

endmodule

