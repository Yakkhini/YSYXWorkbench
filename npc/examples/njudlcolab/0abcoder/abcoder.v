module abcoder(
  input [7:0] sw,
  output reg pointld,
  output reg [7:0] hex,
  output reg [2:0] num
);
  reg [7:0] ihex;
  integer i;
  always @(sw) begin
    num = 3'b000;
    pointld = 1'b0;
    for(i = 0; i<=7; i = i+1)
      if(sw[i] == 1) begin num = i[2:0]; pointld = 1; end
  end

  bcd7seg instance_1 (num, ihex);
  assign hex = ~ihex;
  
endmodule

/* verilator lint_off DECLFILENAME */

module bcd7seg(
  input [2:0] d,
  output reg [7:0] h
);
  always @(*) begin
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

endmodule

