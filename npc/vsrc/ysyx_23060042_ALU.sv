module ysyx_23060042_ALU (
    input  [31:0] data1,
    input  [31:0] data2,
    output [31:0] out
);
  assign out = data1 + data2;
endmodule
