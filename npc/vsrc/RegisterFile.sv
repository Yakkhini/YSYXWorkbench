module RegisterFile #(
    int ADDR_WIDTH = 1,
    int ADDR_COUNT = 1,
    int DATA_WIDTH = 1
) (
    input clk,
    input wen,
    input [DATA_WIDTH-1:0] wdata,
    input [ADDR_WIDTH-1:0] waddr,
    input [ADDR_WIDTH-1:0] raddr1,
    input [ADDR_WIDTH-1:0] raddr2,
    output [DATA_WIDTH-1:0] rdata1,
    output [DATA_WIDTH-1:0] rdata2
);
  reg [DATA_WIDTH-1:0] rf[ADDR_COUNT-1:0];

  assign rdata1 = rf[raddr1];
  assign rdata2 = rf[raddr2];

  always @(posedge clk) begin
    if (wen) rf[waddr] <= wdata;
  end

endmodule
