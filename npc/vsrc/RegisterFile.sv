module RegisterFile #(
    int ADDR_WIDTH = 1,
    int ADDR_COUNT = 1,
    int DATA_WIDTH = 1
) (
    input clk,
    input wen,
    input Jalen,
    input [31:0] pc,
    input [DATA_WIDTH-1:0] wdata,
    input [ADDR_WIDTH-1:0] waddr,
    input [ADDR_WIDTH-1:0] raddr1,
    input [ADDR_WIDTH-1:0] raddr2,
    output [DATA_WIDTH-1:0] rdata1,
    output [DATA_WIDTH-1:0] rdata2,
    output [DATA_WIDTH-1:0] a0
);
  reg [DATA_WIDTH-1:0] rf[ADDR_COUNT-1:0];

  assign rdata1 = rf[raddr1];
  assign rdata2 = rf[raddr2];

  wire [31:0] regin;
  MuxKeyWithDefault #(2, 1, 32) regin_mux (
      .out(regin),
      .key(Jalen),
      .default_out(32'h00000000),
      .lut({1'b0, wdata, 1'b1, pc + 4})
  );

  always @(posedge clk) begin
    if (wen) rf[waddr] <= regin;
  end

endmodule
