/* verilator lint_off UNUSEDSIGNAL */

module sriz (
    input clk,
    input rst
);
  wire [31:0] pc;
  wire [31:0] pc_next;

  wire Pcjen;
  wire Jalen;
  wire [31:0] wdata;
  Pc #(32, 32'h80000000) pc_reg (
      .clk  (clk),
      .rst  (rst),
      .din  (wdata),
      .dout (pc),
      .wen  (1'b1),
      .Pcjen(Pcjen)
  );

  reg [31:0] inst;

  ysyx_23060042_IFU IFU (
      .clk (clk),
      .pc  (pc),
      .inst(inst)
  );

  wire [6:0] opcode;
  wire [2:0] func3;
  wire Pcren;
  wire Regen;
  wire Mwen;
  wire Brken;
  wire [31:0] imm;
  wire [31:0] a0;
  wire [4:0] rs1, rs2, rd;

  ysyx_23060042_IDU IDU (
      .inst(inst),
      .opcode(opcode),
      .func3(func3),
      .imm(imm),
      .rs1(rs1),
      .rs2(rs2),
      .rd(rd),
      .Regen(Regen),
      .Pcjen(Pcjen),
      .Mwen(Mwen),
      .Pcren(Pcren),
      .Jalen(Jalen),
      .Brken(Brken)
  );

  wire Regen;
  wire [31:0] rdata1;
  wire [31:0] rdata2;

  RegisterFile #(5, 32, 32) resgister_file (
      .clk(clk),
      .wen(Regen),
      .wdata(wdata),
      .waddr(rd),
      .raddr1(rs1),
      .raddr2(rs2),
      .rdata1(rdata1),
      .rdata2(rdata2),
      .Jalen(Jalen),
      .pc(pc),
      .a0(a0)
  );

  ysyx_23060042_EXU EXU (
      .rst(rst),
      .Brken(Brken),
      .a0(a0),
      .imm(imm),
      .Pcren(Pcren),
      .pc(pc),
      .rdata1(rdata1),
      .rdata2(rdata2),
      .wdata(wdata)
  );

endmodule

