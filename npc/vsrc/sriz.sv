/* verilator lint_off UNUSEDSIGNAL */

module sriz (
    input clk,
    input rst
);
  wire [31:0] pc, pc_next;
  wire Pcjen;
  Pc #(32, 32'h80000000) pc_reg (
      .clk  (clk),
      .rst  (rst),
      .din  (pc_next),
      .dout (pc),
      .wen  (1'b1),
      .Pcjen(Pcjen)
  );

  assign pc_next = pc + 4;

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
  wire [31:0] imm;
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
      .Pcren(Pcren),
      .Pcjen(Pcjen)
  );

  wire Regen;
  wire [31:0] wdata;
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
      .rdata2(rdata2)
  );

  ysyx_23060042_EXU EXU (
      .imm(imm),
      .Pcren(Pcren),
      .pc(pc),
      .rdata1(rdata1),
      .rdata2(rdata2),
      .wdata(wdata)
  );

endmodule

