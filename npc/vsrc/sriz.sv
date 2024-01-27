/* verilator lint_off UNUSEDSIGNAL */

module sriz (
    input clk,
    input rst
);
  bit [31:0] pc;
  bit [31:0] pc_next;

  bit Pcjen;
  bit Jalen;
  bit [31:0] wdata;
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
      .rst (rst),
      .clk (clk),
      .pc  (pc),
      .inst(inst)
  );

  bit [6:0] opcode;
  bit [2:0] func3;
  bit Pcren;
  bit Regen;
  bit [1:0] Mwen;
  bit [1:0] Mren;
  bit Brken;
  bit [31:0] imm;
  bit [31:0] a0;
  bit [4:0] rs1, rs2, rd;

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
      .Mren(Mren),
      .Jalen(Jalen),
      .Brken(Brken)
  );

  bit [31:0] rdata1;
  bit [31:0] rdata2;

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

  bit [31:0] mwdata;
  bit [31:0] mrdata;

  ysyx_23060042_EXU EXU (
      .rst(rst),
      .Brken(Brken),
      .a0(a0),
      .imm(imm),
      .Pcren(Pcren),
      .Mren(Mren),
      .pc(pc),
      .rdata1(rdata1),
      .rdata2(rdata2),
      .mrdata(mrdata),
      .wdata(wdata)
  );

  Memory mem (
      .rst  (rst),
      .Mwen (Mwen),
      .Mren (Mren),
      .waddr(rdata1 + imm),
      .raddr(rdata1 + imm),
      .wdata(rdata2),
      .rdata(mrdata)
  );

endmodule

