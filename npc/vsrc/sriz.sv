/* verilator lint_off UNUSEDSIGNAL */

module sriz (
    input clk,
    input rst
);
  wire [31:0] pc, pc_next;
  Reg #(32, 32'h80000000) pc_reg (
      .clk (clk),
      .rst (rst),
      .din (pc_next),
      .dout(pc),
      .wen (1'b1)
  );

  assign pc_next = pc + 4;

  reg [31:0] inst;

  ysyx_23060042_IFU IFU (
      .clk (clk),
      .pc  (pc),
      .inst(inst)
  );

  wire [ 6:0] opcode;
  wire [ 2:0] func3;
  wire [11:0] imm12;
  wire [4:0] rs1, rs2, rd;

  ysyx_23060042_IDU IDU (
      .inst(inst),
      .opcode(opcode),
      .func3(func3),
      .imm12(imm12),
      .rs1(rs1),
      .rs2(rs2),
      .rd(rd)
  );

  wire register_wen;
  wire [31:0] wdata;
  wire [31:0] rdata1;
  wire [31:0] rdata2;
  assign register_wen = 1'b1;

  RegisterFile #(5, 32, 32) resgister_file (
      .clk(clk),
      .wen(register_wen),
      .wdata(wdata),
      .waddr(rd),
      .raddr1(rs1),
      .raddr2(rs2),
      .rdata1(rdata1),
      .rdata2(rdata2)
  );

  ysyx_23060042_EXU EXU (
      .imm12 (imm12),
      .rdata1(rdata1),
      .rdata2(rdata2),
      .wdata (wdata)
  );

endmodule

