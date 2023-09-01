/* verilator lint_off DECLFILENAME */
/* verilator lint_off UNUSEDSIGNAL */
module sriz (
    input clk
);
  reg  [31:0] pc = 0;  // pc

  reg  [31:0] x0;  // zero

  reg  [31:0] x1;  // ra
  reg  [31:0] x2;  // sp
  reg  [31:0] x3;  // gp
  reg  [31:0] x4;  // tp

  reg  [31:0] x5;  // t0
  reg  [31:0] x6;  // t1
  reg  [31:0] x7;  // t2

  reg  [31:0] x8;  // fp
  reg  [31:0] x9;  // s1

  reg  [31:0] x10;  // a0
  reg  [31:0] x11;  // a1

  wire [31:0] inst;
  wire [ 6:0] opcode;
  wire [ 2:0] func3;
  wire [11:0] imm12;
  wire [4:0] rs1, rs2, rd;

  ysyx_22060042_IFU IFU (
      pc,
      inst
  );
  ysyx_22060042_IDU IDU (
      inst,
      opcode,
      func3,
      imm12,
      rs1,
      rs2,
      rd
  );
  ysyx_22060042_EXU EXU ();

endmodule

module ysyx_22060042_IFU (
    input  [31:0] pc,
    output [31:0] inst
);

  import "DPI-C" function int pmem_read(int pc);

  assign inst = pmem_read(pc);

endmodule

module ysyx_22060042_IDU (
    input  [31:0] inst,
    output [ 6:0] opcode,
    output [ 2:0] func3,
    output [11:0] imm12,
    output [ 4:0] rs1,
    output [ 4:0] rs2,
    output [ 4:0] rd
);

  assign opcode = inst[6:0];
  assign func3 = inst[14:12];
  assign imm12 = inst[31:20];

  assign rs1 = inst[19:15];
  assign rs2 = inst[24:20];
  assign rd = inst[11:7];

endmodule

module ysyx_22060042_EXU ();
endmodule
