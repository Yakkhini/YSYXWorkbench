module ysyx_23060042_IDU (
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
