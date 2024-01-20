module ysyx_23060042_IDU (
    input  [31:0] inst,
    output [ 6:0] opcode,
    output [ 2:0] func3,
    output [12:0] b_imm,
    output [19:0] u_imm,
    output [11:0] i_imm,
    output [ 4:0] rs1,
    output [ 4:0] rs2,
    output [ 4:0] rd
);

  assign opcode = inst[6:0];
  assign func3 = inst[14:12];
  assign b_imm = {inst[31], inst[7], inst[30:25], inst[11:8], 1'b0};
  assign u_imm = inst[31:12];
  assign i_imm = inst[31:20];

  assign rs1 = inst[19:15];
  assign rs2 = inst[24:20];
  assign rd = inst[11:7];

endmodule
