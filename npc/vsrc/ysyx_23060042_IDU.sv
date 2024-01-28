module ysyx_23060042_IDU (
    input  [31:0] inst,
    output [ 6:0] opcode,
    output [ 2:0] func3,
    output [31:0] imm,
    output [ 4:0] rs1,
    output [ 4:0] rs2,
    output [ 4:0] rd,

    output Regen,
    output Pcjen,
    output Pcren,
    output [1:0] Mwen,
    output [1:0] Mren,
    output [2:0] AluOp,
    output Unsignen,
    output IMMen,
    output BrchOP,
    output Jalen,
    output Brken
);

  assign opcode = inst[6:0];
  assign func3  = inst[14:12];

  wire [31:0] i_imm;
  wire [31:0] s_imm;
  wire [31:0] b_imm;
  wire [31:0] u_imm;
  wire [31:0] j_imm;
  assign i_imm = {inst[31], {20{inst[31]}}, inst[30:20]};
  assign s_imm = {inst[31], {20{inst[31]}}, inst[30:25], inst[11:7]};
  assign b_imm = {inst[31], {19{inst[31]}}, inst[7], inst[30:25], inst[11:8], 1'b0};
  assign u_imm = {inst[31], inst[30:12], 12'b0};
  assign j_imm = {inst[31], {11{inst[31]}}, u_imm[19:12], i_imm[0], i_imm[10:1], 1'b0};

  assign rs1 = inst[19:15];
  assign rs2 = inst[24:20];
  assign rd = inst[11:7];

  // Micro command format: [13]REGEN [12]PCJEN [11]PCREN [10:9]MWEN [8:7]MREN [6:4]ALUOP [3]UNSIGN [2:0]IMM_TYPE
  // IMM Type: 000 for R, 001 for I, 010 for S, 011 for SB, 110 for U, 111 for UJ
  parameter int unsigned MICRO_LEN = 14;
  wire [MICRO_LEN-1:0] micro_cmd;
  LookUPTable lut (
      .inst({inst[31:25], inst[14:12], inst[6:2]}),
      .micro_cmd(micro_cmd)
  );

  assign Regen = micro_cmd[13];
  assign Pcjen = micro_cmd[12];
  assign Pcren = micro_cmd[11];
  assign Mwen = micro_cmd[10:9];
  assign Mren = micro_cmd[8:7];
  assign AluOp = micro_cmd[6:4];
  assign Unsignen = micro_cmd[3];
  assign Jalen = Regen & Pcjen;
  assign Brken = !(Regen | Pcjen | Mwen[1] | Mwen[0]);

  //imm_type: 000 for R, 001 for I, 010 for S, 011 for SB, 110 for U, 111 for UJ
  MuxKeyWithDefault #(6, 3, 32) imm_mux (
      .out(imm),
      .key(micro_cmd[2:0]),  // imm_type signal in micro_cmd
      .default_out(32'h00000000),
      .lut({
        3'b000,
        32'h00000000,
        3'b001,
        i_imm,
        3'b010,
        s_imm,
        3'b011,
        b_imm,
        3'b110,
        u_imm,
        3'b111,
        j_imm
      })
  );

  MuxKeyWithDefault #(1, 3, 1) BrchOP_mux (
      .out(BrchOP),
      .key(micro_cmd[2:0]),  // Only B type instruction has branch condition
      .default_out(1'b0),
      .lut({3'b011, 1'b1})
  );

  MuxKeyWithDefault #(1, 3, 1) IMMen_mux (
      .out(IMMen),
      .key(micro_cmd[2:0]),  // IMM None type instruction no need immediate value
      .default_out(1'b1),
      .lut({3'b000, 1'b0})
  );

endmodule
