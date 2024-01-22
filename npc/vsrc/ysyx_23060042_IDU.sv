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
    output Pcren
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

  //Ctr: Regen[1], Pcjen[1], Pcren[1], imm_type[3]
  //imm_type: 000 for R, 001 for I, 010 for S, 011 for SB, 110 for U, 111 for UJ
  wire [5:0] Ctr00;
  wire [5:0] Ctr01;
  wire [5:0] Ctr11;
  wire [5:0] Ctr;

  MuxKeyWithDefault #(3, 3, 6) ctr00_mux (
      .out(Ctr00),
      .key(opcode[4:2]),
      .default_out(6'b000000),
      .lut({3'b000, 6'b100001, 3'b100, 6'b100001, 3'b101, 6'b101110})
  );

  //MuxKeyWithDefault #(2, 3, 1) ctr01_mux (
  //    .out(Ctr01),
  //    .key(inst[4:2]),
  //    .default_out(1'b0),
  //    .lut({})
  //);

  assign Ctr01 = 6'b000010;

  //MuxKeyWithDefault #(2, 3, 1) ctr11_mux (
  //    .out(Ctr011),
  //    .key(inst[4:2]),
  //    .default_out(1'b0),
  //    .lut({})
  //);

  assign Ctr11 = 6'b000010;

  MuxKeyWithDefault #(3, 2, 6) ctr_mux (
      .out(Ctr),
      .key(opcode[6:5]),
      .default_out(6'b000000),
      .lut({2'b00, Ctr00, 2'b01, Ctr01, 2'b11, Ctr11})
  );

  assign Regen = Ctr[5];
  assign Pcjen = Ctr[4];
  assign Pcren = Ctr[3];

  //imm_type: 000 for R, 001 for I, 010 for S, 011 for SB, 110 for U, 111 for UJ
  MuxKeyWithDefault #(6, 3, 32) imm_mux (
      .out(imm),
      .key(Ctr[2:0]),
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

endmodule
