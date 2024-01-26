module LookUPTable (
    input  bit [PATTERN_LEN-1:0] inst,
    output bit [  MICRO_LEN-1:0] micro_cmd
);
  parameter int unsigned PATTERN_LEN = 8;
  parameter int unsigned MICRO_LEN = 10;
  parameter int unsigned INST_NR = 9;

  parameter bit REGEN_TRUE = 1'b1;
  parameter bit REGEN_FALSE = 1'b0;
  parameter bit PCJEN_TRUE = 1'b1;
  parameter bit PCJEN_FALSE = 1'b0;
  parameter bit [1:0] MWEN_BYTE = 2'b01;
  parameter bit [1:0] MWEN_HALF = 2'b10;
  parameter bit [1:0] MWEN_WORD = 2'b11;
  parameter bit [1:0] MWEN_NONE = 2'b00;
  parameter bit PCREN_TRUE = 1'b1;
  parameter bit PCREN_FALSE = 1'b0;
  parameter bit [1:0] MREN_BYTE = 2'b01;
  parameter bit [1:0] MREN_HALF = 2'b10;
  parameter bit [1:0] MREN_WORD = 2'b11;
  parameter bit [1:0] MREN_NONE = 2'b00;
  parameter bit [2:0] IMM_TYPE_NONE = 3'b000;
  parameter bit [2:0] IMM_TYPE_I = 3'b001;
  parameter bit [2:0] IMM_TYPE_S = 3'b010;
  parameter bit [2:0] IMM_TYPE_SB = 3'b011;
  parameter bit [2:0] IMM_TYPE_U = 3'b110;
  parameter bit [2:0] IMM_TYPE_UJ = 3'b111;

  // Micro command format: Regen[1], Pcjen[1], Pcren[1], Mwen[2], Mren[2], imm_type[3]
  // localparam bit [9:0]  LUIPattern = 9'b0000110111;
  localparam bit [PATTERN_LEN-1:0] AUIPCPattern = {3'b000, 5'b00101};
  localparam bit [MICRO_LEN-1:0] AUIPCMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_TRUE, MWEN_NONE, MREN_NONE, IMM_TYPE_U
  };
  localparam bit [PATTERN_LEN-1:0] JALPattern = {3'b000, 5'b11011};
  localparam bit [MICRO_LEN-1:0] JALMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_TRUE, MWEN_NONE, MREN_NONE, IMM_TYPE_UJ
  };
  localparam bit [PATTERN_LEN-1:0] JALRPattern = {3'b000, 5'b11001};
  localparam bit [MICRO_LEN-1:0] JALRMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_FALSE, MWEN_NONE, MREN_NONE, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LBPattern = {3'b000, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LBMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_BYTE, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LHPattern = {3'b001, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LHMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_HALF, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LWPattern = {3'b010, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LWMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_WORD, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SWPattern = {3'b010, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SWMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_WORD, MREN_NONE, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] ADDIPattern = {3'b000, 5'b00100};
  localparam bit [MICRO_LEN-1:0] ADDIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] EBREAKPattern = {3'b000, 5'b11100};
  localparam bit [MICRO_LEN-1:0] EBREAKMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, IMM_TYPE_NONE
  };

  bit [PATTERN_LEN-1:0] pattern_list[INST_NR];
  bit [  MICRO_LEN-1:0] micro_list  [INST_NR];
  assign pattern_list[0] = AUIPCPattern;
  assign micro_list[0]   = AUIPCMicro;
  assign pattern_list[1] = JALPattern;
  assign micro_list[1]   = JALMicro;
  assign pattern_list[2] = JALRPattern;
  assign micro_list[2]   = JALRMicro;
  assign pattern_list[3] = LBPattern;
  assign micro_list[3]   = LBMicro;
  assign pattern_list[4] = LHPattern;
  assign micro_list[4]   = LHMicro;
  assign pattern_list[5] = LWPattern;
  assign micro_list[5]   = LWMicro;
  assign pattern_list[6] = SWPattern;
  assign micro_list[6]   = SWMicro;
  assign pattern_list[7] = ADDIPattern;
  assign micro_list[7]   = ADDIMicro;
  assign pattern_list[8] = EBREAKPattern;
  assign micro_list[8]   = EBREAKMicro;

  reg hit;
  reg [2:0] func3;
  reg [PATTERN_LEN-1:0] lut_inst;
  always_comb begin : lookup_micro
    micro_cmd = 'b0000000;
    hit = 0;
    for (integer i = 0; i < INST_NR; i = i + 1) begin
      func3 = {3{pattern_list[i][7] | pattern_list[i][6] | pattern_list[i][5]}};
      lut_inst = {inst[7:5] & func3, inst[4:0]};
      micro_cmd = micro_cmd | ({MICRO_LEN{lut_inst == pattern_list[i]}} & micro_list[i]);
      hit = hit | (lut_inst == pattern_list[i]);
    end
    if (!hit) begin
      $display("Error: No micro command found for inst: %b", inst);
    end
    // $display("inst: %b, micro_cmd: %b, hit: %b", inst, micro_cmd, hit);
  end

endmodule
