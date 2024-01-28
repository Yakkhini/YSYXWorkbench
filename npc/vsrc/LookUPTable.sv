module LookUPTable (
    input  bit [PATTERN_LEN-1:0] inst,
    output bit [  MICRO_LEN-1:0] micro_cmd
);
  parameter int unsigned PATTERN_LEN = 15;
  parameter int unsigned MICRO_LEN = 13;
  parameter int unsigned INST_NR = 17;

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
  parameter bit [2:0] ALUOP_ADD_BEQ = 3'b000;
  parameter bit [2:0] ALUOP_SUB_BNE = 3'b001;
  parameter bit [2:0] ALUOP_SL_BLT = 3'b010;
  parameter bit [2:0] ALUOP_SR_BGE = 3'b011;
  parameter bit [2:0] ALUOP_OR = 3'b100;
  parameter bit [2:0] ALUOP_XOR = 3'b101;
  parameter bit [2:0] ALUOP_AND = 3'b110;
  parameter bit [2:0] ALUOP_LESS = 3'b111;
  parameter bit [2:0] IMM_TYPE_NONE = 3'b000;
  parameter bit [2:0] IMM_TYPE_I = 3'b001;
  parameter bit [2:0] IMM_TYPE_S = 3'b010;
  parameter bit [2:0] IMM_TYPE_SB = 3'b011;
  parameter bit [2:0] IMM_TYPE_U = 3'b110;
  parameter bit [2:0] IMM_TYPE_UJ = 3'b111;

  // Micro command format: [12]REGEN [11]PCJEN [10]PCREN [9:8]MWEN [7:6]MREN [5:3]ALUOP(BRCHCONDI) [2:0]IMM_TYPE
  localparam bit [PATTERN_LEN-1:0] LUIPattern = {7'b0000000, 3'b000, 5'b01101};
  localparam bit [MICRO_LEN-1:0] LUIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_U
  };
  localparam bit [PATTERN_LEN-1:0] AUIPCPattern = {7'b0000000, 3'b000, 5'b00101};
  localparam bit [MICRO_LEN-1:0] AUIPCMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_U
  };
  localparam bit [PATTERN_LEN-1:0] JALPattern = {7'b0000000, 3'b000, 5'b11011};
  localparam bit [MICRO_LEN-1:0] JALMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_UJ
  };
  localparam bit [PATTERN_LEN-1:0] JALRPattern = {7'b0000000, 3'b000, 5'b11001};
  localparam bit [MICRO_LEN-1:0] JALRMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] BEQPattern = {7'b0000000, 3'b000, 5'b11000};
  localparam bit [MICRO_LEN-1:0] BEQMicro = {
    REGEN_FALSE, PCJEN_TRUE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_SB
  };
  localparam bit [PATTERN_LEN-1:0] BNEPattern = {7'b0000000, 3'b001, 5'b11000};
  localparam bit [MICRO_LEN-1:0] BNEMicro = {
    REGEN_FALSE, PCJEN_TRUE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_SUB_BNE, IMM_TYPE_SB
  };
  localparam bit [PATTERN_LEN-1:0] LBPattern = {7'b0000000, 3'b000, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LBMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_BYTE, ALUOP_ADD_BEQ, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LHPattern = {7'b0000000, 3'b001, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LHMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_HALF, ALUOP_ADD_BEQ, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LWPattern = {7'b0000000, 3'b010, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LWMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_WORD, ALUOP_ADD_BEQ, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SBPattern = {7'b0000000, 3'b000, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SBMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_BYTE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] SHPattern = {7'b0000000, 3'b001, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SHMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_HALF, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] SWPattern = {7'b0000000, 3'b010, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SWMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_WORD, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] ADDIPattern = {7'b0000000, 3'b000, 5'b00100};
  localparam bit [MICRO_LEN-1:0] ADDIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SLTIPattern = {7'b0000000, 3'b010, 5'b00100};
  localparam bit [MICRO_LEN-1:0] SLTIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_LESS, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SLTIUPattern = {7'b0000000, 3'b011, 5'b00100};
  localparam bit [MICRO_LEN-1:0] SLTIUMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_LESS, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SUBPattern = {7'b0100000, 3'b000, 5'b01100};
  localparam bit [MICRO_LEN-1:0] SUBMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_SUB_BNE, IMM_TYPE_NONE
  };
  localparam bit [PATTERN_LEN-1:0] EBREAKPattern = {7'b0000000, 3'b000, 5'b11100};
  localparam bit [MICRO_LEN-1:0] EBREAKMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD_BEQ, IMM_TYPE_NONE
  };

  bit [PATTERN_LEN-1:0] pattern_list[INST_NR];
  bit [  MICRO_LEN-1:0] micro_list  [INST_NR];
  initial begin
    pattern_list[0] = LUIPattern;
    micro_list[0] = LUIMicro;
    pattern_list[1] = AUIPCPattern;
    micro_list[1] = AUIPCMicro;
    pattern_list[2] = JALPattern;
    micro_list[2] = JALMicro;
    pattern_list[3] = JALRPattern;
    micro_list[3] = JALRMicro;
    pattern_list[4] = BEQPattern;
    micro_list[4] = BEQMicro;
    pattern_list[5] = BNEPattern;
    micro_list[5] = BNEMicro;
    pattern_list[6] = LBPattern;
    micro_list[6] = LBMicro;
    pattern_list[7] = LHPattern;
    micro_list[7] = LHMicro;
    pattern_list[8] = LWPattern;
    micro_list[8] = LWMicro;
    pattern_list[9] = SBPattern;
    micro_list[9] = SBMicro;
    pattern_list[10] = SHPattern;
    micro_list[10] = SHMicro;
    pattern_list[11] = SWPattern;
    micro_list[11] = SWMicro;
    pattern_list[12] = ADDIPattern;
    micro_list[12] = ADDIMicro;
    pattern_list[13] = SLTIPattern;
    micro_list[13] = SLTIMicro;
    pattern_list[14] = SLTIUPattern;
    micro_list[14] = SLTIUMicro;
    pattern_list[15] = SUBPattern;
    micro_list[15] = SUBMicro;
    pattern_list[16] = EBREAKPattern;
    micro_list[16] = EBREAKMicro;
  end

  import "DPI-C" function void halt(int code);

  bit hit  /*verilator public*/;
  bit [2:0] func3;
  bit [6:0] func7;
  bit [PATTERN_LEN-1:0] lut_inst;
  always_comb begin : lookup_micro
    micro_cmd = 0;
    hit = 0;
    for (integer i = 0; i < INST_NR; i = i + 1) begin
      func3 = {3{!(micro_list[i][2] & micro_list[i][1])}};  // U & J Type no need func3
      func7 = {7{micro_list[i][2:0] == IMM_TYPE_NONE}};  // Only no IMM Type need func7
      lut_inst = {inst[14:8] & func7, inst[7:5] & func3, inst[4:0]};
      micro_cmd = micro_cmd | ({MICRO_LEN{lut_inst == pattern_list[i]}} & micro_list[i]);
      hit = hit | (lut_inst == pattern_list[i]);
    end
    if (hit) begin
      $display("Micro command hit: %b, inst: %b, micro_cmd: %b", hit, inst, micro_cmd);
    end
  end

endmodule
