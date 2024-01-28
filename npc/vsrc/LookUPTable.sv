module LookUPTable (
    input  bit [PATTERN_LEN-1:0] inst,
    output bit [  MICRO_LEN-1:0] micro_cmd
);
  parameter int unsigned PATTERN_LEN = 15;
  parameter int unsigned MICRO_LEN = 13;
  parameter int unsigned INST_NR = 15;

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
  parameter bit [2:0] ALUOP_ADD = 3'b000;
  parameter bit [2:0] ALUOP_SUB = 3'b001;
  parameter bit [2:0] ALUOP_SL = 3'b010;
  parameter bit [2:0] ALUOP_SR = 3'b011;
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

  // Micro command format: [12]REGEN [11]PCJEN [10]PCREN [9:8]MWEN [7:6]MREN [5:3]ALUOP [2:0]IMM_TYPE
  localparam bit [PATTERN_LEN-1:0] LUIPattern = {7'b0000000, 3'b000, 5'b01101};
  localparam bit [MICRO_LEN-1:0] LUIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_U
  };
  localparam bit [PATTERN_LEN-1:0] AUIPCPattern = {7'b0000000, 3'b000, 5'b00101};
  localparam bit [MICRO_LEN-1:0] AUIPCMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_U
  };
  localparam bit [PATTERN_LEN-1:0] JALPattern = {7'b0000000, 3'b000, 5'b11011};
  localparam bit [MICRO_LEN-1:0] JALMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_TRUE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_UJ
  };
  localparam bit [PATTERN_LEN-1:0] JALRPattern = {7'b0000000, 3'b000, 5'b11001};
  localparam bit [MICRO_LEN-1:0] JALRMicro = {
    REGEN_TRUE, PCJEN_TRUE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LBPattern = {7'b0000000, 3'b000, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LBMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_BYTE, ALUOP_ADD, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LHPattern = {7'b0000000, 3'b001, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LHMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_HALF, ALUOP_ADD, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] LWPattern = {7'b0000000, 3'b010, 5'b00000};
  localparam bit [MICRO_LEN-1:0] LWMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_WORD, ALUOP_ADD, IMM_TYPE_I
  };
  localparam bit [PATTERN_LEN-1:0] SBPattern = {7'b0000000, 3'b000, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SBMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_BYTE, MREN_NONE, ALUOP_ADD, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] SHPattern = {7'b0000000, 3'b001, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SHMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_HALF, MREN_NONE, ALUOP_ADD, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] SWPattern = {7'b0000000, 3'b010, 5'b01000};
  localparam bit [MICRO_LEN-1:0] SWMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_WORD, MREN_NONE, ALUOP_ADD, IMM_TYPE_S
  };
  localparam bit [PATTERN_LEN-1:0] ADDIPattern = {7'b0000000, 3'b000, 5'b00100};
  localparam bit [MICRO_LEN-1:0] ADDIMicro = {
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_I
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
    REGEN_TRUE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_SUB, IMM_TYPE_NONE
  };
  localparam bit [PATTERN_LEN-1:0] EBREAKPattern = {7'b0000000, 3'b000, 5'b11100};
  localparam bit [MICRO_LEN-1:0] EBREAKMicro = {
    REGEN_FALSE, PCJEN_FALSE, PCREN_FALSE, MWEN_NONE, MREN_NONE, ALUOP_ADD, IMM_TYPE_NONE
  };

  bit [PATTERN_LEN-1:0] pattern_list[INST_NR];
  bit [  MICRO_LEN-1:0] micro_list  [INST_NR];
  initial begin
    pattern_list[0] = LUIPattern;
    pattern_list[1] = AUIPCPattern;
    pattern_list[2] = JALPattern;
    pattern_list[3] = JALRPattern;
    pattern_list[4] = LBPattern;
    pattern_list[5] = LHPattern;
    pattern_list[6] = LWPattern;
    pattern_list[7] = SBPattern;
    pattern_list[8] = SHPattern;
    pattern_list[9] = SWPattern;
    pattern_list[10] = ADDIPattern;
    pattern_list[11] = SLTIPattern;
    pattern_list[12] = SLTIUPattern;
    pattern_list[13] = SUBPattern;
    pattern_list[14] = EBREAKPattern;

    micro_list[0] = LUIMicro;
    micro_list[1] = AUIPCMicro;
    micro_list[2] = JALMicro;
    micro_list[3] = JALRMicro;
    micro_list[4] = LBMicro;
    micro_list[5] = LHMicro;
    micro_list[6] = LWMicro;
    micro_list[7] = SBMicro;
    micro_list[8] = SHMicro;
    micro_list[9] = SWMicro;
    micro_list[10] = ADDIMicro;
    micro_list[11] = SLTIMicro;
    micro_list[12] = SLTIUMicro;
    micro_list[13] = SUBMicro;
    micro_list[14] = EBREAKMicro;
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

      // $display("lut_inst: %b, pattern_list[%0d]: %b, micro_list[%0d]: %b", lut_inst, i,
      //          pattern_list[i], i, micro_list[i]);
    end
  end

endmodule
