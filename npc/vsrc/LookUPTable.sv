module LookUPTable (
    input  logic [PATTERN_LEN-1:0] inst,
    output logic [  MICRO_LEN-1:0] micro_cmd
);
  parameter int unsigned PATTERN_LEN = 8;
  parameter int unsigned MICRO_LEN = 7;
  parameter int unsigned INST_NR = 6;

  // Micro command format: Regen[1], Pcjen[1], Mwen[1], Pcren[1], imm_type[3]
  // localparam bit [9:0]  LUIPattern = 9'b0000110111;
  localparam bit [PATTERN_LEN-1:0] AUIPCPattern = 'b00000101;
  localparam bit [MICRO_LEN-1:0] AUIPCMicro = 'b1001110;
  localparam bit [PATTERN_LEN-1:0] JALPattern = 'b00011011;
  localparam bit [MICRO_LEN-1:0] JALMicro = 'b1101111;
  localparam bit [PATTERN_LEN-1:0] JALRPattern = 'b00011001;
  localparam bit [MICRO_LEN-1:0] JALRMicro = 'b1100001;
  localparam bit [PATTERN_LEN-1:0] SWPattern = 'b01001000;
  localparam bit [MICRO_LEN-1:0] SWMicro = 'b0010000;
  localparam bit [PATTERN_LEN-1:0] ADDIPattern = 'b00000100;
  localparam bit [MICRO_LEN-1:0] ADDIMicro = 'b1000001;
  localparam bit [PATTERN_LEN-1:0] EBREAKPattern = 'b00011100;
  localparam bit [MICRO_LEN-1:0] EBREAKMicro = 'b0000000;

  logic [PATTERN_LEN-1:0] pattern_list[INST_NR];
  logic [  MICRO_LEN-1:0] micro_list  [INST_NR];
  assign pattern_list[0] = AUIPCPattern;
  assign micro_list[0]   = AUIPCMicro;
  assign pattern_list[1] = JALPattern;
  assign micro_list[1]   = JALMicro;
  assign pattern_list[2] = JALRPattern;
  assign micro_list[2]   = JALRMicro;
  assign pattern_list[3] = SWPattern;
  assign micro_list[3]   = SWMicro;
  assign pattern_list[4] = ADDIPattern;
  assign micro_list[4]   = ADDIMicro;
  assign pattern_list[5] = EBREAKPattern;
  assign micro_list[5]   = EBREAKMicro;

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
    $display("inst: %b, micro_cmd: %b, hit: %b", inst, micro_cmd, hit);
  end

endmodule
