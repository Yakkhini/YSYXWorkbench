/***************************************************************************************
* Copyright (c) 2014-2022 Zihao Yu, Nanjing University
*
* NEMU is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#include <isa.h>
#include <stdio.h>
#include <string.h>
#include "debug.h"
#include "local-include/reg.h"

const char *regs[] = {
  "$0", "ra", "sp", "gp", "tp", "t0", "t1", "t2",
  "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
  "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7",
  "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

void isa_reg_display() {
  printf("pc 0x%X\n", cpu.pc);
  printf("Print gpr...\n");
  printf("zero 0x%X;\n", cpu.gpr[0]);
  printf("%s 0x%X;   ", regs[1], cpu.gpr[1]);
  printf("%s 0x%X;   ", regs[2], cpu.gpr[2]);
  printf("%s 0x%X;\n", regs[3], cpu.gpr[3]);
 
  for (int i=4; i<29; i+=4) {
    printf("%s 0x%X;   ", regs[i], cpu.gpr[i]);
    printf("%s 0x%X;   ", regs[i+1], cpu.gpr[i+1]);
    printf("%s 0x%X;   ", regs[i+2], cpu.gpr[i+2]);
    printf("%s 0x%X;\n", regs[i+3], cpu.gpr[i+3]);
  }
}

word_t isa_reg_str2val(const char *s, bool *success) {
  Log("Entered isa reg str2val func...\n");
  if ((strcmp(s, "$0") == 0) || strcmp(s, "$zero") == 0) {
    Log("REG Zero...\n");
    *success = true;
    return 0;
  }
  s += 1;
  for (int i = 1; i < 30; i++) {
    if (strcmp(s, regs[i]) == 0) {
      *success = true;
      Log("Get value %u at %s...\n", cpu.gpr[i], s);
      return cpu.gpr[i];
    }
  }
  return -1;
}
