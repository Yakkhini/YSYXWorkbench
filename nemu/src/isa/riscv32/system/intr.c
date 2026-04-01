/***************************************************************************************
 * Copyright (c) 2014-2022 Zihao Yu, Nanjing University
 *
 * NEMU is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan
 *PSL v2. You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY
 *KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 *NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

#include <isa.h>

#define IRQ_TIMER 0x80000007 // Machine timer interrupt

word_t isa_raise_intr(word_t NO, vaddr_t epc) {
#if CONFIG_ETRACE
  Log("ETRACE: Exception NO = 0x%08x", NO);
#endif

  // Currently only NO.11 "Environment call from M-mode" is
  // used by  ECALL instruction so no switch case is needed.
  cpu.csr.mcause = NO;
  cpu.csr.mepc = epc;

  // Store and Set MIE to 0 to enable machine interrupt
  bool global_mie = (cpu.csr.mstatus & 0x8) >> 0x3;
  cpu.csr.mstatus &= ~0x8;

  // Set MPIE to 1 to disable machine previous interrupt
  cpu.csr.mstatus |= global_mie << 0x7;

  return cpu.csr.mtvec;
}

word_t isa_query_intr() {

  word_t ret = INTR_EMPTY;
  if (cpu.intr && (cpu.csr.mstatus & 0x8)) {
    ret = IRQ_TIMER;
  }

  cpu.intr = false;
  return ret;
}
