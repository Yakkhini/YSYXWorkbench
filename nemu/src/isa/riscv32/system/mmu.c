/***************************************************************************************
 * Copyright (c) 2014-2022 Zihao Yu, Nanjing University
 *
 * NEMU is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan
 * PSL v2. You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 * NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

#include <isa.h>
#include <memory/paddr.h>
#include <memory/vaddr.h>
#include <stdint.h>

int isa_mmu_check(vaddr_t vaddr, int len, int type) {
  if (cpu.csr.satp >> 31) {
    return MMU_TRANSLATE;
  }

  return MMU_DIRECT;
}

paddr_t isa_mmu_translate(vaddr_t vaddr, int len, int type) {
  uint32_t vpn1 = vaddr >> 22;
  uint32_t vpn0 = (vaddr >> 12) & 0x3FF;

  uint32_t root_page_table_addr = cpu.csr.satp << 12;

  uint32_t pte1 = paddr_read(root_page_table_addr + vpn1 * 4, 4);
  uint32_t pte2 = paddr_read(((pte1 >> 10) << 12) + vpn0 * 4, 4);

  return ((pte2 >> 10) << 12) | (vaddr & 0xFFF);
}
