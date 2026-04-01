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

word_t vaddr_ifetch(vaddr_t addr, int len) {
  IFDEF(CONFIG_MTRACE, Log("Read memory 0x%X for %i len.", addr, len));
  if (isa_mmu_check(addr, len, MEM_TYPE_READ) == MMU_DIRECT) {
    return paddr_read(addr, len);
  }
  IFDEF(CONFIG_MTRACE, Log("Needs address translation."));
  paddr_t addr_translated = isa_mmu_translate(addr, len, MEM_TYPE_READ);
  IFDEF(CONFIG_MTRACE, Log("Translated address: 0x%X", addr_translated));

  return paddr_read(addr_translated, len);
}

word_t vaddr_read(vaddr_t addr, int len) {
  IFDEF(CONFIG_MTRACE, Log("Read memory 0x%X for %i len.", addr, len));
  if (isa_mmu_check(addr, len, MEM_TYPE_READ) == MMU_DIRECT) {
    return paddr_read(addr, len);
  }
  IFDEF(CONFIG_MTRACE, Log("Needs address translation."));
  paddr_t addr_translated = isa_mmu_translate(addr, len, MEM_TYPE_READ);
  IFDEF(CONFIG_MTRACE, Log("Translated address: 0x%X", addr_translated));

  return paddr_read(addr_translated, len);
}

void vaddr_write(vaddr_t addr, int len, word_t data) {
  IFDEF(CONFIG_MTRACE,
        Log("Write memory 0x%X for %i len. Data: 0x%X", addr, len, data));
  if (isa_mmu_check(addr, len, MEM_TYPE_WRITE) == MMU_DIRECT) {
    paddr_write(addr, len, data);
    return;
  }
  IFDEF(CONFIG_MTRACE, Log("Needs address translation."));
  paddr_t addr_translated = isa_mmu_translate(addr, len, MEM_TYPE_WRITE);
  IFDEF(CONFIG_MTRACE, Log("Translated address: 0x%X", addr_translated));

  paddr_write(addr_translated, len, data);
}
