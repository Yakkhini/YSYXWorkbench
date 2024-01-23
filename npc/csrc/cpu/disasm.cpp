#include <capstone/capstone.h>
#include <common.h>
#include <cpu/disasm.h>

static csh hadler;

void disasm_init() { cs_open(CS_ARCH_RISCV, CS_MODE_RISCV32, &hadler); }

void disassembler(word_t pc, word_t code) {
  cs_insn *insn;
  int count = cs_disasm(hadler, (uint8_t *)&code, sizeof(word_t), 0, 1, &insn);

  uint8_t byte1 = (code >> 24) & 0xFF;
  uint8_t byte2 = (code >> 16) & 0xFF;
  uint8_t byte3 = (code >> 8) & 0xFF;
  uint8_t byte4 = code & 0xFF;

  if (count) {
    printf("[0x%08X]: %02x %02x %02x %02x %s\t%s\n", pc, byte1, byte2, byte3,
           byte4, insn->mnemonic, insn->op_str);
    free(insn);
  }
}

void disasm_exit() { cs_close(&hadler); }