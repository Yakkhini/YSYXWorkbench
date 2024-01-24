#ifndef __DISASM_H__
#define __DISASM_H__

#include "common.h"

void disasm_init();
void disassembler(word_t code);
void disasm_exit();

#endif