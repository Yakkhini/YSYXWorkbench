#include "common.h"
#include <elf.h>
#ifndef __CPU_FTRACE_H__

// Public

void ftrace_init(char *path);
void ftrace_check(int rd, vaddr_t dnpc, word_t src1);

// Internal
bool get_ftrace_enable();

void elf_parse(char *path);
Elf32_Sym *get_symtab();
int get_symnum();
char *get_strtab();

void ftrace_link_table_build();

#endif