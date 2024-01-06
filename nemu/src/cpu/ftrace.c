#include "common.h"
#include "debug.h"
#include <assert.h>
#include <cpu/ftrace.h>
#include <elf.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct FunctionNode {
  char *name;
  vaddr_t pos;
  struct FunctionNode *next;
} FunctionNode;

enum FUNCCALLMODE { FTRACE_CALL, FTRACE_RET };

typedef struct FunctionCallNode {
  vaddr_t source;
  vaddr_t target;
  char *funcname;
  enum FUNCCALLMODE mode;

  struct FunctionCallNode *next;
} FunctionCallNode;

static int ftrace_is_enabled = false;
static FunctionNode *function_head = NULL;
static FunctionCallNode *function_call_head = NULL;

void function_call_link_table_insert(vaddr_t source, vaddr_t target,
                                     char *funcname, enum FUNCCALLMODE mode);

void function_link_table_insert(char *name, vaddr_t pos);
void function_link_table_print();

void enable_ftrace(char *path) {
  ftrace_is_enabled = true; // Maybe depends on TRACE or other options?
  FILE *elf_file = fopen(path, "rb");
  Elf32_Ehdr elf_hdr;
  Elf32_Shdr symtab_shdr;
  Elf32_Shdr strtab_shdr;
  Elf32_Shdr shstrtab_shdr;

  int check;

  check = fread(&elf_hdr, sizeof(elf_hdr), 1, elf_file);
  assert(check == 1);

  Elf32_Shdr section_hdrs[elf_hdr.e_shnum];
  fseek(elf_file, elf_hdr.e_shoff, SEEK_SET);
  check = fread(&section_hdrs, elf_hdr.e_shentsize, elf_hdr.e_shnum, elf_file);
  assert(check == elf_hdr.e_shnum);

  char *shstrtab;
  shstrtab_shdr = section_hdrs[elf_hdr.e_shstrndx];
  shstrtab = malloc(shstrtab_shdr.sh_size);
  fseek(elf_file, shstrtab_shdr.sh_offset, SEEK_SET);
  check = fread(shstrtab, shstrtab_shdr.sh_size, 1, elf_file);
  assert(check == 1);

  for (int i = 0; i < elf_hdr.e_shnum; i++) {
    if (section_hdrs[i].sh_type == SHT_SYMTAB) {
      symtab_shdr = section_hdrs[i];
    }

    if (strcmp(shstrtab + section_hdrs[i].sh_name, ".strtab") == 0) {
      strtab_shdr = section_hdrs[i];
    }
  }

  int symnum = symtab_shdr.sh_size / sizeof(Elf32_Sym);
  Elf32_Sym syms[symnum];
  fseek(elf_file, symtab_shdr.sh_offset, SEEK_SET);
  check = fread(syms, symtab_shdr.sh_size, 1, elf_file);
  assert(check == 1);

  char *strtab = malloc(strtab_shdr.sh_size);
  fseek(elf_file, strtab_shdr.sh_offset, SEEK_SET);
  check = fread(strtab, strtab_shdr.sh_size, 1, elf_file);
  assert(check == 1);

  for (int i = 0; i < symnum; i++) {
    if (ELF32_ST_TYPE(syms[i].st_info) != STT_FUNC) {
      continue;
    }

    function_link_table_insert(strtab + syms[i].st_name, syms[i].st_value);
  }

  function_link_table_print();

  fclose(elf_file);

  return;
}

void function_link_table_insert(char *name, vaddr_t pos) {

  if (function_head == NULL) {
    function_head = (FunctionNode *)malloc(sizeof(FunctionNode));
    function_head->name = name;
    function_head->pos = pos;
    function_head->next = NULL;
    return;
  }

  FunctionNode *current = function_head;
  while (current->next != NULL) {
    current = current->next;
  }

  current->next = (FunctionNode *)malloc(sizeof(FunctionNode));
  current->next->name = name;
  current->next->pos = pos;
  current->next->next = NULL;

  return;
}

void function_link_table_print() {
  FunctionNode *current = function_head;
  while (current != NULL) {
    printf("Function name in link table: %s\n", current->name);
    current = current->next;
  }
}

void ftrace_check(vaddr_t source_addr, vaddr_t target_addr) {
  FunctionNode *current = function_head;

  while (current != NULL) {
    if (current->pos == target_addr) {
      function_call_link_table_insert(source_addr, target_addr, current->name,
                                      FTRACE_CALL);
      printf("0x%X: Call function %s@0x%X, return in 0x%X.\n", source_addr,
             current->name, target_addr, source_addr + 4);
      return;
    }
    current = current->next;
  }

  FunctionCallNode *call_current = function_call_head;

  while (call_current != NULL) {

    if (call_current->source + 4 == target_addr) {
      function_call_link_table_insert(source_addr, target_addr,
                                      call_current->funcname, FTRACE_RET);
      printf("0x%X: Return from function %s@0x%X.\n", source_addr,
             call_current->funcname, call_current->target);
      return;
    }

    call_current = call_current->next;
  }

  return;
}

void function_call_link_table_insert(vaddr_t source, vaddr_t target,
                                     char *funcname, enum FUNCCALLMODE mode) {

  if (function_call_head == NULL) {
    function_call_head = (FunctionCallNode *)malloc(sizeof(FunctionCallNode));
    function_call_head->source = source;
    function_call_head->target = target;
    function_call_head->funcname = funcname;
    function_call_head->mode = mode;
    function_call_head->next = NULL;
    return;
  }

  FunctionCallNode *current = function_call_head;
  while (current->next != NULL) {
    current = current->next;
  }

  current->next = (FunctionCallNode *)malloc(sizeof(FunctionCallNode));
  current->next->source = source;
  current->next->target = target;
  current->next->funcname = funcname;
  current->next->mode = mode;
  current->next->next = NULL;
  return;
}