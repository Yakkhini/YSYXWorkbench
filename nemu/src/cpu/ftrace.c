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

static int ftrace_is_enabled = false;
static FunctionNode *head = NULL;

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

  if (head == NULL) {
    head = (FunctionNode *)malloc(sizeof(FunctionNode));
    head->name = name;
    head->pos = pos;
    head->next = NULL;
    return;
  }

  FunctionNode *current = head;
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
  FunctionNode *current = head;
  while (current != NULL) {
    printf("Function name in link table: %s\n", current->name);
    current = current->next;
  }
}