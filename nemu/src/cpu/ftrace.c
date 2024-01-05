#include "common.h"
#include "debug.h"
#include <assert.h>
#include <cpu/ftrace.h>
#include <elf.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int ftrace_is_enabled = false;

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
  printf("%i, %i", symtab_shdr.sh_name, strtab_shdr.sh_name);

  fclose(elf_file);

  return;
}
