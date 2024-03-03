#include <elf.h>
#include <proc.h>
#include <stdint.h>

#ifdef __LP64__
#define Elf_Ehdr Elf64_Ehdr
#define Elf_Phdr Elf64_Phdr
#else
#define Elf_Ehdr Elf32_Ehdr
#define Elf_Phdr Elf32_Phdr
#endif

#if defined(__ISA_AM_NATIVE__)
#define EXPECT_MACHINETYPE EM_X86_64
#elif defined(__riscv)
#define EXPECT_MACHINETYPE EM_RISCV
#endif

size_t ramdisk_read(void *buf, size_t offset, size_t len);
size_t ramdisk_write(const void *buf, size_t offset, size_t len);

static uintptr_t loader(PCB *pcb, const char *filename) {
  Elf_Ehdr *ehdr = (Elf_Ehdr *)malloc(sizeof(Elf_Ehdr));
  ramdisk_read(ehdr, 0, sizeof(Elf_Ehdr));
  Elf_Phdr *phdr_list = (Elf_Phdr *)malloc(ehdr->e_phnum * sizeof(Elf_Phdr));
  ramdisk_read(phdr_list, ehdr->e_phoff, ehdr->e_phnum * sizeof(Elf_Phdr));

  assert(*(uint32_t *)ehdr->e_ident ==
         0x464c457f); // 0x7f, 'E'=0x45, 'L'=0x4c, 'F'=0x46
  assert(ehdr->e_machine == EXPECT_MACHINETYPE);

  for (int i = 0; i < ehdr->e_phnum; i++) {
    if (phdr_list[i].p_type == PT_LOAD) {
      uintptr_t addr = phdr_list[i].p_vaddr;
      uintptr_t off = phdr_list[i].p_offset;
      uintptr_t filesz = phdr_list[i].p_filesz;
      uintptr_t memsz = phdr_list[i].p_memsz;
      void *buf = malloc(filesz);
      Log("Loading [0x%08x, 0x%08x) to [0x%08x, 0x%08x)", off, off + filesz,
          addr, addr + memsz);
      ramdisk_read(buf, off, filesz);
      memcpy((void *)addr, buf, filesz);
      memset((void *)(addr + filesz), 0, memsz - filesz);
    }
  }
  return ehdr->e_entry;
}

void naive_uload(PCB *pcb, const char *filename) {
  uintptr_t entry = loader(pcb, filename);
  Log("Jump to entry = %p", entry);
  ((void (*)())entry)();
}
