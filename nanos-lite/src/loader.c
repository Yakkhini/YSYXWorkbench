#include <elf.h>
#include <fs.h>
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

uintptr_t loader(PCB *pcb, const char *filename) {
  int fd = fs_open(filename, 0, 0);

  fs_lseek(fd, 0, SEEK_SET);
  Elf_Ehdr *ehdr =
      (Elf_Ehdr *)new_page((sizeof(Elf_Ehdr) + PGSIZE - 1) / PGSIZE);
  fs_read(fd, ehdr, sizeof(Elf_Ehdr));
  Elf_Phdr *phdr_list = (Elf_Phdr *)new_page(
      ehdr->e_phnum * (sizeof(Elf_Phdr) + PGSIZE - 1) / PGSIZE);
  fs_lseek(fd, ehdr->e_phoff, SEEK_SET);
  fs_read(fd, phdr_list, ehdr->e_phnum * sizeof(Elf_Phdr));

  assert(*(uint32_t *)ehdr->e_ident ==
         0x464c457f); // 0x7f, 'E'=0x45, 'L'=0x4c, 'F'=0x46
  assert(ehdr->e_machine == EXPECT_MACHINETYPE);

  for (int i = 0; i < ehdr->e_phnum; i++) {
    if (phdr_list[i].p_type == PT_LOAD) {
      uintptr_t addr = phdr_list[i].p_vaddr;
      uintptr_t off = phdr_list[i].p_offset;
      uintptr_t filesz = phdr_list[i].p_filesz;
      uintptr_t memsz = phdr_list[i].p_memsz;

      uint32_t preset_zero_size = addr & 0xfff;
      uint32_t nr_page = (preset_zero_size + memsz + PGSIZE - 1) / PGSIZE;
      void *buf = new_page(nr_page);
      Log("Loading [0x%08x, 0x%08x) to [0x%08x, 0x%08x) in %d page start at "
          "0x%08x",
          off, off + filesz, addr + preset_zero_size,
          addr + preset_zero_size + memsz, nr_page, buf + preset_zero_size);

      pcb->max_brk = addr + memsz;
      memset(buf, 0, nr_page * PGSIZE);
      fs_lseek(fd, off, SEEK_SET);
      fs_read(fd, buf + preset_zero_size, filesz);
      for (int i = 0; i < nr_page; i++) {
        map(&pcb->as, (void *)(addr + i * PGSIZE),
            buf + preset_zero_size + i * PGSIZE, 0);
      }
    }
  }

  fs_close(fd);

  return ehdr->e_entry;
}

void naive_uload(PCB *pcb, const char *filename) {
  uintptr_t entry = loader(pcb, filename);
  Log("Jump to entry = 0x%08x", entry);
  ((void (*)())entry)();
}
