#include <am.h>
#include <klib.h>
#include <nemu.h>

static AddrSpace kas = {};
static void *(*pgalloc_usr)(int) = NULL;
static void (*pgfree_usr)(void *) = NULL;
static int vme_enable = 0;

// Kernel memory mappings
static Area segments[] = {NEMU_PADDR_SPACE};

#define USER_SPACE RANGE(0x40000000, 0x80000000)

static inline void set_satp(void *pdir) {
  uintptr_t mode = 1ul << (__riscv_xlen - 1);
  asm volatile("csrw satp, %0" : : "r"(mode | ((uintptr_t)pdir >> 12)));
}

static inline uintptr_t get_satp() {
  uintptr_t satp;
  asm volatile("csrr %0, satp" : "=r"(satp));
  return satp << 12;
}

bool vme_init(void *(*pgalloc_f)(int), void (*pgfree_f)(void *)) {
  pgalloc_usr = pgalloc_f;
  pgfree_usr = pgfree_f;

  kas.ptr = pgalloc_f(PGSIZE);

  int i;
  for (i = 0; i < LENGTH(segments); i++) {
    void *va = segments[i].start;
    for (; va < segments[i].end; va += PGSIZE) {
      map(&kas, va, va, 0);
    }
  }

  set_satp(kas.ptr);
  vme_enable = 1;

  return true;
}

void protect(AddrSpace *as) {
  PTE *updir = (PTE *)(pgalloc_usr(PGSIZE));
  as->ptr = updir;
  as->area = USER_SPACE;
  as->pgsize = PGSIZE;
  // map kernel space
  memcpy(updir, kas.ptr, PGSIZE);
}

void unprotect(AddrSpace *as) {}

void __am_get_cur_as(Context *c) {
  c->pdir = (vme_enable ? (void *)get_satp() : NULL);
}

void __am_switch(Context *c) {
  if (vme_enable && c->pdir != NULL) {
    set_satp(c->pdir);
  }
}

void map(AddrSpace *as, void *va, void *pa, int prot) {
  int vpn0 = ((uintptr_t)va >> 12) & ((0x1 << 10) - 1);
  int vpn1 = (uintptr_t)va >> 22;

  uint32_t *pte1 = as->ptr + vpn1 * 4;
  if (*pte1 == 0) {
    *pte1 = ((uint32_t)pgalloc_usr(PGSIZE) >> 2) | 0b0000000001;
  }

  uint32_t *pte0 = (uint32_t *)(((*pte1 >> 10) << 12) + vpn0 * 4);
  if (*pte0 == 0) {
    *pte0 = (((uint32_t)pa & 0xFFFFF000) >> 2) | 0b0000001111;
  }
}

// MSCRATCH can be any value since it will be override by
// kernel sp when first time run this process.
Context *ucontext(AddrSpace *as, Area kstack, void *entry) {
  Context *c = (Context *)(kstack.end - sizeof(Context));
  c->pdir = as->ptr;
  c->mstatus = 0x1880;
  c->mepc = (uintptr_t)entry;
  c->mscratch = 0xdeadbeef;
  c->next_privilege = PRIVILEGE_USER;
  return c;
}
