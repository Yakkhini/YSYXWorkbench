#include <memory.h>
#include <proc.h>

static void *pf = NULL;

void *new_page(size_t nr_page) {
  void *p = pf;
  pf += nr_page * PGSIZE;
  return p;
}

#ifdef HAS_VME
static void *pg_alloc(int n) {
  int nr_page = (n + PGSIZE - 1) / PGSIZE;
  void *p = new_page(nr_page);
  memset(p, 0, nr_page * PGSIZE);
  return p;
}

void syscall_pg_alloc_handler(uintptr_t start, uintptr_t offset) {
  if ((start + offset) < current->max_brk) {
    memset((void *)start, 0, offset);
    return;
  }

  uintptr_t va_start = ROUNDUP(start, PGSIZE);
  uintptr_t new_brk = ROUNDUP(start + offset, PGSIZE);
  uintptr_t alloc_size = new_brk - va_start;

  uintptr_t pa_start = (uintptr_t)pg_alloc(alloc_size);

  for (int i = 0; va_start + i < new_brk; i += PGSIZE) {
    map(&current->as, (void *)(va_start + i), (void *)(pa_start + i), 0);
  }

  current->max_brk = new_brk;
  memset((void *)start, 0, offset);
  return;
}
#endif

void free_page(void *p) { panic("not implement yet"); }

/* The brk() system call handler. */
int mm_brk(uintptr_t brk) { return 0; }

void init_mm() {
  pf = (void *)ROUNDUP(heap.start, PGSIZE);
  Log("free physical pages starting from %p", pf);

#ifdef HAS_VME
  vme_init(pg_alloc, free_page);
#endif
}
