#include "config.h"
#include <common.h>
#include <proc.h>

#define MAX_NR_PROC 4

static PCB pcb[MAX_NR_PROC] __attribute__((used)) = {};
static PCB pcb_boot = {};
PCB *current = NULL;

static uintptr_t user_stack_alloc_offset = 0;

void naive_uload(PCB *pcb, const char *filename);
uintptr_t loader(PCB *pcb, const char *filename);

void switch_boot_pcb() { current = &pcb_boot; }

void hello_fun(void *arg) {
  int j = 1;
  while (1) {
    Log("Hello World from Nanos-lite with arg '%p' for the %dth time!",
        (uintptr_t)arg, j);
    j++;
    yield();
  }
}

void context_kload(PCB *pcb, void (*entry)(void *), void *arg) {
  Area area = {pcb->stack, pcb->stack + STACK_SIZE};
  pcb->cp = kcontext(area, entry, arg);
}

void context_uload(PCB *pcb, char *filename) {
  Area area = {pcb->stack, pcb->stack + STACK_SIZE};

  uint8_t *ustack_start = heap.end - user_stack_alloc_offset - STACK_SIZE;
  user_stack_alloc_offset += STACK_SIZE;

  Context *c = (Context *)(area.end - sizeof(Context));
  memset(c, 0, sizeof(Context));
  c->GPR2 = (uintptr_t)ustack_start;

  void(*entry) = (void (*)())loader(pcb, filename);

  // Currently set Address space as NULL will cause segfault on native
  pcb->cp = ucontext(NULL, area, entry);
}

void init_proc() {
  Log("Initializing processes...");

  context_kload(&pcb[0], hello_fun, (void *)1L);
  context_uload(&pcb[1], "/bin/pal");
  switch_boot_pcb();
}

Context *schedule(Context *prev) {
  current->cp = prev;
  current = (current == &pcb[0] ? &pcb[1] : &pcb[0]);
  return current->cp;
}
