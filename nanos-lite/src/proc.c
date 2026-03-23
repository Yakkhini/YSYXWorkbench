#include "config.h"
#include <common.h>
#include <proc.h>

#define MAX_NR_PROC 4

static PCB pcb_boot = {};
PCB pcb[MAX_NR_PROC] __attribute__((used)) = {};
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

// Ref: Process stack initialization in LSB 5.0.0 AMD64 core refspec,
// [https://refspecs.linuxfoundation.org/LSB_5.0.0/LSB-Core-AMD64/LSB-Core-AMD64.html].
void context_uload(PCB *pcb, char *filename, char *argv[], char *envp[]) {
  uint32_t argc = 0;
  uint32_t envc = 0;

  uint8_t *ustack_end = heap.end - user_stack_alloc_offset;
  user_stack_alloc_offset += STACK_SIZE;

  uint8_t *stack_pointer = ustack_end - 0x1000;
  uint8_t *arg_string_pointer = stack_pointer + 0x500;

  for (int i = 0; argv[i] != NULL; i++) {
    argc++;
    *(uintptr_t *)(stack_pointer + (i + 1) * sizeof(uintptr_t)) =
        (uintptr_t)arg_string_pointer;

    size_t arg_len = strlen(argv[i]) + 1;
    memcpy(arg_string_pointer, argv[i], arg_len);
    argv[i] = (char *)arg_string_pointer;
    arg_string_pointer += arg_len;
  }

  *(uintptr_t *)(stack_pointer + (argc + 2) * sizeof(uintptr_t)) =
      (uintptr_t)NULL;

  for (int i = 0; envp[i] != NULL; i++) {
    envc++;
    *(uintptr_t *)(stack_pointer + (argc + 2 + i) * sizeof(uintptr_t)) =
        (uintptr_t)arg_string_pointer;

    size_t env_len = strlen(envp[i]) + 1;
    memcpy(arg_string_pointer, envp[i], env_len);
    envp[i] = (char *)arg_string_pointer;
    arg_string_pointer += env_len;
  }

  *(uintptr_t *)(stack_pointer + (argc + 2 + envc + 1) * sizeof(uintptr_t)) =
      (uintptr_t)NULL;

  *(uintptr_t *)stack_pointer = argc;

  Log("Loading program '%s' with argc = %d, envc = %d, stack_pointer = %p",
      filename, argc, envc, stack_pointer);

  Area area = {pcb->stack, pcb->stack + STACK_SIZE};

  Context *c = (Context *)(area.end - sizeof(Context));
  memset(c, 0, sizeof(Context));
  c->GPR2 = (uintptr_t)stack_pointer;

  void(*entry) = (void (*)())loader(pcb, filename);

  // Currently set Address space as NULL will cause segfault on native
  pcb->cp = ucontext(NULL, area, entry);
}

void init_proc() {
  Log("Initializing processes...");

  context_kload(&pcb[0], hello_fun, (void *)1L);
  context_uload(&pcb[1], "/bin/menu", (char *[]){NULL}, (char *[]){NULL});
  switch_boot_pcb();
}

Context *schedule(Context *prev) {
  current->cp = prev;
  current = (current == &pcb[0] ? &pcb[1] : &pcb[0]);
  return current->cp;
}
