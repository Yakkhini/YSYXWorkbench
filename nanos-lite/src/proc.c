#include "config.h"
#include <common.h>
#include <proc.h>
#include <stdint.h>

#define MAX_NR_PROC 4

static PCB pcb_boot = {};
PCB pcb[MAX_NR_PROC] __attribute__((used)) = {};
PCB *current = NULL;

void naive_uload(PCB *pcb, const char *filename);
uintptr_t loader(PCB *pcb, const char *filename);

void switch_boot_pcb() { current = &pcb_boot; }

void hello_fun(void *arg) {
  int j = 1;
  int i = 0;
  while (1) {
    i++;
    if (i == 10) {
      Log("Hello World from Nanos-lite with arg '%p' for the %dth time!",
          (uintptr_t)arg, j);
      j++;
      i = 0;
    }
    yield();
  }
}

void context_kload(PCB *pcb, void (*entry)(void *), void *arg) {
  Area area = {pcb->stack, pcb->stack + STACK_SIZE};
  pcb->cp = kcontext(area, entry, arg);
}

// Ref1: Process stack initialization in LSB 5.0.0 AMD64 core refspec,
// [https://refspecs.linuxfoundation.org/LSB_5.0.0/LSB-Core-AMD64/LSB-Core-AMD64.html].
// Ref2: Section 5.1.2.3.2 "Program startup" in C23 standard,
// [https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf]
void context_uload(PCB *pcb, char *filename, char *argv[], char *envp[]) {
  protect(&pcb->as);

  uint32_t argc = 0;
  uint32_t envc = 0;

  uint8_t *ustack_start = new_page(8);
  uint8_t *ustack_end = ustack_start + 8 * PGSIZE;

  memset(ustack_start, 0, 8 * PGSIZE);

  uint8_t *stack_pointer = ustack_end - 0x400;
  uint8_t *arg_string_pointer = stack_pointer + 0x200;

  for (int i = 0; argv[i] != NULL; i++) {
    argc++;
    *(uintptr_t *)(stack_pointer + (i + 1) * sizeof(uintptr_t)) =
        (uintptr_t)arg_string_pointer;

    Log("Copying argument '%s' to stack at %p, pointer = %p", argv[i],
        arg_string_pointer, stack_pointer + (i + 1) * sizeof(uintptr_t));

    size_t arg_len = strlen(argv[i]) + 1;
    memcpy(arg_string_pointer, argv[i], arg_len);
    Log("arg_len = %d, string = '%s', pointer = %p", arg_len,
        arg_string_pointer, arg_string_pointer);
    arg_string_pointer += arg_len;
  }

  *(uintptr_t *)(stack_pointer + (argc + 1) * sizeof(uintptr_t)) =
      (uintptr_t)NULL;

  for (int i = 0; envp[i] != NULL; i++) {
    envc++;
    *(uintptr_t *)(stack_pointer + (argc + 2 + i) * sizeof(uintptr_t)) =
        (uintptr_t)arg_string_pointer;
    Log("Copying environment variable '%s' to stack at %p, pointer = %p",
        envp[i], arg_string_pointer,
        stack_pointer + (argc + 2 + i) * sizeof(uintptr_t));

    size_t env_len = strlen(envp[i]) + 1;
    memcpy(arg_string_pointer, envp[i], env_len);
    Log("env_len = %d, string = '%s', pointer = %p", env_len,
        arg_string_pointer, arg_string_pointer);
    arg_string_pointer += env_len;
  }

  *(uintptr_t *)(stack_pointer + (argc + 1 + envc + 1) * sizeof(uintptr_t)) =
      (uintptr_t)NULL;

  *(uintptr_t *)stack_pointer = argc;

  void *virtual_ustack_start = pcb->as.area.end - 8 * PGSIZE;

  for (int i = 0; i < 8; i++) {
    map(&pcb->as, virtual_ustack_start + i * PGSIZE, ustack_start + i * PGSIZE,
        0);
  }

  void *virtual_ustack_pointer = pcb->as.area.end - 0x400;

  Log("Loading program '%s' with argc = %d, envc = %d, stack_pointer = %p",
      filename, argc, envc, stack_pointer);

  Area area = {pcb->stack, pcb->stack + STACK_SIZE};

  Context *c = (Context *)(area.end - sizeof(Context));
  memset(c, 0, sizeof(Context));
  c->GPR2 = (uintptr_t)virtual_ustack_pointer;

  void(*entry) = (void (*)())loader(pcb, filename);

  // Currently set Address space as NULL will cause segfault on native
  pcb->cp = ucontext(&pcb->as, area, entry);
}

void init_proc() {
  Log("Initializing processes...");

  context_kload(&pcb[0], hello_fun, (void *)1L);
  context_uload(&pcb[1], "/bin/pal", (char *[]){NULL}, (char *[]){NULL});
  switch_boot_pcb();
}

uint32_t user_proc_shedule_counter = 10;

Context *schedule(Context *prev) {
  current->cp = prev;
  if (current == &pcb[1]) {
    current = user_proc_shedule_counter > 0 ? &pcb[1] : &pcb[0];
    user_proc_shedule_counter =
        user_proc_shedule_counter > 0 ? user_proc_shedule_counter - 1 : 10;
  } else {
    current = &pcb[1];
  }

  return current->cp;
}
