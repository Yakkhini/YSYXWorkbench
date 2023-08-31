#include <Vsriz.h>
#include <cstdint>
#include <stdio.h>
#include <verilated.h>

uint32_t pmem_read(IData x) { return 0; }

int main(int argc, char **argv) {
  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vsriz *top = new Vsriz(contextp);
  printf("Hello, SuanChou Processor Core!\n");

  while (!contextp->gotFinish()) {
    top->inst = pmem_read(top->pc);
    top->eval();
  }
  return 0;
}
