#include <Vsriz.h>
#include <Vsriz__Dpi.h>
#include <getopt.h>
#include <memory/paddr.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

static char *NPC_HOME = getenv("NPC_HOME");
static char *img_file = NULL;
static bool HALT = false;

int inst_fetch(int pc) {
  uint32_t inst = paddr_read(pc, 4);
  if (inst == 0x00100073) {
    HALT = true;
  }

  printf("Fetch instruction 0x%08X\n", inst);

  return inst;
};

static long load_img() {
  if (img_file == NULL) {
    printf("No image is given. Use the default build-in image.");
    return 4096; // built-in image size
  }

  FILE *fp = fopen(img_file, "rb");

  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);

  printf("The image is %s, size = %ld\n", img_file, size);

  fseek(fp, 0, SEEK_SET);
  int ret = fread(guest_to_host(0x80000000), size, 1, fp);
  assert(ret == 1);

  fclose(fp);
  return size;
}

static int parse_args(int argc, char *argv[]) {
  const struct option table[] = {
      {0, 0, NULL, 0},
  };
  int o;
  while ((o = getopt_long(argc, argv, "-h:", table, NULL)) != -1) {
    switch (o) {
    case 1:
      img_file = optarg;
      return 0;
    default:
      printf("Usage: %s [OPTION...] IMAGE [args]\n\n", argv[0]);
      printf("\t-b,--batch              run with batch mode\n");
      printf("\t-f,--ftrace=FILE        trace functions with elf file.\n");
      printf("\t-l,--log=FILE           output log to FILE\n");
      printf("\t-d,--diff=REF_SO        run DiffTest with reference REF_SO\n");
      printf("\t-p,--port=PORT          run DiffTest with port PORT\n");
      printf("\n");
      exit(0);
    }
  }
  return 0;
}

void single_clock(Vsriz *top, VerilatedContext *contextp, VerilatedVcdC *tfp) {
  contextp->timeInc(1);
  top->clk = 1;
  top->eval();
  tfp->dump(contextp->time());
  contextp->timeInc(1);
  top->clk = 0;
  top->eval();
  tfp->dump(contextp->time());
}

void reset(Vsriz *top, VerilatedContext *contextp, VerilatedVcdC *tfp) {
  top->rst = 1;
  contextp->timeInc(1);
  top->eval();
  tfp->dump(contextp->time());
  single_clock(top, contextp, tfp);
  top->rst = 0;
}

int main(int argc, char **argv) {

  parse_args(argc, argv);
  load_img();

  VerilatedContext *contextp = new VerilatedContext;
  contextp->commandArgs(argc, argv);

  Vsriz *top = new Vsriz(contextp);

  char wavefile_name[80];
  strcpy(wavefile_name, NPC_HOME);
  strcat(wavefile_name, "/build/waveform.vcd");
  printf("Hello, SuanChou Processor Core!\n");
  printf("Wave Path: %s\n", wavefile_name);

  Verilated::traceEverOn(true);
  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 5);

  tfp->open(wavefile_name);
  int sim_time = 100;

  reset(top, contextp, tfp);

  while (contextp->time() < sim_time && !contextp->gotFinish() &&
         HALT == false) {
    single_clock(top, contextp, tfp);
  }

  tfp->close();

  top->final();
  delete top;

  return 0;
}
