#include <string.h>
#define IRINGBUF_NR 20

typedef struct Iringbuf {
  char *store[IRINGBUF_NR];
  int current;
} Iringbuf;

static Iringbuf iringbuf;

void init_iringbuf() {
  for (int i = 0; i < IRINGBUF_NR; i++) {
    iringbuf.store[i] = "Unused.";
  }
  iringbuf.current = 20;
  return;
}

void iringbuf_insert(char *itrace) {
  iringbuf.current %= 20;
  iringbuf.store[iringbuf.current] = itrace;
  return;
}

void iringbuf_print() {
  iringbuf.store[iringbuf.current] =
      strcat("--> ", iringbuf.store[iringbuf.current]);
  return;
}
