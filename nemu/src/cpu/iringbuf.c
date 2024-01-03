#include <cpu/iringbuf.h>
#include <stdio.h>
#include <string.h>

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

  for (int i = 0; i < IRINGBUF_NR; i++) {
    printf("%s\n", iringbuf.store[i]);
  }
  return;
}
