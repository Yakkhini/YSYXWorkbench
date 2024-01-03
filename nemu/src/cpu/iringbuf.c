#include <common.h>
#include <cpu/iringbuf.h>
#include <stdio.h>
#include <string.h>

static Iringbuf iringbuf;

void init_iringbuf() {
  for (int i = 0; i < IRINGBUF_NR; i++) {
    strcpy(iringbuf.store[i], "   \tUnused.");
  }

  iringbuf.current = 19;

  return;
}

void iringbuf_insert(char *itrace) {
  iringbuf.current++;
  iringbuf.current %= 20;

  strcpy(iringbuf.store[iringbuf.current], "   \t");
  strcat(iringbuf.store[iringbuf.current], itrace);
  //Log("Store instruction in %i position.", iringbuf.current);
  //Log("Content: %s", itrace);

  return;
}

void iringbuf_print() {
  iringbuf.store[iringbuf.current][0] = '-';
  iringbuf.store[iringbuf.current][1] = '-';
  iringbuf.store[iringbuf.current][2] = '>';

  for (int i = 0; i < IRINGBUF_NR; i++) {
    printf("%s\n", iringbuf.store[i]);
  }

  return;
}
