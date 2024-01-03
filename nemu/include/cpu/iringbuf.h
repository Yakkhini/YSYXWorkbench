#ifndef __CPU_IRINGBUF_H__
#define IRINGBUF_NR 20

typedef struct Iringbuf {
  char *store[IRINGBUF_NR];
  int current;
} Iringbuf;

void init_iringbuf();
void iringbuf_insert(char *itrace);
void iringbuf_print();

#endif