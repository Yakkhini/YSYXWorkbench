#ifndef __CPU_IRINGBUF_H__
#define IRINGBUF_NR 20
#define IRINGBUF_LEN 128

typedef struct Iringbuf {
  char store[IRINGBUF_NR][IRINGBUF_LEN];
  int current;
} Iringbuf;

void init_iringbuf();
void iringbuf_insert(char *itrace);
void iringbuf_print();

#endif