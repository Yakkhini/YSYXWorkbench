#ifndef __VSIMD_H__
#define __VSIMD_H__

#include "fixedpt.h"
#include <common.h>
#include <memory/paddr.h>

void vsimd_init();
void vsimd_receiver(paddr_t addr, int len, word_t data);
word_t vsimd_sender(paddr_t addr, int len);

#endif
