#include <common.h>
#include <memory/paddr.h>
#include <memory/vaddr.h>

#if CONFIG_MTRACE
enum MemTraceType { MEM_TRACE_READ, MEM_TRACE_WRITE };

typedef struct {
  bool trace_on;
  MemTraceType type;
  int len;
  int addr;
  int data;
} MTRACERecord;

static MTRACERecord mtrace_record;
#endif

word_t vaddr_ifetch(vaddr_t addr) { return paddr_ifetch(addr); }

int vaddr_read(int addr, const svBitVecVal *len) {
  int plen = *len;
  if (plen == 3)
    plen = 4;
  int ret = paddr_read(addr, plen);

#if CONFIG_MTRACE
  mtrace_record = (MTRACERecord){.trace_on = true,
                                 .type = MEM_TRACE_READ,
                                 .len = plen,
                                 .addr = addr,
                                 .data = ret};
#endif

  return ret;
}

void vaddr_write(int addr, const svBitVecVal *len, int data) {
  int plen = *len;
  if (plen == 3)
    plen = 4;
  paddr_write(addr, plen, data);

#if CONFIG_MTRACE
  mtrace_record = (MTRACERecord){.trace_on = true,
                                 .type = MEM_TRACE_WRITE,
                                 .len = plen,
                                 .addr = addr,
                                 .data = data};
#endif
}

#if CONFIG_MTRACE
void mtrace() {
  if (!mtrace_record.trace_on) {
    return;
  }

  char *type;
  char *len;

  switch (mtrace_record.type) {
  case MEM_TRACE_READ:
    type = (char *)"read";
    break;
  case MEM_TRACE_WRITE:
    type = (char *)"write";
    break;
  }

  switch (mtrace_record.len) {
  case 1:
    len = (char *)"byte";
    break;
  case 2:
    len = (char *)"half word";
    break;
  case 4:
    len = (char *)"word";
    break;
  }

  Log("MTRACE: addr = 0x%08x, %s %s = 0x%08x", mtrace_record.addr, type, len,
      mtrace_record.data);

  mtrace_record.trace_on = false;
}
#endif

void mtrace_reset() {
#if CONFIG_MTRACE
  mtrace_record.trace_on = false;
#endif
}