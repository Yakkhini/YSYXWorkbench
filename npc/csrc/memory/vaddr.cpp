#include <common.h>
#include <memory/paddr.h>
#include <memory/vaddr.h>

enum MemLen { MEM_BYTE, MEM_HWORD, MEM_WORD };

enum MemTraceType { MEM_TRACE_READ, MEM_TRACE_WRITE };

typedef struct {
  bool trace_on;
  MemTraceType type;
  MemLen len;
  int addr;
  int data;
} MTRACERecord;

static MTRACERecord mtrace_record;

word_t vaddr_ifetch(vaddr_t addr) { return paddr_read(addr, 4); }

int vaddr_read(int addr, const svBitVecVal *len) {
  mtrace_record.trace_on = true;

  switch (*len) {
    int ret;
  case 1:
    ret = paddr_read(addr, 1);
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_READ,
                                   .len = MEM_BYTE,
                                   .addr = addr,
                                   .data = ret};
    return ret;
  case 2:
    ret = paddr_read(addr, 2);
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_READ,
                                   .len = MEM_HWORD,
                                   .addr = addr,
                                   .data = ret};
    return ret;
  case 3:
    ret = paddr_read(addr, 4);
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_READ,
                                   .len = MEM_WORD,
                                   .addr = addr,
                                   .data = ret};
    return ret;
  default:
    Log("Invalid memory access type");
    return 0;
  }
}

void vaddr_write(int addr, const svBitVecVal *len, int data) {
  switch (*len) {
  case 1:
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_WRITE,
                                   .len = MEM_BYTE,
                                   .addr = addr,
                                   .data = data};
    paddr_write(addr, 1, data);
    break;
  case 2:
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_WRITE,
                                   .len = MEM_HWORD,
                                   .addr = addr,
                                   .data = data};
    paddr_write(addr, 2, data);
    break;
  case 3:
    mtrace_record = (MTRACERecord){.trace_on = true,
                                   .type = MEM_TRACE_WRITE,
                                   .len = MEM_WORD,
                                   .addr = addr,
                                   .data = data};
    paddr_write(addr, 4, data);
    break;
  default:
    Log("Invalid memory access type");
    break;
  }
}

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
  case MEM_BYTE:
    len = (char *)"byte";
    break;
  case MEM_HWORD:
    len = (char *)"half word";
    break;
  case MEM_WORD:
    len = (char *)"word";
    break;
  }

  Log("MTRACE: addr = 0x%08x, %s %s = 0x%08x", mtrace_record.addr, type, len,
      mtrace_record.data);

  mtrace_record.trace_on = false;
}

void mtrace_reset() { mtrace_record.trace_on = false; }