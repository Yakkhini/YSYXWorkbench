ENTRY(_start)
PHDRS { text PT_LOAD; rodata PT_LOAD; data PT_LOAD; }

SECTIONS {
  /* _pmem_start and _entry_offset are defined in LDFLAGS */
  . = _pmem_start + _entry_offset;
  .text : {
    *(entry)
    *(.text*)
    etext = .;
    _etext = .;
  } : text
  .rodata : {
    *(.rodata*)
    *(.srodata*)
  } : rodata
  .data : {
    *(.data*)
    *(.sdata*)
    edata = .;
    _edata = .;
  } : data
  .bss : {
	_bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    ebss = .;
  } : data
  _stack_top = ALIGN(0x1000);
  . = _stack_top + 0x8000;
  _stack_pointer = .;
  end = .;
  _end = .;
  _heap_start = ALIGN(0x1000);
}
