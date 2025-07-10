ENTRY(_start)
PHDRS { code PT_LOAD; rodata PT_LOAD; rwdata PT_LOAD; bss PT_LOAD; }

MEMORY {
  sram : ORIGIN = 0x0f000000, LENGTH = 8K
  mrom : ORIGIN = 0x20000000, LENGTH = 4K
  flash : ORIGIN = 0x30000000, LENGTH = 16M
}

SECTIONS {
  _entry_offset = 0x0;
  . = ORIGIN(flash);
  _pmem_start = .;
  .text : {
    *(entry)
    *(.text*)
    etext = .;
    _etext = .;
  } > flash AT> flash : code
  . = ALIGN(0x10);
  .rodata : {
    *(.rodata*)
    *(.srodata*)
  } > flash AT> flash : rodata
  _rwdata_load_start = .;
  _rwdata_load_end = _rwdata_load_start + SIZEOF(.data) + SIZEOF(.bss);
  /* sram address: [0x0f00_0000, 0x0f00_1fff)  */
  . = ORIGIN(sram);
  _sram_start = .;
  .data : {
    _data_start = .;
    *(.data*)
    *(.sdata*)
    edata = .;
    _edata = .;
  } > sram AT> flash : rwdata
  .bss : {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    ebss = .;
  } > sram AT > flash : bss

  . = . + 0x10;
  _heap_start = ALIGN(0x4);
  _stack_top = ALIGN(0x4);
  _stack_pointer = 0x0F001FF0;
  end = .;
  _end = .;
}
