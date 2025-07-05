ENTRY(_start)
PHDRS { code PT_LOAD; rodata PT_LOAD; rwdata PT_LOAD; }

MEMORY {
  sram : ORIGIN = 0x0f000000, LENGTH = 8K
  mrom : ORIGIN = 0x20000000, LENGTH = 4K
  flash : ORIGIN = 0x30000000, LENGTH = 16M
}

SECTIONS {
  . = ORIGIN(flash);
  _entry_offset = 0x0;
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
  /* sram address: [0x0f00_0000, 0x0f00_1fff)  */
  _rwdata_load_start = .;
  _heap_start = ORIGIN(sram);
  _stack_top = ORIGIN(sram) + LENGTH(sram) - 0x1000;
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
  } > sram : rwdata
  _rwdata_size = SIZEOF(.data);

  _stack_pointer = ORIGIN(sram) + LENGTH(sram) - 0x100;
  end = 0x0f001fff;
  _end = 0x0f001fff;
}
