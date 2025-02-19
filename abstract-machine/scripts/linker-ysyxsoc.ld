ENTRY(_start)
PHDRS { text PT_LOAD; data PT_LOAD; }

MEMORY {
  mrom : ORIGIN = 0x20000000, LENGTH = 4K
  sram : ORIGIN = 0x0f000000, LENGTH = 8K
}

SECTIONS {
  /* sram address: [0x0f00_0000, 0x0f00_1fff)  */
  _heap_start = 0x0f000000;
  _stack_top = 0x0f000000;
  _stack_pointer = _stack_top + 0x1ffc;
  /* _pmem_start and _entry_offset are defined in LDFLAGS */
  . = ORIGIN(mrom);
  .text : {
    *(entry)
    *(.text*)
    etext = .;
    _etext = .;
  } AT> mrom :text
  .rodata : {
    *(.rodata*)
  } AT> mrom
  .data : {
    _data_start = .;
    *(.data*)
    *(.sdata*)
    edata = .;
    _edata = .;
  } > sram AT> mrom : data
  _data_size = SIZEOF(.data);
  _data_load_start = LOADADDR(.data);
  .bss : {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    _bss_end = .;
  } > sram AT> mrom
  end = .;
  _end = .;
}
