ENTRY(_start)
PHDRS { boot PT_LOAD; code PT_LOAD; rodata PT_LOAD; rwdata PT_LOAD; bss PT_LOAD; }

MEMORY {
  sram : ORIGIN = 0x0f000000, LENGTH = 8K
  mrom : ORIGIN = 0x20000000, LENGTH = 4K
  flash : ORIGIN = 0x30000000, LENGTH = 16M
  psram : ORIGIN = 0x80000000, LENGTH = 4M
}

SECTIONS {
  _entry_offset = 0x0;

  . = ORIGIN(flash);
  .bootloader : {
    *(entry)
  } > flash AT > flash : boot
  _rxdata_load_start = .;
  _rxdata_load_end = _rxdata_load_start + SIZEOF(.text) + SIZEOF(.rodata);
  _rwdata_load_start = _rxdata_load_end;
  _rwdata_load_end = _rwdata_load_start + SIZEOF(.data) + SIZEOF(.bss);

  /* sram address: [0x0f00_0000, 0x0f00_1fff)  */
  . = ORIGIN(sram);
  _sram_start = .;
  _stack_top = ALIGN(0x4);
  _stack_pointer = 0x0f001ff0;

  /* psram address: [0x80_00_0000, 0x80_3f_ffff) */
  . = ORIGIN(psram);
  _psram_start = .;
  .text : {
    _rxdate_dest_start = .;
    *(.text*)
    etext = .;
    _etext = .;
  } > psram AT> flash : code
  .rodata : {
    *(.rodata*)
    *(.srodata*)
    *(.rela*)
  } > psram AT> flash : rodata
  .data : {
    _rwdata_dest_start = .;
    _data_start = .;
    *(.data*)
    *(.sdata*)
    edata = .;
    _edata = .;
  } > psram AT> flash : rwdata
  .bss : {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    ebss = .;
  } > psram AT > flash : bss

  . = . + 0x10;
  _heap_start = ALIGN(0x4);
  _heap_end = 0x803ffff0;
  end = .;
  _end = .;
}
