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
  . = _rxdata_load_end;

  _rwdata_load_start = .;
  _rwdata_load_end = _rwdata_load_start + (edata - _rwdata_dest_start);

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

  . = ALIGN(0x8);
  _rwdata_dest_start = .;
  .data BLOCK(0x8) : {
    _data_start = .;
    *(.data*)
    *(.sdata*)
    edata = .;
    _edata = .;
    . = ALIGN(0x8);
  } > psram AT> flash : rwdata
  .bss BLOCK(0x8) : {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    ebss = .;
  } > psram AT> flash : rwdata

  . = . + 0x10;
  _heap_start = ALIGN(0x8);
  . = 0x803fffe0;
  _heap_end = .;
  end = .;
  _end = .;
}
