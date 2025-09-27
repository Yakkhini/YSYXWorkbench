ENTRY(_start)

PHDRS { boot PT_LOAD; code PT_LOAD; rodata PT_LOAD; rwdata PT_LOAD; }

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
    *(bootloader)
  } > flash AT > flash : boot

  _rxdata_load_start = LOADADDR(.text);
  _rxdata_load_end = _rxdata_load_start + SIZEOF(.text) + SIZEOF(.rodata);
  . = _rxdata_load_end;

  _rwdata_load_start = LOADADDR(.data);
  _rwdata_load_end = _rwdata_load_start + (edata - _rwdata_dest_start);

  /* sram address: [0x0f00_0000, 0x0f00_1fff)  */
  . = ORIGIN(sram);
  _stack_top = ALIGN(0x4);
  _stack_pointer = 0x0f001ff0;

  /* psram address: [0x80_00_0000, 0x80_3f_ffff) */
  . = ORIGIN(psram);
  .text : ALIGN(0x8) {
    _rxdata_dest_start = .;
    *(.text*)
    . = ALIGN(0x8);
    etext = .;
    _etext = .;
  } > psram AT> flash : code
  .rodata : ALIGN(0x8) {
    *(.rodata*)
    *(.srodata*)
    . = ALIGN(0x8);
    *(.rela*)
    . = ALIGN(0x8);
  } > psram AT> flash : rodata

  .data : ALIGN(0x8) {
    _rwdata_dest_start = .;
    _data_start = .;
    *(.data*)
    *(.sdata*)
    . = ALIGN(0x8);
    PROVIDE (edata = .);
    PROVIDE (_edata = .);
  } > psram AT> flash : rwdata

  .bss edata : ALIGN(0x8) {
    _bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(.scommon)
    . = ALIGN(0x8);
    PROVIDE (ebss = .);
  } > psram

  . = ALIGN(0x8);
  _heap_start = ebss;
  _heap_end = 0x803ffff0;
  end = _heap_end;
  _end = _heap_end;

}
