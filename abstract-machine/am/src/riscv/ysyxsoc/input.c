#include <am.h>
#include <ysyxsoc.h>

uint32_t scankey2keycode(uint8_t scankey);
bool pressing = false;

void __am_input_keybrd(AM_INPUT_KEYBRD_T *kbd) {
  uint8_t scankey = inb(KBD_ADDR);
  uint32_t keycode = scankey2keycode(scankey);
  pressing = keycode != AM_KEY_NONE;
  if (scankey == 0xf0) {
    scankey = inb(KBD_ADDR);
    keycode = scankey2keycode(scankey);
  }
  kbd->keydown = pressing;
  kbd->keycode = keycode;
}

uint32_t scankey2keycode(uint8_t scankey) {
  uint32_t keycode = AM_KEY_NONE;
  switch (scankey) {
  case 0x16:
    keycode = AM_KEY_1;
    break;
  case 0x1e:
    keycode = AM_KEY_2;
    break;
  case 0x26:
    keycode = AM_KEY_3;
    break;
  case 0x25:
    keycode = AM_KEY_4;
    break;
  case 0x2e:
    keycode = AM_KEY_5;
    break;
  case 0x36:
    keycode = AM_KEY_6;
    break;
  case 0x3d:
    keycode = AM_KEY_7;
    break;
  case 0x3e:
    keycode = AM_KEY_8;
    break;
  case 0x46:
    keycode = AM_KEY_9;
    break;
  case 0x45:
    keycode = AM_KEY_0;
    break;
  case 0x1c:
    keycode = AM_KEY_A;
    break;
  case 0x32:
    keycode = AM_KEY_B;
    break;
  case 0x21:
    keycode = AM_KEY_C;
    break;
  case 0x23:
    keycode = AM_KEY_D;
    break;
  case 0x24:
    keycode = AM_KEY_E;
    break;
  case 0x2b:
    keycode = AM_KEY_F;
    break;
  case 0x34:
    keycode = AM_KEY_G;
    break;
  case 0x33:
    keycode = AM_KEY_H;
    break;
  case 0x43:
    keycode = AM_KEY_I;
    break;
  case 0x3b:
    keycode = AM_KEY_J;
    break;
  case 0x42:
    keycode = AM_KEY_K;
    break;
  case 0x4b:
    keycode = AM_KEY_L;
    break;
  case 0x3a:
    keycode = AM_KEY_M;
    break;
  case 0x31:
    keycode = AM_KEY_N;
    break;
  case 0x44:
    keycode = AM_KEY_O;
    break;
  case 0x4d:
    keycode = AM_KEY_P;
    break;
  case 0x15:
    keycode = AM_KEY_Q;
    break;
  case 0x2d:
    keycode = AM_KEY_R;
    break;
  case 0x1b:
    keycode = AM_KEY_S;
    break;
  case 0x2c:
    keycode = AM_KEY_T;
    break;
  case 0x3c:
    keycode = AM_KEY_U;
    break;
  case 0x2a:
    keycode = AM_KEY_V;
    break;
  case 0x1d:
    keycode = AM_KEY_W;
    break;
  case 0x22:
    keycode = AM_KEY_X;
    break;
  case 0x35:
    keycode = AM_KEY_Y;
    break;
  case 0x1a:
    keycode = AM_KEY_Z;
    break;
  case 0x29:
    keycode = AM_KEY_SPACE;
    break;
  case 0x5a:
    keycode = AM_KEY_RETURN;
    break;
  case 0x66:
    keycode = AM_KEY_BACKSPACE;
    break;
  case 0x0d:
    keycode = AM_KEY_TAB;
    break;
  case 0x76:
    keycode = AM_KEY_ESCAPE;
    break;
  case 0x12:
    keycode = AM_KEY_LSHIFT;
    break;
  case 0x59:
    keycode = AM_KEY_RSHIFT;
    break;
  case 0x14:
    keycode = AM_KEY_LCTRL;
    break;
  case 0x11:
    keycode = AM_KEY_LALT;
    break;
  }
  return keycode;
}
