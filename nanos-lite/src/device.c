#include <common.h>

#if defined(MULTIPROGRAM) && !defined(TIME_SHARING)
#define MULTIPROGRAM_YIELD() yield()
#else
#define MULTIPROGRAM_YIELD()
#endif

#define NAME(key) [AM_KEY_##key] = #key,

static const char *keyname[256]
    __attribute__((used)) = {[AM_KEY_NONE] = "NONE", AM_KEYS(NAME)};

size_t serial_write(const void *buf, size_t offset, size_t len) {
  for (int i = 0; i < len; i++) {
    putch(*(char *)(buf + i));
  }
  return len;
}

size_t events_read(void *buf, size_t offset, size_t len) {
  AM_INPUT_KEYBRD_T ev = io_read(AM_INPUT_KEYBRD);
  if (ev.keycode == AM_KEY_NONE) {
    return 0;
  }

  char *keydown = ev.keydown ? "d" : "u";
  sprintf(buf, "k%s %s\n", keydown, keyname[ev.keycode]);
  return strlen(buf);
}

size_t dispinfo_read(void *buf, size_t offset, size_t len) {
  uint32_t size =
      (io_read(AM_GPU_CONFIG).width << 16) + io_read(AM_GPU_CONFIG).height;
  memcpy(buf, &size, 4); // sizeof(uint32_t) == 4

  return 4; // sizeof(uint32_t) == 4
}

// buf[0] = x; buf[1] = y; buf[2] = w; buf[3] = h; buf[4] = (uint32_t)pixels;
size_t fb_write(const void *buf, size_t offset, size_t len) {

  int x = ((uint32_t *)buf)[0], y = ((uint32_t *)buf)[1];
  int w = ((uint32_t *)buf)[2], h = ((uint32_t *)buf)[3];
  uint32_t *pixels = (uint32_t *)((uint32_t *)buf)[4];

  io_write(AM_GPU_FBDRAW, x * w, y * h, pixels, w, h, false);
  io_write(AM_GPU_FBDRAW, 0, 0, NULL, 0, 0, true);

  return 0;
}

void init_device() {
  Log("Initializing devices...");
  ioe_init();
}
