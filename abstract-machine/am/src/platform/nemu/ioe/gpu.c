#include <am.h>
#include <nemu.h>

#define SYNC_ADDR (VGACTL_ADDR + 4)
uint32_t screen_w;
uint32_t screen_h;
uint32_t *fb = NULL;

void __am_gpu_init() {
  // ref: vgactl_port_base[0] = (screen_width() << 16) | screen_height();
  uint32_t size = inl(VGACTL_ADDR);
  screen_w = size >> 16;
  screen_h = size & 0xffff;
  fb = (uint32_t *)(uintptr_t)FB_ADDR;
}

void __am_gpu_config(AM_GPU_CONFIG_T *cfg) {
  uint32_t size = inl(VGACTL_ADDR);
  int w = size >> 16, h = size & 0xffff;
  *cfg = (AM_GPU_CONFIG_T){.present = true,
                           .has_accel = false,
                           .width = w,
                           .height = h,
                           .vmemsz = 0};
}

void __am_gpu_fbdraw(AM_GPU_FBDRAW_T *ctl) {
  if (ctl->sync) {
    outl(SYNC_ADDR, 1);
  }
  uint32_t *px = ctl->pixels;
  int x = ctl->x, y = ctl->y, w = ctl->w, h = ctl->h;
  int W = screen_w, H = screen_h;
  int i;
  for (i = 0; i < h && y + i < H; i++) {
    int j;
    for (j = 0; j < w && x + j < W; j++) {
      fb[(y + i) * W + x + j] = px[i * w + j];
    }
  }
}

void __am_gpu_status(AM_GPU_STATUS_T *status) { status->ready = true; }
