#include <am.h>
#include <ysyxsoc.h>

uint32_t am_screen_w = 640;
uint32_t am_screen_h = 480;

void __am_gpu_config(AM_GPU_CONFIG_T *cfg) {
  cfg->present = true;
  cfg->has_accel = false;
  cfg->width = am_screen_w;
  cfg->height = am_screen_h;
  cfg->vmemsz = 0;
}

void __am_gpu_fbdraw(AM_GPU_FBDRAW_T *ctl) {
  uint32_t *px = ctl->pixels;
  int x = ctl->x, y = ctl->y, w = ctl->w, h = ctl->h;
  int W = am_screen_w, H = am_screen_h;
  for (int i = 0; i < h && y + i < H; i++) {
    for (int j = 0; j < w && x + j < W; j++) {
      outl(VGA_FB_ADDR + ((y + i) * W + x + j) * 4, px[i * w + j]);
    }
  }
}

void __am_gpu_status(AM_GPU_STATUS_T *status) { status->ready = true; }
