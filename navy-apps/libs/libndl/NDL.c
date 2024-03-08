#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

static int evtdev = -1;
static int fbdev = -1;
static int screen_w = 0, screen_h = 0;
static struct timeval *tv = NULL;

uint32_t NDL_GetTicks() {
  if (tv == NULL) {
    printf("NDL_GetTicks: NDL not init.\n");
    return 0;
  }

  gettimeofday(tv, NULL);
  return tv->tv_usec;
}

int NDL_PollEvent(char *buf, int len) {
  if (evtdev == -1) {
    printf("NDL_PollEvent: NDL not init or event device not exist.\n");
    return 0;
  }

  memset(buf, 0, len);
  if (read(evtdev, buf, len)) {
    return 1;
  }

  return 0;
}

void NDL_OpenCanvas(int *w, int *h) {
  if (fbdev == -1) {
    printf("NDL_OpenCanvas: NDL not init.\n");
    return;
  }

  if (getenv("NWM_APP")) {
    int fbctl = 4;
    fbdev = 5;
    screen_w = *w;
    screen_h = *h;
    char buf[64];
    int len = sprintf(buf, "%d %d", screen_w, screen_h);
    // let NWM resize the window and create the frame buffer
    write(fbctl, buf, len);
    while (1) {
      // 3 = evtdev
      int nread = read(3, buf, sizeof(buf) - 1);
      if (nread <= 0)
        continue;
      buf[nread] = '\0';
      if (strcmp(buf, "mmap ok") == 0)
        break;
    }
    close(fbctl);
  }

  fbdev = open("/dev/fb", O_RDWR);
  uint32_t size = 0;
  read(fbdev, &size, 0);
  uint32_t fb_width = size >> 16;
  uint32_t fb_height = size & 0xffff;

  if (*w == 0 && *h == 0) {
    *w = fb_width;
    *h = fb_height;
  }

  screen_w = *w;
  screen_h = *h;

  return;
}

void NDL_DrawRect(uint32_t *pixels, int x, int y, int w, int h) {
  switch (fbdev) {
  case -1:
    printf("NDL_DrawRect: NDL not init.\n");
    return;
  case -2:
    printf("NDL_DrawRect: Canvas not open.\n");
    return;
  default:
    break;
  }

  uint32_t *buf = (uint32_t *)malloc(5 * sizeof(uint32_t));
  buf[0] = x;
  buf[1] = y;
  buf[2] = w;
  buf[3] = h;
  buf[4] = (uint32_t)pixels;

  write(fbdev, buf, 0);
}

void NDL_OpenAudio(int freq, int channels, int samples) {}

void NDL_CloseAudio() {}

int NDL_PlayAudio(void *buf, int len) { return 0; }

int NDL_QueryAudio() { return 0; }

int NDL_Init(uint32_t flags) {
  if (getenv("NWM_APP")) {
    evtdev = 3;
  } else {
    evtdev = open("/dev/events", O_RDONLY);
  }

  fbdev = -2;
  tv = (struct timeval *)malloc(sizeof(struct timeval));
  return 0;
}

void NDL_Quit() {
  if (evtdev != -1) {
    close(evtdev);
    evtdev = -1;
  }
  if (fbdev != -1) {
    close(fbdev);
    fbdev = -1;
  }
  if (tv != NULL) {
    free(tv);
  }
}
