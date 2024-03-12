#define SDL_malloc malloc
#define SDL_free free
#define SDL_realloc realloc

#define SDL_STBIMAGE_IMPLEMENTATION
#include "SDL_stbimage.h"
#include <stdio.h>

SDL_Surface *IMG_Load_RW(SDL_RWops *src, int freesrc) {
  assert(src->type == RW_TYPE_MEM);
  assert(freesrc == 0);
  return NULL;
}

SDL_Surface *IMG_Load(const char *filename) {
  FILE *img_file = fopen(filename, "r");
  fseek(img_file, 0, SEEK_END);
  uint32_t size = ftell(img_file);
  fseek(img_file, 0, SEEK_SET);
  void *buf = SDL_malloc(size);
  fread(buf, 1, size, img_file);
  SDL_Surface *surface = STBIMG_LoadFromMemory(buf, size);
  if (surface == NULL) {
    printf("STBIMG_LoadFromMemory failed: %s\n", SDL_GetError());
  }
  SDL_free(buf);
  fclose(img_file);
  return surface;
}

int IMG_isPNG(SDL_RWops *src) {
  return 0;
}

SDL_Surface* IMG_LoadJPG_RW(SDL_RWops *src) {
  return IMG_Load_RW(src, 0);
}

char *IMG_GetError() {
  return "Navy does not support IMG_GetError()";
}
