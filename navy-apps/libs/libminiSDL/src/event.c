#include <NDL.h>
#include <SDL.h>
#include <string.h>

#define keyname(k) #k,

static const char *keyname[] = {"NONE", _KEYS(keyname)};

static uint8_t keystate[81];

int SDL_PushEvent(SDL_Event *ev) { return 0; }

int SDL_PollEvent(SDL_Event *ev) {

  char buf[64];
  NDL_PollEvent(buf, sizeof(buf));

  switch (buf[1]) {
  case 'd':
    ev->type = SDL_KEYDOWN;
    break;
  case 'u':
    ev->type = SDL_KEYUP;
    break;
  default:
    return 0;
  }

  int i = 0;
  while (1) {
    if (buf[i] != '\n') {
      i++;
    } else {
      buf[i] = '\0';
      break;
    }
  }

  i = 0;
  while (i < 81) {
    if (strcmp(buf + 3, keyname[i]) == 0) {
      ev->key.keysym.sym = i;
      keystate[i] = ev->type == SDL_KEYDOWN ? 1 : 0;
      return 1;
    }

    i++;
  }
  return 0;
}

int SDL_WaitEvent(SDL_Event *event) {

  while (1) {
    if (SDL_PollEvent(event)) {
      break;
    }
  }

  return 1;
}

int SDL_PeepEvents(SDL_Event *ev, int numevents, int action, uint32_t mask) {
  return 0;
}

uint8_t *SDL_GetKeyState(int *numkeys) { return keystate; }
