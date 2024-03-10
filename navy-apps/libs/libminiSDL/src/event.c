#include <NDL.h>
#include <SDL.h>
#include <string.h>

#define keyname(k) #k,

static const char *keyname[] = {
  "NONE",
  _KEYS(keyname)
};

int SDL_PushEvent(SDL_Event *ev) {
  return 0;
}

int SDL_WaitEvent(SDL_Event *event) {

  char buf[64];
  while (1) {
    if (NDL_PollEvent(buf, sizeof(buf))) {
      break;
    }
  }

  switch (buf[1]) {
  case 'd':
    event->type = SDL_KEYDOWN;
    break;
  case 'u':
    event->type = SDL_KEYUP;
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
      event->key.keysym.sym = i;
      break;
    }

    i++;
  }

  return 1;
}

int SDL_PeepEvents(SDL_Event *ev, int numevents, int action, uint32_t mask) {
  return 0;
}

uint8_t* SDL_GetKeyState(int *numkeys) {
  return NULL;
}
