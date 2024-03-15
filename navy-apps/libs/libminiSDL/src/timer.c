#include <NDL.h>
#include <sdl-timer.h>
#include <stdint.h>
#include <stdio.h>

SDL_TimerID SDL_AddTimer(uint32_t interval, SDL_NewTimerCallback callback,
                         void *param) {
  return NULL;
}

int SDL_RemoveTimer(SDL_TimerID id) { return 1; }

uint32_t SDL_GetTicks() {
  // It should be reset or wrap every 49 days according to the doc.
  // But we don't care about it. No one will run the NEMU for 49 days.
  return NDL_GetTicks();
}

void SDL_Delay(uint32_t ms) {
  uint32_t start = SDL_GetTicks();
  while (SDL_GetTicks() - start < ms)
    ;
}
