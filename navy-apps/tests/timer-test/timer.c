#include <NDL.h>
#include <stdio.h>
#include <unistd.h>

int main() {
  NDL_Init(0);

  long last_print = 0;
  long now_ms = 0;

  // Print every 0.5 seconds
  while (1) {
    now_ms = NDL_GetTicks();

    for (int i = 0; i < 10000; i++)
      ;

    if (now_ms - last_print > 500) {
      printf("Hello Navy-apps at %ld micro sec!\n", now_ms);
      last_print = now_ms;
    }
  }

  NDL_Quit();

  return 0;
}
