#include <stdio.h>
#include <sys/time.h>
#include <unistd.h>

int main() {
  struct timeval tv;
  long last_print = 0;
  long now_ms = 0;

  // Print every 0.5 seconds
  while (1) {
    gettimeofday(&tv, NULL);
    now_ms = tv.tv_usec;

    for (int i = 0; i < 10000; i++)
      ;

    if (now_ms - last_print > 500) {
      printf("Hello Navy-apps at %ld micro sec!\n", now_ms);
      last_print = now_ms;
    }
  }
  return 0;
}
