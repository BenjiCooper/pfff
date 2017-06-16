#include <stdio.h>

int fun(int *a) {
  a = &a*2;
  printf("%d", a);
}

int main(void) {
  int a = 1;
  fun(a);
  fun(a);
  fun(a);
  return 0;
}
