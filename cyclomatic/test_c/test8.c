#include <stdio.h>

int fun(a, b) {
  return a > b ? 0:1;
}

int main(void) {
  int x = fun(1, 2);
  int y = fun(2, 1);
  if(x) printf("x\n");
  if(y) print("y\n");
  return 0;
}
