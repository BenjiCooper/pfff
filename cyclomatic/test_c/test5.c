#include <stdio.h>

int function(int a, int b) {
  if (a > b) {
    return a + b;
  } else {
    return a*b;
  }
}

int main(void) {
  printf("%d", function(1,2));
  return 0;
}
