#include <stdio.h>

int function(int a, int b) {
  return a + b;
} 

int main(void) {
  int a = function(1,2);
  printf("%d", a);
}
