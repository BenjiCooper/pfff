#include <stdio.h>

void fun1(int a) {
  printf("%d", a);
}

void fun2(int a) {
  printf("%d", a*2);
}

int main(void) {
  fun1(1);
  fun2(2);
  return 0;
}
