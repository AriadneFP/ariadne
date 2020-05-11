#include <klee/klee.h>
#include <stdio.h>
#include "foo.h"
int main(void){
  double x,y;
  klee_make_symbolic(&x, sizeof x, "x");
  y = sqrt(x);
  if ( y > 1)
    printf("Large!\n");
  return 0;
}