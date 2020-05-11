#include <klee/klee.h>
#include <stdio.h>
#include "foo.h"
int main(void){
  double x,y,z,u;
  z = fabs(x);
  klee_make_symbolic(&x, sizeof x, "x");
  klee_make_symbolic(&y, sizeof y, "y");
  z = x / y;
  if ( z > 1.0 ) 
    printf("Large!\n");
  return 0;
}