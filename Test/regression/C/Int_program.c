#include <klee/klee.h>
int main(void ){
  double x;
  double y;
  klee_make_symbolic(&x, sizeof x, "x");
  klee_make_symbolic(&y, sizeof y, "y");
  double z = x + y;
  if ( z > 1) 
    printf("Large!\n");
  else
    printf("Small!\n");
  return 0;
}