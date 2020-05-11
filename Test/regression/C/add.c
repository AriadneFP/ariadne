#include <float.h>
#include <stdio.h>

double fadd (double x, double y) 
{
  return (x + y);
}

/*int main() {
  int x,y;
  klee_make_symbolic( &x, sizeof(x), "x");
  klee_make_symbolic( &y, sizeof(y), "y");
  int z = x + y;
  if ( z > 1 ) {
    printf("Large!\n");
  } else
    printf("Small!\n");
}*/
