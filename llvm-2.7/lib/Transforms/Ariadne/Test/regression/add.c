//#include <klee/klee.h>
#include "foo.h"
#include <float.h>
/*double fabs(double x){
  if (x > 0)
    return x;
  else return -x;
}*/
int main(void){
  double x,y,z;
  x = DBL_MAX;
  y = 11;
  //z = fabs(x);
  //klee_make_symbolic(&x, sizeof x, "x");
  //klee_make_symbolic(&y, sizeof y, "y");
  /*if ( x > 10.0 )
    printf("Large!\n");
  else
    printf("Small!\n");*/
  z = x + y;
  printf("%f\n%f\n",x,z);
  return 0;
}
