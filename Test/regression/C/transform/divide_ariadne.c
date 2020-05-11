#include <float.h>
double __ariadne_abs1;
double __ariadne_abs2;
double divide(double x , double y ) {
double z ;

if (y == 0) {
  if (x == 0) {
    printf("Invalid!\n");
  } else {
    printf("Division by Zero!\n");
  }
  exit(0);
}
__ariadne_abs1 = (double )fabs(x);
__ariadne_abs2 = (double )fabs(y);
if (__ariadne_abs1 > __ariadne_abs2 * DBL_MAX) {
  printf("Overflow!\n");
  exit(0);
}
if (0 < __ariadne_abs1 && __ariadne_abs1 < __ariadne_abs2 * DBL_MIN) {
  printf("Underflow!\n");
  exit(0);
}
z = x / y;
#line 15 "test.c"
return (z);
}