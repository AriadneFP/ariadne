#include <stdio.h>
#include <float.h>
double ariadne_add(double x,double y){
  double z,abs1;
  z = x+y;
  abs1 = fabs(z);
  if (DBL_MAX < abs1){
    printf("Overflow!\n");
    exit(0);
  }
  if (0 < abs1 && abs1 < DBL_MIN){
    printf("Underflow!\n");
    exit(0);
  }
  return z;
}
