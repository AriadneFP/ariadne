#include<stdio.h>

int foo(double x, double y){
  double z;
  int val = gsl_sf_bessel_asymp_Mnu_e(x, y, &z);
  return 0;
}