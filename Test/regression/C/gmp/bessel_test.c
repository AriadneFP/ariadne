/*
   Example taken from the GNU Scientific Library Reference Manual
    Edition 1.1, for GSL Version 1.1
    9 January 2002
   URL: gsl/ref/gsl-ref_23.html#SEC364
*/

/* 
  Compile and link with:
    gcc -c J0_test.c
    gcc -o J0_test J0_test.o -lgsl -lgslcblas -lm
*/    
    
/* The answer should be J0(5) = -1.775967713143382920e-01 */


#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>

int main(void) {
  gsl_sf_result result;
  double x = 0.0;
  double y = 1.0;
  int val = gsl_sf_bessel_cos_pi4_e(x,y,&result);
  printf("%f\n", result.val);
}
