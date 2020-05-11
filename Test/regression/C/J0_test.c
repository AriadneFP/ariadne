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

#include <gmp.h>
#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>

mpq_t __ariadne_deleteme_;

int
main (void)
{
  double x = 5.0;

  double y = gsl_sf_bessel_J0 (x);

  printf("J0(%g) = %.18e\n", x, y);

  return 0;
}

