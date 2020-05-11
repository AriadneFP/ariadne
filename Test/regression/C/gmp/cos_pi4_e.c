#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>
//#include <gmp.h>
//mpq_t deleteme;

int
main (void)
{
  double x = 2.0;
  double y = 1.0;
  gsl_sf_result result;  
  
  const int val = gsl_sf_bessel_cos_pi4_e(x, y, &result);

  printf("J0(%g) = %.18e\n", x, result.val);

  return 0;
}

