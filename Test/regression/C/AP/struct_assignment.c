#include <stdio.h>
#include <gsl/gsl_sf_result.h>
#include <gsl/gsl_machine.h>
#include <gmp.h>

mpq_t deleteme;

int foo(int n, double x, gsl_sf_result * result) {
  gsl_sf_result b;
  //int status = gsl_sf_bessel_IJ_taylor_e((double)n, x, -1, 50, GSL_DBL_EPSILON, &b);
  int status = gsl_sf_bessel_IJ_taylor_e((double)n, 1.0, -1, 50, GSL_DBL_EPSILON, &b);
  //return status;
  //result->val  = b.val;  
  return 0;
}
