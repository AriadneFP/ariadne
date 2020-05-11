#include <stdio.h>
#include <gsl/gsl_sf_result.h>
/*#include <gsl/gsl_machine.h>
#include <config.h>*/
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
/*#include <gsl/gsl_sf_exp.h>
#include <gsl/gsl_sf_airy.h>*/

#include "error.h"
//#include "/home/thanh/ariadne/ndev/Test/gsl-1.14/specfunc/chebyshev.h"
//#include "/home/thanh/ariadne/ndev/Test/gsl-1.14/specfunc/cheb_eval_mode.c"


static
int
foo(const double x, gsl_sf_result * ampl, gsl_sf_result * phi)
{
  const double pi34 = 2.356194490192344928847;
  gsl_sf_result result_a;
  gsl_sf_result result_p;
  double a, p;
  double sqx;
  double x32;

  if(x <= -4.0) {
    double z = 128.0/(x*x*x) + 1.0;
    //cheb_eval_mode_e(&an20_cs, z, mode, &result_a);
    //cheb_eval_mode_e(&aph0_cs, z, mode, &result_p);
  }
  else if(x <= -2.0) {
    double z = (128.0/(x*x*x) + 9.0) / 7.0;
    //cheb_eval_mode_e(&an21_cs, z, mode, &result_a);
    //cheb_eval_mode_e(&aph1_cs, z, mode, &result_p);
  }
  else if(x <= -1.0) {
    double z = (16.0/(x*x*x) + 9.0) / 7.0;
    //cheb_eval_mode_e(&an22_cs, z, mode, &result_a);
    //cheb_eval_mode_e(&aph2_cs, z, mode, &result_p);
  }
  else {
    ampl->val = 0.0;
    ampl->err = 0.0;
    phi->val  = 0.0;
    phi->err  = 0.0;
    GSL_ERROR ("x is greater than 1.0", GSL_EDOM);
  }

  a =  0.3125 + result_a.val;
  p = -0.625  + result_p.val;

  sqx = sqrt(-x);
  x32   = x*sqx;

  ampl->val = sqrt(a * sqx);
  ampl->err = fabs(ampl->val) * (GSL_DBL_EPSILON + fabs(result_a.err/result_a.val));
  phi->val  = pi34 - x * sqx * p;
  phi->err = fabs(phi->val) * (GSL_DBL_EPSILON + fabs(result_p.err/result_p.val));

  return GSL_SUCCESS;
}

static
double tAdd( double x, double y) {
  return x + y;
}