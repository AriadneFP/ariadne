#include <stdio.h>
#include <float.h>
double ariadne_sqrt(double x, double y) {
  //const double ATR =  8.7506905708484345;
  //const double BTR = -2.0938363213560543;
  const double ATR = 1.0;
  const double BTR = -1.0;

  if(x < 1) {
    //double sqx = sqrt(x);
    //double z   = ATR/(x*sqx) + BTR;
    //double z   = ATR/(x*y) + BTR;
    double z = x/y + BTR;
    //double y   = sqrt(sqx);
    /*gsl_sf_result result_c;
    cheb_eval_mode_e(&bip_cs, z, mode, &result_c);
    result->val = (0.625 + result_c.val)/y;
    result->err = result_c.err/y + GSL_DBL_EPSILON * fabs(result->val);
    result->val = sqx;*/
    return -1.0;
  }
  /*else {
    double sqx = sqrt(x);
    double z   = 16.0/(x*sqx) - 1.0;
    gsl_sf_result result_c;
    cheb_eval_mode_e(&bip2_cs, z, mode, &result_c);
    result->val = (0.625 + result_c.val)/y;
    result->err = result_c.err/y + GSL_DBL_EPSILON * fabs(result->val);
    return 1.0;
  }*/
}