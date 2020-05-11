#include <stdio.h>
#include <float.h>
double ariadne_exp(double eps) {
  const double c0 =  0.07721566490153286061;
  const double c1 =  0.08815966957356030521;
  const double c2 = -0.00436125434555340577;
  const double c3 =  0.01391065882004640689;
  const double c4 = -0.00409427227680839100;
  /*const double c5 =  0.00275661310191541584;
  const double c6 = -0.00124162645565305019;
  const double c7 =  0.00065267976121802783;
  const double c8 = -0.00032205261682710437;
  const double c9 =  0.00016229131039545456;*/
  const double g4 = c0 + eps*(c1 + eps*(c1 + eps*(c2 + eps*(c3 + eps*c4))));
  /*const double g5 = c5 + eps*(c6 + eps*(c7 + eps*(c8 + eps*c9)));
  const double g  = eps*(c0 + eps*(c1 + eps*(c2 + eps*(c3 + eps*(c4 + eps*g5)))));*/

  /* calculate eps gamma(-1+eps), a negative quantity */
  //const double gam_e = g - 1.0 - 0.5*eps*(1.0+3.0*eps)/(1.0 - eps*eps);

  //double z = log(fabs(gam_e)/fabs(eps));
  double z = log(g4);
  return z;
}