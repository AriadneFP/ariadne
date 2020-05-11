#include <stdio.h>
#include <math.h>
#include <fenv.h>
//#define M_PI 3.14
int
Knu_scaled_asympx_e(const double nu, const double x)
{
  double pre = (2*x);
  //double mu   = 4.0*nu*nu;
  //double mum1 = mu-1.0;
  //double mum9 = mu-9.0;
  //double pre  = sqrt(M_PI/(2.0*x));
  //double r    = nu/x;
  //result->val = pre * (1.0 + mum1/(8.0*x) + mum1*mum9/(128.0*x*x));
  //result->err = 2.0 * GSL_DBL_EPSILON * fabs(result->val) + pre * fabs(0.1*r*r*r);
  //return GSL_SUCCESS;
  return 0;
}

/*int main(int argc, char* argv[])
{
   // disguise constants one and zero, in order to fool the compiler
   double x = atof(argv[1]);
   double y = atof(argv[2]);
   if (fetestexcept(FE_ALL_EXCEPT)) {
       printf("flag set before call.\n");
   }
   int result = Knu_scaled_asympx_e(x,y);
   
   if (fetestexcept(FE_OVERFLOW)) {
      printf("Overflow!\n");
   }
   if (fetestexcept(FE_UNDERFLOW)) {
      printf("Underflow!\n");
   }
   if (fetestexcept(FE_INVALID)) {
      printf("Invalid!\n");
   }
   if (fetestexcept(FE_DIVBYZERO)) {
      printf("Divide by zero!\n");
   }
   if (fetestexcept(FE_INEXACT)) {
      printf("Inexact!\n");
   }
}*/
