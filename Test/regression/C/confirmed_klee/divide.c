#include <gmp.h>
#include <fenv.h>
#include <stdio.h>

double divide(double x, double y) {
  //double z = sqrt(x);
  //return z;
  //return x/y;
  double z = x/y;
  //double z = exp(x);
  return z;
}

double exp(double x) {
  return exp(x);
}

/*int main(int argc, char* argv[])
{
   double x, y;
   //sscanf(argv[1], "%lf", &x);
   //sscanf(argv[2], "%lf", &y);
   //double x = atof(argv[1]);
   //double y = atof(argv[2]);
   mpq_t tmp;
   mpq_init(tmp);
   mpq_set_str(tmp,argv[1],10);
   x = mpq_get_d(tmp);
   mpq_set_str(tmp,argv[2],10);
   y = mpq_get_d(tmp);
   //y = nextafter(x, DBL_MAX);
      
  double z = divide(x,y);
  
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
   
  return 0;
}*/
