#include <stdio.h>
#include <math.h>
#include <fenv.h>
#include <gmp.h>
#include <float.h>

/*char * double2HexString(double a)
{
   char *buf = new char[17]; // double is 8-byte long, so we have 2*8 + terminating \0
   char *d2c;
   d2c = (char *) &a;
   char *n = buf;
   int i;
   for(i = 0; i < 8; i++)
   {
      sprintf(n, "%02X", *d2c++);
      n += 2;
   } 
   *(n) = '\0';
}*/

/*char *doubleToRawString(double x) {
    // Assumes sizeof(long long) == 8.

    char *buffer = new char[32];
    sprintf(buffer, "%llx", *(unsigned long long *)&x);  // Evil!
    return buffer;
}*/

int
Knu_scaled_asympx_e(const double nu, const double x)
{
  //double pre = sqrt(1.0/(2*x));
  //double mu   = 4.0*nu*nu;
  //double mum1 = mu-1.0;
  //double mum9 = mu-9.0;
  //double tmp = (x*x*x -3*x +2) / (x*x -1) ;
  double tmp = nu*nu;
  double y = tmp + 2*x;
  return 0;
}

/*int
Knu_scaled_asympx_e(const double nu, const double x)
{
  //double pre = sqrt(1.0/(2*x));
  //double mu   = 4.0*nu*nu;
  //double mum1 = mu-1.0;
  //double mum9 = mu-9.0;
  double tmp =  2.0 * x;
  
  printf("tmp: %.16e\n", x);
  //printf("tmp: %.16e\n", DBL_MAX/2);
  //printf("tmp1: %.16e\n", tmp1);
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
  if (x > DBL_MAX/2.0)
    printf("X greater than dbl_max/2!\n");
  
  if (tmp1 < DBL_MIN)
    printf("Less than dbl_min!\n");
  if (tmp1 >=  DBL_EPSILON)
    printf("Not less than denorm_dbl_min!\n");
  double tmp1 = 3.14/ tmp;
  double pre = sqrt(tmp1);
  //double pre  = sqrt(3.14/(2.0*x));
  //printf("x = %lf\n", x);
  //printf("2*x = %lf\n", 2*x);
  //printf("pre = %lf\n", pre);
  //double r    = nu/x;
  //result->val = pre * (1.0 + mum1/(8.0*x) + mum1*mum9/(128.0*x*x));
  //result->err = 2.0 * GSL_DBL_EPSILON * fabs(result->val) + pre * fabs(0.1*r*r*r);
  //return GSL_SUCCESS;
  return 0;
}*/

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
   y = nextafter(x, DBL_MAX);
   //y = DBL_MAX;   
   //printf("%s\n", doubleToRawString(y));
  if (2*y == -DBL_MAX)
     printf("Round down!\n");
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
  return result;  
}*/