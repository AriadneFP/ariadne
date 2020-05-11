#include <gmp.h>
#include <fenv.h>
double fadd (double x, double y) {
  return (x + y);
}

int main(int argc, char* argv[])
{  
  double x, y, z;
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_str(tmp,argv[1],10);
  x = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  y = mpq_get_d(tmp);

  feenableexcept (
	FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
  );
  z = fadd(x,y);
  printf("%e\n",z);  
  return 0;
}
