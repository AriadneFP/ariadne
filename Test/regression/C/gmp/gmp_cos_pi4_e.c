#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>
#include <gmp.h>

struct mpq_result_struct{
  mpq_t val;
  mpq_t err;
};

typedef struct mpq_result_struct mpq_result;
int
main (void)
{
  mpq_t x;
  mpq_t y;
  mpq_init(x);
  mpq_init(y);
  mpq_set_d(x, 2.0);
  mpq_set_d(y, 1.0);
  
  mpq_result result;
  mpq_init(result.val);
  mpq_init(result.err);
  
  gsl_sf_result result_tmp;
  result_tmp.val = mpq_get_d(result.val);
  result_tmp.err = mpq_get_d(result.err);
  
  double x_tmp = mpq_get_d(x);
  double y_tmp = mpq_get_d(y);  
  
  const int val = gsl_sf_bessel_cos_pi4_e(x_tmp, y_tmp, &result_tmp);
  
  printf("J0(%g) = %.18e\n", x_tmp, result_tmp.val);

  return 0;
}

