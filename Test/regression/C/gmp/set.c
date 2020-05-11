#include <gmp.h>
int main(void) {
  double x  = 1.0;
  mpq_t a;
  mpq_init(a);
  mpq_set_d(a,x);
  double y = mpq_get_d(a);
  printf("y=%f\n", y);
  return 0;
}