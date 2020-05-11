#include <gmp.h>
mpq_t deleteme;
double x = 1.0;
int main (void ) {
  double y = 3.0;
  double z = x + y;
  printf ("%f\n",z);
  return 0;
}