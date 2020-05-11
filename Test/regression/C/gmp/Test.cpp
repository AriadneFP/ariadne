#include <gmp.h>
#include "Initializer.c"

int main (void) {
  double val = mpq_get_d(array[0]);
  printf("%f\n", val);
}