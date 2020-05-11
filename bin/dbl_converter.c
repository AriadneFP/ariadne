#include <stdio.h>
#include <gmp.h>

int main (int argc, char *argv[]) {
  mpq_t value;
  mpq_init(value);
  int i;
  for ( i = 1; i<argc; i++) {
    mpq_set_str(value, argv[i], 10);
    double x = mpq_get_d(value);
    printf("%e ",x);
  }
}