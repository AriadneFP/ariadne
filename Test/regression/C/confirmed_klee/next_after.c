#include <math.h>
#include <float.h>
#include <stdio.h>

int main(void) {
  double x = nextafter(-2.2250738585072009e-308, DBL_MAX);
  //double x = nextafter(DBL_MAX, 1.0/0.0);
  printf ("%e\n", x);
}