#include <stdio.h>

double ariadne_div(double x, double y) {
  double z = (x*x + 1) / ( y*y*y - 1);
  return z;
}