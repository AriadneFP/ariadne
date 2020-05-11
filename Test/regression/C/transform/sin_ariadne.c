#include <math.h>

double sin_ariadne ( double x) {
  double y;
  klee_make_symbolic (&y, sizeof y, "y");
  if ( -1 <= y && y <= 1)
    return y;
  else{
    exit(0);
  }
}