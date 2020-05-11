#include <stdio.h>
#include <float.h>
double ariadne_func(double x, double y) {
  const double BTR = 1.0;
  if(x < 1.0) {
<<<<<<< HEAD
    double z = 1.0/(x + y) + BTR;
=======
    double z = x/y + BTR;
>>>>>>> 90a9356... Add the inputs causing Z3 to hang.
    return 0.0;
  }
  return 0.0;
}