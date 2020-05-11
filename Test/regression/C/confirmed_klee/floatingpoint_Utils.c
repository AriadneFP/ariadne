#include <float.h>
#include <math.h>
#include <gmp.h>
#include <stdio.h>

double getMinNonAbsorption( double x, double y) {
  while ( x + y == y) {
    x = 2.0 * x;
  }
  double end = x;
  double start = x/2.0;
  
  while ( start + y != y) {
    double mid = (start + end) / 2.0;
    if (mid == start || mid == end)
      break;
    if ( mid + y != y )
      end = mid;
    else 
      start = mid;
  }
  return end;  
}

int main(int argc, char* argv[])
{
  double x, y;
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_str(tmp,argv[1],10);
  x = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  y = mpq_get_d(tmp);      
  double z = getMinNonAbsorption(x,y);
  printf("%lf\n", z);
  return 0;
}
