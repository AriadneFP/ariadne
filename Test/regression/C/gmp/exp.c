#include <gmp.h>
#include <math.h>
mpq_t deleteme;
double foo add(double x, double y){
  return (x + y);
}
int main (void ) {
  double x = 1.0; 
  double y = 3.0;
  double z = add(x,y);
  printf ("%f\n",z);
  return 0;
}