#include <math.h>
#include <float.h>

int main(int argc, char *argv[]) {
  double x = DBL_MAX;
  double y = nextafter(1.0, DBL_MAX);
  /*if ( y == 1.0)
    printf("Absorption!\n");*/
  double z = x*y;
  
  if ( x == z) 
    printf("Absorption!\n");
  
  return 1;    
}  