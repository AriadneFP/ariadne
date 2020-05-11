#include <math.h>
#include <float.h>
double ariadne_log (double x){
  if ( x <= 0 ){
    printf("Invalid!");
    exit(0);
  }
  double c1 = exp(-DBL_MAX); 
   int check;
  check = ( 0 < x) && ( x < c1);
  if (check){
    printf("Overflow!\n");
    exit(0);
  }
  double c2 = exp(DBL_MIN);
  check = (1 < x) && (x < c2);
  if (check){
    printf("Underflow!\n");
    exit(0);
  }
  double y;
  klee_make_symbolic(&y, sizeof y, "y");
  check = (x == 1 ) && (y == 0);
  if (check)
    return y;
  check = (x > 1) && (y > 0);
  if (check)
    return y;
  check = (0 < x) && (x < 1) && ( y < 0);
  if (check)
    return y;
  else{
    printf("Infeasible path!\n");
    exit(0);
  }      
}