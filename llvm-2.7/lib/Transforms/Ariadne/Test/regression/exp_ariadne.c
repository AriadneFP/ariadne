#include <math.h>
#include <float.h>

double exp_ariadne (double x){
  double c1 = log(DBL_MAX);
  double c2 = log(DBL_MIN);
  if (x > c1){
    printf("Overflow!\n");
    exit(0);
  }
  if (x < c2){
    printf("Underflow!\n");
    exit(0);
  }
  else{
    double y;
    klee_make_symbolic(&y,sizeof y,"y");
    int check;
    check = (x == 0) && ( y == 1);
    if (check)
      return y;
    check = (x > 0 ) && ( y > 1);
    if (check)
      return y;
    check = (x < 0) && ( 0 < y ) && ( y < 1);
    if (check)
      return y;
    else{
      printf("Infeasible Path!\n");
      exit(0);
    }      
  }
}