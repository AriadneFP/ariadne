#include <stdio.h>
#include <math.h>

double pow_ariadne(double x, double y){
  double z;
  if ((x <= 0) && (y <= 0) ){
    printf("Invalid!");
    exit(0);
  }
  
  klee_make_symbolic(&z, sizeof z,"z");
  
  if ((x == 0) && (z ==0))
    return z;
  int check;
  check = (0 < x) && ( x <= 1) && ( 0 < z ) && ( z <= 1);
  if (check)
    return z;
  check = ( x >1) && ( z > 1);
  if (check)
    return z;
  else {
    printf("Infeasible path!\n");
    exit ( 0 );
  }
    
}