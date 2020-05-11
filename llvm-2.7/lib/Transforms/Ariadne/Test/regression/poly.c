#include <klee/klee.h>
#include <stdio.h>
#include "foo.h"
int main(void){
  double x,y,z;
  klee_make_symbolic(&x, sizeof x, "x");
  //klee_make_symbolic(&y, sizeof y, "y");
  z = (x*x -1) / ( x*x -3*x + 2); 
}