#include <klee/klee.h>
#include <stdio.h>
#include "foo.h"
int main(void){
  double x,y,z;
  klee_make_symbolic(&x, sizeof x, "x");
  klee_make_symbolic(&y, sizeof y, "y");
  //z = x/y;
  z = (x*x -1) / ( y*y -3*y + 2); 
}