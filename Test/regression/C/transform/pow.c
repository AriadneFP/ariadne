#include <klee/klee.h>
int main(void){
  double x,y,z;
  klee_make_symbolic(&x, sizeof x, "x");
  klee_make_symbolic(&y, sizeof y, "y");
  z = pow(x,y);
  return 0;
}