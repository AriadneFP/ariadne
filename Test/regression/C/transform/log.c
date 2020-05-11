#include <klee/klee.h>
int main(void){
  double x,y;
  klee_make_symbolic(&x, sizeof x, "x");
  y = log(x);
  return 0;
}