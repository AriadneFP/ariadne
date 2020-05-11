#include <gmp.h> // debugging
#include <stdio.h>
mpq_t deleteme;

static double foo1[3] = { 1.0, 2.0, 3.0};
static double foo2[3] = { 1.0, 2.0, 3.0};

//double x = 1.0;
static const double * foo[] = {
  0 ,
  foo1
};


int main(void) {
  //printf("Hello\n");
  printf("Value %f\n",foo[1][1]);
  return 0;
}  