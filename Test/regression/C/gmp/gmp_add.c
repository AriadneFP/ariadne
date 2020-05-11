#include <stdio.h>
#include <gmp.h>

int main(void) {
   mpq_t x;
   mpq_t y;
   mpq_t z;
   mpq_init(x);
   mpq_init(y);
   mpq_init(z);
   mpq_set_str (x,"1/2",10);
   mpq_set_str (y,"3/4",10);
   mpq_add (z,x,y);
   double val = mpq_get_d(z);
   printf("%f\n", val);
}