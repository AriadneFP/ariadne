#include <gmp.h>
struct foo_struct {
  int a;
  mpq_t data;
};
typedef struct foo_struct foo;
mpq_t x;
//foo val = {1, x};
int main(void) {
  //mpq_init(x);
  //mpq_set_d(x,1.0);
  foo val = {1, x};
  mpq_init(val.data);
  mpq_set_d(val.data,1.0);
  //val.data = x;
  double y = mpq_get_d(val.data);
  double z = mpq_get_d(x);
  printf("value: %d %f\n",val.a,y);
  return 0;
}

