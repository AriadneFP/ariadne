#include <gmp.h>

struct foo_struct{
  double x;
  double y;
};
typedef struct foo_struct foo;

struct mpq_foo_struct{
  mpq_t x;
  mpq_t y;
};
typedef struct mpq_foo_struct mpq_foo;

foo_function(foo* dump);

int somefunction(){
  mpq_t mpq_x;
  mpq_set_d(mpq_x, 1.0);
  mpq_t mpq_y;
  mpq_set_d(mpq_y, 1.0);
  mpq_foo mpq_dump = {mpq_x,mpq_y};
  double x = mpq_get_d(mpq_dump.x);
  double y = mpq_get_d(mpq_dump.y);
  foo dump = {x,y};  
  foo_function(&dump);
  return 0;
}