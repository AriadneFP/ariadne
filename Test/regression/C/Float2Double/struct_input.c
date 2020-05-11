#include <gmp.h>
mpq_t deleteme;

struct foo_struct{
  float x;
  float y;
};
typedef struct foo_struct foo;

struct double_foo_struct{
  double x;
  double y;
};
typedef struct double_foo_struct double_foo;

void foo_function(foo);
//x = 1.0;
//y = 2.0;
//foo val = {x,y}; 
int somefunction() {
  double_foo d_dump = {1.0, 2.0};
  //float x = d_dump.x;
  //float y = d_dump.y;
  //foo dump = {x,y};
  foo dump = {(float)d_dump.x, (float)d_dump.y};
  foo_function(dump);
  //cfabs(&dump);
  return 0;
}