#include <gmp.h>
mpq_t  deleteme;

struct foo_struct{
  mpq_t x;
  mpq_t y;
};
typedef struct foo_struct foo;

mpq_t a;
mpq_t b;

foo val = {a,b};