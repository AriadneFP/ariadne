#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <setjmp.h>
#include <signal.h>
#include <math.h>
#include <gmp.h>


#ifndef __SUPPORT_SNAN__
#define __SUPPORT_SNAN__
#endif

jmp_buf return_to_top_level;

fenv_t env;
fexcept_t flagp;
/* floating-point error handler */
void fperror()
{
  int set_excepts = fetestexcept(FE_DIVBYZERO | FE_UNDERFLOW |FE_OVERFLOW |FE_INVALID);

  if(set_excepts & FE_DIVBYZERO)
    printf("Divide By Zero!\n");

  if(set_excepts & FE_UNDERFLOW)
    printf("Underflow!\n");

  if(set_excepts & FE_OVERFLOW)
    printf("Overflow!\n");

  if(set_excepts & FE_INVALID)
    printf("Invalid!\n");
  
}

double divide(double x, double y) {
  double z = x/y;
  //fperror();
  double u = sqrt(z);
  //fperror();
  exit(1);
  return z;
}

int main(int argc, char *argv[])
{
  
  double x, y;
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_str(tmp,argv[1],10);
  x = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  y = mpq_get_d(tmp);
  double z = divide(x, y);
  fperror();
  return EXIT_SUCCESS;
  
}