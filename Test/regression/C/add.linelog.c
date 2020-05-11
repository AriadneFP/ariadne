#include <float.h>
#include <stdio.h>
#include <gmp.h>
int lineno;

#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <signal.h>
#include <gmp.h>

mpq_t __deleteme;
	
mpq_t deleteme;

double fadd (double x, double y) 
{
{lineno = 9;}
  return (x + y);
}

double foo(double x) 
{
{lineno = 14;}
  return (1/(x*x +1));
}
