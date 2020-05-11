#include <stdio.h>
#include <fenv.h>
#include <signal.h>
#include <gmp.h>
#include <setjmp.h>
#define printIfSet(status, x) if(status & x) printf(#x " was set.\n");

jmp_buf return_to_top_level;

static void FPExceptionHandler(int signalNumber);

double divide(double x, double y) {
  return x/y;
}

int main(int argc, char* argv[])
{
  /*
  Initialization code......
  */
    
  //feenableexcept(FE_ALL_EXCEPT);/* enable the traps for all excepts.*/
  feenableexcept(FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID | FE_DIVBYZERO);
  signal(SIGFPE, FPExceptionHandler);
  feclearexcept(FE_ALL_EXCEPT);/* make sure we have a clean start */
  
  /* enableing floating point exception signaling */
  
  /*
  Call a function that will cause an exception.
  The reason of the exception is unknown and that's why I
  need the traps in the first place.
  */
  
  double x, y;
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_str(tmp,argv[1],10);
  x = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  y = mpq_get_d(tmp);      
  double z = divide(x,y);
  
  //FPExceptionHandler(0);
  return 0;
}

static void FPExceptionHandler(int signalNumber)
{
  //printf ("%d\n", signalNumber);
  /* Obtain the set of exceptions that were raised */
  /* Unfortunately, none of the flags are set */
  fenv_t env;
  fexcept_t flagp;
  fegetexceptflag(&flagp,FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW);
  fesetexceptflag(&flagp,FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW);
  int result = fetestexcept(FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID | FE_DIVBYZERO);
  printf("Inside FP exception handler.\n");
  //Print out which exception was raised 
  /*printIfSet(result, FE_INEXACT);
  printIfSet(result, FE_DIVBYZERO);
  printIfSet(result, FE_UNDERFLOW);
  printIfSet(result, FE_OVERFLOW);
  printIfSet(result, FE_INVALID);*/
  
  if (result & FE_OVERFLOW) {
      printf("Overflow!\n");
  }
  if (result & FE_UNDERFLOW) {
      printf("Underflow!\n");
  }
  if (result & FE_INVALID) {
      printf("Invalid!\n");
  }
  if (result & FE_DIVBYZERO) {
      printf("Divide by zero!\n");
  }  
  if (result & FE_INEXACT) {
      printf("Inexact!\n");
  }
  
  /*if (fetestexcept(FE_OVERFLOW)) {
      printf("Overflow!\n");
  }
  if (fetestexcept(FE_UNDERFLOW)) {
      printf("Underflow!\n");
  }
  if (fetestexcept(FE_INVALID)) {
      printf("Invalid!\n");
  }
  if (fetestexcept(FE_DIVBYZERO)) {
      printf("Divide by zero!\n");
  }
  if (fetestexcept(FE_INEXACT)) {
      printf("Inexact!\n");
  }*/
  
  /* Revert back to the default action, and raise signal to
  terminate the program */  
  signal(SIGFPE, SIG_DFL);
  raise(SIGFPE);  
}

/*int main()
{
   // disguise constants one and zero, in order to fool the compiler
   double x = (double)printf("") + 1.0; // one
   double y = (double)printf(""); // zero
   if (fetestexcept(FE_ALL_EXCEPT)) {
       printf("flag set before call.\n");
   }
   double z = x / y;
   //feraiseexcept(FE_ALL_EXCEPT);
   if (fetestexcept(FE_OVERFLOW)) {
      printf("Overflow!\n");
   }
   if (fetestexcept(FE_UNDERFLOW)) {
      printf("Underflow!\n");
   }
   if (fetestexcept(FE_INVALID)) {
      printf("Invalid!\n");
   }
   if (fetestexcept(FE_DIVBYZERO)) {
      printf("Divide by zero!\n");
   }
   if (fetestexcept(FE_INEXACT)) {
      printf("Inexact!\n");
   }
   printf("%f/%f==%f\n",x,y,z);
}*/