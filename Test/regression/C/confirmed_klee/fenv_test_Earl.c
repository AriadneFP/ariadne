#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <signal.h>
#include <math.h>
#include <gmp.h>
#include <gsl/gsl_sf_gamma.h>
#include <gsl/gsl_sf_legendre.h>
#include <float.h>

struct sigaction tmp1;
siginfo_t tmp2;
/* floating-point error handler */
static void
catch_sigfpe(int sig, siginfo_t* code, void* v) {

	//char* exception;
	//char buffer[128];	
	
	switch (code->si_code) {
		case FPE_FLTDIV:
			printf("Divide By Zero!\n");
			break;
		case FPE_FLTOVF:
			printf("Overflow!\n");
			break;
		case FPE_FLTUND:
			printf("Underflow!\n");
			break;
		case FPE_FLTRES:
			printf("Inexact!\n");
			break;
		case FPE_FLTINV:
			printf("Invalid!\n");
			break;
		default:
			//sprintf(buffer, "general exception (code = %d)", code->si_code);
			//exception = buffer;
			break;
	}

	//printf ("*** SIGFPE taken at 0x%x ***\n", code->si_addr);
	//printf ("%s\n", exception);
	exit(1);
	return;
}

void
enable_fpe_traps() {
	struct sigaction act;
	act.sa_sigaction = catch_sigfpe;
	//if (sigemptyset (&(act.sa_mask))) printf ("sigemptyset\n");
	act.sa_flags = SA_SIGINFO;		/* want 3 args for handler */
	if (sigaction (SIGFPE, &act, NULL))	printf ("Bad sigaction call.\n");

	feenableexcept (
		FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
	);
}

double divide(double x, double y) {
  double z = sqrt(x);
  return x/y;
}

int main(int argc, char* argv[])
{
  
  /*double x, y, z, t;
  mpq_t tmp;
  mpq_init(tmp);
  mpq_set_str(tmp,argv[1],10);
  x = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  y = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[1],10);
  z = mpq_get_d(tmp);
  mpq_set_str(tmp,argv[2],10);
  t = mpq_get_d(tmp);
  gsl_sf_result result;
  double *u; */
  //double x = 0.0;
  //enable_fpe_traps();
  feenableexcept (
	FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
  );
  double x = DBL_MIN/2.0;
  printf("%f\n", x);
  //double y = -x*x/6.0;  
  //gsl_sf_conicalP_xgt1_neg_mu_largetau_e (x,y,z,t, &result,u);
  //gamma_inc_Q_series(x,y, &result);
  //double z = divide(x,y);  
  return EXIT_SUCCESS;
}
