#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fenv.h>
#include <setjmp.h>
#include <signal.h>
#include <math.h>
#include <float.h>


/* floating-point error handler */
static void
catch_sigfpe(int sig, siginfo_t* code, void* v) {

	char* exception;
	char buffer[128];

	switch (code->si_code) {
		case FPE_FLTDIV:
			exception = "divide by zero";
			break;
		case FPE_FLTOVF:
			exception = "overflow";
			break;
		case FPE_FLTUND:
			exception = "underflow";
			break;
		case FPE_FLTRES:
			exception = "inexact";
			break;
		case FPE_FLTINV:
			exception = "invalid";
			break;
		default:
			sprintf(buffer, "general exception (code = %d)", code->si_code);
			exception = buffer;
			break;
	}

	printf ("*** SIGFPE taken at 0x%x ***\n", code->si_addr);
	printf ("Error = %s\n", exception);

	exit(1);
}

void
enable_fpe_traps() {
	struct sigaction act;
	act.sa_sigaction = catch_sigfpe;
	//if (sigemptyset (&(act.sa_mask))) printf ("sigemptyset\n");
	act.sa_flags = SA_SIGINFO;		/* want 3 args for handler */
	if (sigaction (SIGFPE, &act, NULL))	perror ("Bad sigaction call.\n");

	feenableexcept (
		FE_INEXACT | FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
	);
}

int main()
{

double test1, test2;

  enable_fpe_traps();
  //double y = 2*DBL_MAX;
  double x = sqrt(-2*DBL_MAX);
  //test1 = 0.0;
  //test2 = 3.14/test1;

  //test1 = -1.0;
  //test2 = sqrt(test1);

  return EXIT_SUCCESS;
}
