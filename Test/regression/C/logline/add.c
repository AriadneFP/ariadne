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

/* floating-point error handler */
static void
catch_sigfpe(int sig, siginfo_t* code, void* v) {

	char* ex;
	char buffer[128];

	switch (code->si_code) {
		case FPE_FLTDIV:
			ex = "divide by zero";
			break;
		case FPE_FLTOVF:
			ex = "overflow";
			break;
		case FPE_FLTUND:
			ex = "underflow";
			break;
		case FPE_FLTRES:
			ex = "inexact";
			break;
		case FPE_FLTINV:
			ex = "invalid";
			break;
		default:
			sprintf(buffer, "general exception (code = %d)", code->si_code);
			ex = buffer;
			break;
	}

	printf ("Error: %s floating-point exception at line %d.\n", ex, lineno);
	exit(1);
}

	

void
enable_fp_traps() {
	struct sigaction act;
	act.sa_sigaction = catch_sigfpe;
	if (sigemptyset (&(act.sa_mask))) printf ("sigemptyset\n");
	act.sa_flags = SA_SIGINFO;		/* want 3 args for handler */
	if (sigaction (SIGFPE, &act, NULL))	perror ("Bad sigaction call.\n");

	feenableexcept (
		FE_INEXACT | FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW
	);
}

	
mpq_t deleteme;

double fadd (double x, double y) 
{
{lineno = 8;}
  return (x + y);
{lineno = 9;}
}

double foo(double x) 
{
{lineno = 13;}
  return (1/(x*x +1));
{lineno = 14;}
}
