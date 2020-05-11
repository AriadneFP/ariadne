/*************************************************************/
#include <stdio.h>
#include <math.h>
#include <fenv.h>
#include <signal.h>
 
int traps;
fexcept_t flags;
 
/* signal handler for floating-point exceptions */
void handle_sigfpe(int signo, siginfo_t *siginfo, 
                   ucontext_t *ucontextptr)
{
    void print_flags(int);
    void print_traps(int);
 
    printf("Raised signal %d -- floating point error\n", signo);
    printf("code is %d\n", siginfo->si_code);
    printf("fr0L is %08x\n", ucontextptr->uc_mcontext.ss_frstat);
    print_traps(traps);
    fegetexceptflag(&flags, FE_ALL_EXCEPT);
    print_flags(flags);
    exit(-1);
}
 
int main(void)
{
    void print_flags(int);
    void print_traps(int);
    double x, y, z;
    struct sigaction act;
 
    act.sa_sigaction = (void (*)(int, siginfo_t *, 
                                 void *))&handle_sigfpe; 
    act.sa_flags = SA_SIGINFO;
 
    /* establish the signal handler */
    sigaction(SIGFPE, &act, NULL);
 
    fesettrapenable(FE_OVERFLOW);
    traps = fegettrapenable();
    print_traps(traps);
 
    x = 1.79e308;
    y = 2.2e-308;
    z = x / y;    /* divide very big by very small */
    printf("%g / %g = %g\n", x, y, z);
}                                               /* continued */
/*************************************************************/

/*************************************************************/
void print_flags(int flags)
{
    if (flags & FE_INEXACT)
      printf(" inexact result flag set\n");
    if (flags & FE_UNDERFLOW)
      printf(" underflow flag set\n");
    if (flags & FE_OVERFLOW)
      printf(" overflow flag set\n");
    if (flags & FE_DIVBYZERO)
      printf(" division by zero flag set\n");
    if (flags & FE_INVALID)
      printf(" invalid operation flag set\n");
 
    if (!(flags & FE_ALL_EXCEPT))
        printf(" no exception flags are set\n");
}
 
void print_traps(int traps)
{
    if (traps & FE_INEXACT)
      printf(" inexact result trap set\n");
    if (traps & FE_UNDERFLOW)
      printf(" underflow trap set\n");
    if (traps & FE_OVERFLOW)
      printf(" overflow trap set\n");
    if (traps & FE_DIVBYZERO)
      printf(" division by zero trap set\n");
    if (traps & FE_INVALID)
      printf(" invalid operation trap set\n"); 
    if (!(traps & FE_ALL_EXCEPT))
      printf(" no trap enables are set\n");
}
/*************************************************************/