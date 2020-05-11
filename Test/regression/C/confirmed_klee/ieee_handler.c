#include <stdio.h>
#include <sys/ieeefp.h>
#include <sunmath.h>
#include <siginfo.h>
#include <ucontext.h>

void handler(int sig, siginfo_t *sip, ucontext_t *uap)
{
    unsigned    code, addr;

#ifdef i386
    unsigned    sw;

    sw = uap->uc_mcontext.fpregs.fp_reg_set.fpchip_state.status &
       ~uap->uc_mcontext.fpregs.fp_reg_set.fpship_state.state[0];
    if (sw & (1 << fp_invalid))
	code = FPE_FLTINV;
    else if (sw & (1 << fp_division))
	code = FPE_FLTDIV;
    else if (sw & (1 << fp_overflow))
	code = FPE_FLTOVF;
    else if (sw & (1 << fp_underflow))
	code = FPE_FLTUND;
    else if (sw & (1 << fp_inexact))
	code = FPE_FLTRES;
    else
	code = 0;
    addr = uap->uc_mcontext.fpregs.fp_reg_set.fpchip_state.state[3];
#else
    code = sip->si_code;
    addr = (unsigned) sip->si_addr;
#endif
    fprintf(stderr, "fp exception %x at address %x\n", code,
	addr);
}

int main()
{
    double  x;

    /* trap on common floating point exceptions */
    if (ieee_handler("set", "common", handler) != 0)
	printf("Did not set exception handler\n");

    /* cause an underflow exception (will not be reported) */
    x = min_normal();
    printf("min_normal = %g\n", x);
    x = x / 13.0;
    printf("min_normal / 13.0 = %g\n", x);

    /* cause an overflow exception (will be reported) */
    x = max_normal();
    printf("max_normal = %g\n", x);
    x = x * x;
    printf("max_normal * max_normal = %g\n", x);

    ieee_retrospective(stderr);
    return 0;
}