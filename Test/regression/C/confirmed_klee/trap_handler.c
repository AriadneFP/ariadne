/*
 * This code demonstates a working floating-point exception
 * trap handler. The handler simply identifies which
 * floating-point exceptions caused the trap and return.
 * The handler will return the default signal return
 * mechanism longjmp().
 */

#include <signal.h>
#include <setjmp.h>
#include <fpxcp.h>
#include <fptrap.h>
#include <stdlib.h>
#include <stdio.h>

#define EXIT_BAD  -1
#define EXIT_GOOD  0

/*
 * Handshaking variable with the signal handler. If zero,
 * then the signal hander returns via  the default signal
 * return mechanism; if non-zero, then the signal handler
 * returns via longjmp.
 */
static int fpsigexit;
#define SIGRETURN_EXIT 0
#define LONGJUMP_EXIT  1

static jmp_buf jump_buffer;      /* jump buffer */
#define JMP_DEFINED 0            /* setjmp rc on initial call */
#define JMP_FPE     2            /* setjmp rc on return from */
                                 /* signal handler */

/*
 * The fp_list structure allows text descriptions
 * of each possible trap type to be tied to the mask
 * that identifies it.
 */

typedef struct
  {
  fpflag_t mask;
  char     *text;
  } fp_list_t;

/* IEEE required trap types */

fp_list_t
trap_list[] =
  {
      { FP_INVALID,      "FP_INVALID"},
      { FP_OVERFLOW,     "FP_OVERFLOW"},
      { FP_UNDERFLOW,    "FP_UNDERFLOW"},
      { FP_DIV_BY_ZERO,  "FP_DIV_BY_ZERO"},
      { FP_INEXACT,      "FP_INEXACT"}
  };

/* INEXACT detail list -- this is an system extension */

fp_list_t
detail_list[] =
  {
      { FP_INV_SNAN,   "FP_INV_SNAN" } ,
      { FP_INV_ISI,    "FP_INV_ISI" } ,
      { FP_INV_IDI,    "FP_INV_IDI" } ,
      { FP_INV_ZDZ,    "FP_INV_ZDZ" } ,
      { FP_INV_IMZ,    "FP_INV_IMZ" } ,
      { FP_INV_CMP,    "FP_INV_CMP" } ,
      { FP_INV_SQRT,   "FP_INV_SQRT" } ,
      { FP_INV_CVI,    "FP_INV_CVI" } ,
      { FP_INV_VXSOFT, "FP_INV_VXSOFT" }
  };

/*
 * the TEST_IT macro is used in main() to raise
 * an exception.
 */

#define TEST_IT(WHAT, RAISE_ARG)        \
  {                                     \
  puts(strcat("testing: ", WHAT));      \
  fp_clr_flag(FP_ALL_XCP);              \
  fp_raise_xcp(RAISE_ARG);              \
  }

/*
 * NAME: my_div
 *
 * FUNCTION:  Perform floating-point division.
 *
 */

double
my_div(double x, double y)
  {
  return x / y;
  }

/*
 * NAME: sigfpe_handler
 *
 * FUNCTION: A trap handler that is entered when
 *           a floating-point exception occurs. The
 *           function determines what exceptions caused
 *           the trap, prints this to stdout, and returns
 *           to the process which caused the trap.
 *
 * NOTES:    This trap handler can return either via the
 *           default return mechanism or via longjmp().
 *           The global variable fpsigexit determines which.
 *
 *           When entered, all floating-point traps are
 *           disabled.
 *
 *           This sample uses printf(). This should be used
 *           with caution since printf() of a floating-
 *           point number can cause a trap, and then
 *           another printf() of a floating-point number
 *           in the signal handler will corrupt the static
 *           buffer used for the conversion.
 *
 * OUTPUTS:  The type of exception that caused
 *           the trap.
 */

static void
sigfpe_handler(int sig,
               int code,
               struct sigcontext *SCP)
  {
  struct mstsave * state = &SCP->sc_jmpbuf.jmp_context;
  fp_sh_info_t flt_context;     /* structure for fp_sh_info()
                                /* call */
  int i;                        /* loop counter */
  extern int fpsigexit;         /* global handshaking variable */
  extern jmp_buf jump_buffer    /*  */

  /*
   * Determine which floating-point exceptions caused
   * the trap. fp_sh_info() is used to build the floating signal
   * handler info  structure, then the member
   * flt_context.trap can be examined. First the trap type is
   * compared for the IEEE required traps, and if the trap type
   * is an invalid operation, the detail bits are examined.
   */

  fp_sh_info(SCP, &flt_context, FP_SH_INFO_SIZE);

static void
sigfpe_handler(int sig,
               int code,
               struct sigcontext *SCP)
  {
  struct mstsave * state = &SCP->sc_jmpbuf.jmp_context;
  fp_sh_info_t flt_context;     /* structure for fp_sh_info()
                                /* call */
  int i;                        /* loop counter */
  extern int fpsigexit;         /* global handshaking variable */
  extern jmp_buf jump_buffer;   /*  */

  /*
   * Determine which floating-point exceptions caused
   * the trap. fp_sh_info() is used to build the floating signal
   * handler info  structure, then the member
   * flt_context.trap can be examined. First the trap type is
   * compared for the IEEE required traps, and if the trap type
   * is an invalid operation, the detail bits are examined.
   */

  fp_sh_info(SCP, &flt_context, FP_SH_INFO_SIZE);

  for (i = 0; i < (sizeof(trap_list) / sizeof(fp_list_t)); i++)
      {
      if (flt_context.trap & trap_list[i].mask)
        (void) printf("Trap caused by %s error\n", trap_list[i].text);
      }

  if (flt_context.trap & FP_INVALID)
      {
      for (i = 0; i < (sizeof(detail_list) / sizeof(fp_list_t)); i++)
          {
          if (flt_context.trap & detail_list[i].mask)
            (void) printf("Type of invalid op is %s\n", detail_list[i].text);
          }
      }

  /* report which trap mode was in effect */

  switch (flt_context.trap_mode)
      {
    case FP_TRAP_OFF:
      puts("Trapping Mode is OFF");
      break;

    case FP_TRAP_SYNC:
      puts("Trapping Mode is SYNC");
      break;

    case FP_TRAP_IMP:
      puts("Trapping Mode is IMP");
      break;

    case FP_TRAP_IMP_REC:
      puts("Trapping Mode is IMP_REC");
      break;

    default:
      puts("ERROR:  Invalid trap mode");
      }

  if (fpsigexit == LONGJUMP_EXIT)
      {
      /*
       * Return via longjmp. In this instance there is no need to
       * clear any exceptions or disable traps to prevent
       * recurrence of the exception, because on return the
       * process will have the signal handler's floating-point
       * state.
       */
      longjmp(jump_buffer, JMP_FPE);
      }
  else
      {
      /*
       * Return via default signal handler return mechanism.
       * In this case you must take some action to prevent
       * recurrence of the trap, either by clearing the
       * exception bit in the fpscr or by disabling the trap.
       * In this case, clear the exception bit.
       * The fp_sh_set_stat routine is used to clear
       * the exception bit.
       */

      fp_sh_set_stat(SCP, (flt_context.fpscr & ((fpstat_t) ~flt_context.trap)));
      

      /*
       * Increment the iar of the process that caused the trap,
       * to prevent re-execution of the instruction.
       * The FP_IAR_STAT bit in flt_context.flags indicates if
       * state->iar points to an instruction that has logically
       * started. If this bit is true, state->iar points to
       * an operation that has started and will cause another
       * exception if it runs again. In this case you want to
       * continue execution and ignore the results of that
       * operation, so the iar is advanced to point to the
       * next instruction. If the bit is false, the iar already
       * points to the next instruction that must run.
       */

      if ( flt_context.flags & FP_IAR_STAT )
          {
          puts("Increment IAR");
          state->iar += 4;
          }
      }
  return;
  }

/*
 * NAME: main
 *
 * FUNCTION: Demonstrate the sigfpe_handler trap handler.
 *
 */

int
main(void)
  {
  struct sigaction response;
  struct sigaction old_response;
  extern int fpsigexit;
  extern jmp_buf jump_buffer;
  int jump_rc;
  int trap_mode;
  double arg1, arg2, r;

  /*
   * Set up for floating-point trapping. Do the following:
   *  1.  Clear any existing floating-point exception flags.
   *  2.  Set up a SIGFPE signal handler.
   *  3.  Place the process in synchronous execution mode.
   *  4.  Enable all floating-point traps.
   */

  fp_clr_flag(FP_ALL_XCP);
  (void) sigaction(SIGFPE, NULL, &old_response);
  (void) sigemptyset(&response.sa_mask);
  response.sa_flags = FALSE;
  response.sa_handler = (void (*)(int)) sigfpe_handler;
  (void) sigaction(SIGFPE, &response, NULL);
  fp_enable_all();

  /*
   * Demonstate trap handler return via default signal handler
   * return. The TEST_IT macro will raise the floating-point
   * exception type given in its second argument. Testing
   * is done in this case with precise trapping, because
   * it is supported on all platforms to date.
   */

  trap_mode = fp_trap(FP_TRAP_SYNC);
  if ((trap_mode == FP_TRAP_ERROR) ||
      (trap_mode == FP_TRAP_UNIMPL))
      {
      printf("ERROR: rc from fp_trap is %d\n",
             trap_mode);
      exit(-1);
      }

  (void) printf("Default signal handler return: \n");
  fpsigexit = SIGRETURN_EXIT;

  TEST_IT("div by zero", FP_DIV_BY_ZERO);
  TEST_IT("overflow",    FP_OVERFLOW);
  TEST_IT("underflow",   FP_UNDERFLOW);
  TEST_IT("inexact",     FP_INEXACT);

  TEST_IT("signaling nan",      FP_INV_SNAN);
  TEST_IT("INF - INF",          FP_INV_ISI);
  TEST_IT("INF / INF",          FP_INV_IDI);
  TEST_IT("ZERO / ZERO",        FP_INV_ZDZ);
  TEST_IT("INF * ZERO",         FP_INV_IMZ);
  TEST_IT("invalid compare",    FP_INV_CMP);
  TEST_IT("invalid sqrt",       FP_INV_SQRT);
  TEST_IT("invalid coversion",  FP_INV_CVI);
  TEST_IT("software request",   FP_INV_VXSOFT);

  /*
   * Next, use fp_trap() to determine what the
   * the fastest trapmode available is on
   * this platform.
   */

  trap_mode = fp_trap(FP_TRAP_FASTMODE);
  switch (trap_mode)
      {
    case FP_TRAP_SYNC:
      puts("Fast mode for this platform is PRECISE");
      break;

    case FP_TRAP_OFF:
      puts("This platform dosn't support trapping");
      break;

    case FP_TRAP_IMP:
      puts("Fast mode for this platform is IMPRECISE");
      break;

    case FP_TRAP_IMP_REC:
      puts("Fast mode for this platform is IMPRECISE RECOVERABLE");
      break;

    default:
      printf("Unexpected return code from fp_trap(FP_TRAP_FASTMODE): %d\n",
             trap_mode);
      exit(-2);
      }

  /*
   * if this platform supports imprecise trapping, demonstate this.
   */

  trap_mode = fp_trap(FP_TRAP_IMP);
  if (trap_mode != FP_TRAP_UNIMPL)
      {
      puts("Demonsrate imprecise FP execeptions");
      arg1 = 1.2;
      arg2 = 0.0;
      r = my_div(arg1, arg2);
      fp_flush_imprecise();
      }

  /* demonstate trap handler return via longjmp().
   */

  (void) printf("longjmp return: \n");
  fpsigexit = LONGJUMP_EXIT;
  jump_rc = setjmp(jump_buffer);

  switch (jump_rc)
      {
    case JMP_DEFINED:
      (void) printf("setjmp has been set up; testing ...\n");
      TEST_IT("div by zero", FP_DIV_BY_ZERO);
      break;

    case JMP_FPE:
      (void) printf("back from signal handler\n");
      /*
       * Note that at this point the process has the floating-
       * point status inherited from the trap handler. If the
       * trap hander did not enable trapping (as the example
       * did not) then this process at this point has no traps
       * enabled.  We create a floating-point exception to
       * demonstrate that a trap does not occur, then re-enable
       * traps.
       */

      (void) printf("Creating overflow; should not trap\n");
      TEST_IT("Overflow", FP_OVERFLOW);
      fp_enable_all();
      break;

    default:
      (void) printf("unexpected rc from setjmp: %d\n", jump_rc);
      exit(EXIT_BAD);
      }
  exit(EXIT_GOOD);
  }