*DECK XERMSG
      SUBROUTINE XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
C***BEGIN PROLOGUE  XERMSG
C***PURPOSE  Process error messages for SLATEC and other libraries.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERMSG-A)
C***KEYWORDS  ERROR MESSAGE, XERROR
C***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
C***DESCRIPTION
C
C   XERMSG processes a diagnostic message in a manner determined by the
C   value of LEVEL and the current value of the library error control
C   flag, KONTRL.  See subroutine XSETF for details.
C
C    LIBRAR   A character constant (or character variable) with the name
C             of the library.  This will be 'SLATEC' for the SLATEC
C             Common Math Library.  The error handling package is
C             general enough to be used by many libraries
C             simultaneously, so it is desirable for the routine that
C             detects and reports an error to identify the library name
C             as well as the routine name.
C
C    SUBROU   A character constant (or character variable) with the name
C             of the routine that detected the error.  Usually it is the
C             name of the routine that is calling XERMSG.  There are
C             some instances where a user callable library routine calls
C             lower level subsidiary routines where the error is
C             detected.  In such cases it may be more informative to
C             supply the name of the routine the user called rather than
C             the name of the subsidiary routine that detected the
C             error.
C
C    MESSG    A character constant (or character variable) with the text
C             of the error or warning message.  In the example below,
C             the message is a character constant that contains a
C             generic message.
C
C                   CALL XERMSG ('SLATEC', 'MMPY',
C                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
C                  *3, 1)
C
C             It is possible (and is sometimes desirable) to generate a
C             specific message--e.g., one that contains actual numeric
C             values.  Specific numeric values can be converted into
C             character strings using formatted WRITE statements into
C             character variables.  This is called standard Fortran
C             internal file I/O and is exemplified in the first three
C             lines of the following example.  You can also catenate
C             substrings of characters to construct the error message.
C             Here is an example showing the use of both writing to
C             an internal file and catenating character strings.
C
C                   CHARACTER*5 CHARN, CHARL
C                   WRITE (CHARN,10) N
C                   WRITE (CHARL,10) LDA
C                10 FORMAT(I5)
C                   CALL XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
C                  *   ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
C                  *   CHARL, 3, 1)
C
C             There are two subtleties worth mentioning.  One is that
C             the // for character catenation is used to construct the
C             error message so that no single character constant is
C             continued to the next line.  This avoids confusion as to
C             whether there are trailing blanks at the end of the line.
C             The second is that by catenating the parts of the message
C             as an actual argument rather than encoding the entire
C             message into one large character variable, we avoid
C             having to know how long the message will be in order to
C             declare an adequate length for that large character
C             variable.  XERMSG calls XERPRN to print the message using
C             multiple lines if necessary.  If the message is very long,
C             XERPRN will break it into pieces of 72 characters (as
C             requested by XERMSG) for printing on multiple lines.
C             Also, XERMSG asks XERPRN to prefix each line with ' *  '
C             so that the total line length could be 76 characters.
C             Note also that XERPRN scans the error message backwards
C             to ignore trailing blanks.  Another feature is that
C             the substring '$$' is treated as a new line sentinel
C             by XERPRN.  If you want to construct a multiline
C             message without having to count out multiples of 72
C             characters, just use '$$' as a separator.  '$$'
C             obviously must occur within 72 characters of the
C             start of each line to have its intended effect since
C             XERPRN is asked to wrap around at 72 characters in
C             addition to looking for '$$'.
C
C    NERR     An integer value that is chosen by the library routine's
C             author.  It must be in the range -99 to 999 (three
C             printable digits).  Each distinct error should have its
C             own error number.  These error numbers should be described
C             in the machine readable documentation for the routine.
C             The error numbers need be unique only within each routine,
C             so it is reasonable for each routine to start enumerating
C             errors from 1 and proceeding to the next integer.
C
C    LEVEL    An integer value in the range 0 to 2 that indicates the
C             level (severity) of the error.  Their meanings are
C
C            -1  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.  An attempt is made to only print this
C                message once.
C
C             0  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.
C
C             1  A recoverable error.  This is used even if the error is
C                so serious that the routine cannot return any useful
C                answer.  If the user has told the error package to
C                return after recoverable errors, then XERMSG will
C                return to the Library routine which can then return to
C                the user's routine.  The user may also permit the error
C                package to terminate the program upon encountering a
C                recoverable error.
C
C             2  A fatal error.  XERMSG will not return to its caller
C                after it receives a fatal error.  This level should
C                hardly ever be used; it is much better to allow the
C                user a chance to recover.  An example of one of the few
C                cases in which it is permissible to declare a level 2
C                error is a reverse communication Library routine that
C                is likely to be called repeatedly until it integrates
C                across some interval.  If there is a serious error in
C                the input such that another step cannot be taken and
C                the Library routine is called again without the input
C                error having been corrected by the caller, the Library
C                routine will probably be called forever with improper
C                input.  In this case, it is reasonable to declare the
C                error to be fatal.
C
C    Each of the arguments to XERMSG is input; none will be modified by
C    XERMSG.  A routine may make multiple calls to XERMSG with warning
C    level messages; however, after a call to XERMSG with a recoverable
C    error, the routine should return to the user.  Do not try to call
C    XERMSG with a second recoverable error after the first recoverable
C    error because the error package saves the error number.  The user
C    can retrieve this error number by calling another entry point in
C    the error handling package and then clear the error number when
C    recovering from the error.  Calling XERMSG in succession causes the
C    old error number to be overwritten by the latest error number.
C    This is considered harmless for error numbers associated with
C    warning messages but must not be done for error numbers of serious
C    errors.  After a call to XERMSG with a recoverable error, the user
C    must be given a chance to call NUMXER or XERCLR to retrieve or
C    clear the error number.
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
C***REVISION HISTORY  (YYMMDD)
C   880101  DATE WRITTEN
C   880621  REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
C           THERE ARE TWO BASIC CHANGES.
C           1.  A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
C               PRINT MESSAGES.  THIS ROUTINE WILL BREAK LONG MESSAGES
C               INTO PIECES FOR PRINTING ON MULTIPLE LINES.  '$$' IS
C               ACCEPTED AS A NEW LINE SENTINEL.  A PREFIX CAN BE
C               ADDED TO EACH LINE TO BE PRINTED.  XERMSG USES EITHER
C               ' ***' OR ' *  ' AND LONG MESSAGES ARE BROKEN EVERY
C               72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
C               LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
C           2.  THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
C               FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
C               OF LOWER CASE.
C   880708  REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
C           THE PRINCIPAL CHANGES ARE
C           1.  CLARIFY COMMENTS IN THE PROLOGUES
C           2.  RENAME XRPRNT TO XERPRN
C           3.  REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
C               SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
C               CHARACTER FOR NEW RECORDS.
C   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
C           CLEAN UP THE CODING.
C   890721  REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
C           PREFIX.
C   891013  REVISED TO CORRECT COMMENTS.
C   891214  Prologue converted to Version 4.0 format.  (WRB)
C   900510  Changed test on NERR to be -9999999 < NERR < 99999999, but
C           NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
C           LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
C           XERCTL to XERCNT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERMSG
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      CHARACTER*8 XLIBR, XSUBR
      CHARACTER*72  TEMP
      CHARACTER*20  LFIRST
C***FIRST EXECUTABLE STATEMENT  XERMSG
      LKNTRL = J4SAVE (2, 0, .FALSE.)
      MAXMES = J4SAVE (4, 0, .FALSE.)
C
C       LKNTRL IS A LOCAL COPY OF THE CONTROL FLAG KONTRL.
C       MAXMES IS THE MAXIMUM NUMBER OF TIMES ANY PARTICULAR MESSAGE
C          SHOULD BE PRINTED.
C
C       WE PRINT A FATAL ERROR MESSAGE AND TERMINATE FOR AN ERROR IN
C          CALLING XERMSG.  THE ERROR NUMBER SHOULD BE POSITIVE,
C          AND THE LEVEL SHOULD BE BETWEEN 0 AND 2.
C
      IF (NERR.LT.-9999999 .OR. NERR.GT.99999999 .OR. NERR.EQ.0 .OR.
     *   LEVEL.LT.-1 .OR. LEVEL.GT.2) THEN
         CALL XERPRN (' ***', -1, 'FATAL ERROR IN...$$ ' //
     *      'XERMSG -- INVALID ERROR NUMBER OR LEVEL$$ '//
     *      'JOB ABORT DUE TO FATAL ERROR.', 72)
         CALL XERSVE (' ', ' ', ' ', 0, 0, 0, KDUMMY)
         CALL XERHLT (' ***XERMSG -- INVALID INPUT')
         RETURN
      ENDIF
C
C       RECORD THE MESSAGE.
C
      I = J4SAVE (1, NERR, .TRUE.)
      CALL XERSVE (LIBRAR, SUBROU, MESSG, 1, NERR, LEVEL, KOUNT)
C
C       HANDLE PRINT-ONCE WARNING MESSAGES.
C
      IF (LEVEL.EQ.-1 .AND. KOUNT.GT.1) RETURN
C
C       ALLOW TEMPORARY USER OVERRIDE OF THE CONTROL FLAG.
C
      XLIBR  = LIBRAR
      XSUBR  = SUBROU
      LFIRST = MESSG
      LERR   = NERR
      LLEVEL = LEVEL
      CALL XERCNT (XLIBR, XSUBR, LFIRST, LERR, LLEVEL, LKNTRL)
C
      LKNTRL = MAX(-2, MIN(2,LKNTRL))
      MKNTRL = ABS(LKNTRL)
C
C       SKIP PRINTING IF THE CONTROL FLAG VALUE AS RESET IN XERCNT IS
C       ZERO AND THE ERROR IS NOT FATAL.
C
      IF (LEVEL.LT.2 .AND. LKNTRL.EQ.0) GO TO 30
      IF (LEVEL.EQ.0 .AND. KOUNT.GT.MAXMES) GO TO 30
      IF (LEVEL.EQ.1 .AND. KOUNT.GT.MAXMES .AND. MKNTRL.EQ.1) GO TO 30
      IF (LEVEL.EQ.2 .AND. KOUNT.GT.MAX(1,MAXMES)) GO TO 30
C
C       ANNOUNCE THE NAMES OF THE LIBRARY AND SUBROUTINE BY BUILDING A
C       MESSAGE IN CHARACTER VARIABLE TEMP (NOT EXCEEDING 66 CHARACTERS)
C       AND SENDING IT OUT VIA XERPRN.  PRINT ONLY IF CONTROL FLAG
C       IS NOT ZERO.
C
      IF (LKNTRL .NE. 0) THEN
         TEMP(1:21) = 'MESSAGE FROM ROUTINE '
         I = MIN(LEN(SUBROU), 16)
         TEMP(22:21+I) = SUBROU(1:I)
         TEMP(22+I:33+I) = ' IN LIBRARY '
         LTEMP = 33 + I
         I = MIN(LEN(LIBRAR), 16)
         TEMP(LTEMP+1:LTEMP+I) = LIBRAR (1:I)
         TEMP(LTEMP+I+1:LTEMP+I+1) = '.'
         LTEMP = LTEMP + I + 1
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       IF LKNTRL IS POSITIVE, PRINT AN INTRODUCTORY LINE BEFORE
C       PRINTING THE MESSAGE.  THE INTRODUCTORY LINE TELLS THE CHOICE
C       FROM EACH OF THE FOLLOWING THREE OPTIONS.
C       1.  LEVEL OF THE MESSAGE
C              'INFORMATIVE MESSAGE'
C              'POTENTIALLY RECOVERABLE ERROR'
C              'FATAL ERROR'
C       2.  WHETHER CONTROL FLAG WILL ALLOW PROGRAM TO CONTINUE
C              'PROG CONTINUES'
C              'PROG ABORTED'
C       3.  WHETHER OR NOT A TRACEBACK WAS REQUESTED.  (THE TRACEBACK
C           MAY NOT BE IMPLEMENTED AT SOME SITES, SO THIS ONLY TELLS
C           WHAT WAS REQUESTED, NOT WHAT WAS DELIVERED.)
C              'TRACEBACK REQUESTED'
C              'TRACEBACK NOT REQUESTED'
C       NOTICE THAT THE LINE INCLUDING FOUR PREFIX CHARACTERS WILL NOT
C       EXCEED 74 CHARACTERS.
C       WE SKIP THE NEXT BLOCK IF THE INTRODUCTORY LINE IS NOT NEEDED.
C
      IF (LKNTRL .GT. 0) THEN
C
C       THE FIRST PART OF THE MESSAGE TELLS ABOUT THE LEVEL.
C
         IF (LEVEL .LE. 0) THEN
            TEMP(1:20) = 'INFORMATIVE MESSAGE,'
            LTEMP = 20
         ELSEIF (LEVEL .EQ. 1) THEN
            TEMP(1:30) = 'POTENTIALLY RECOVERABLE ERROR,'
            LTEMP = 30
         ELSE
            TEMP(1:12) = 'FATAL ERROR,'
            LTEMP = 12
         ENDIF
C
C       THEN WHETHER THE PROGRAM WILL CONTINUE.
C
         IF ((MKNTRL.EQ.2 .AND. LEVEL.GE.1) .OR.
     *       (MKNTRL.EQ.1 .AND. LEVEL.EQ.2)) THEN
            TEMP(LTEMP+1:LTEMP+14) = ' PROG ABORTED,'
            LTEMP = LTEMP + 14
         ELSE
            TEMP(LTEMP+1:LTEMP+16) = ' PROG CONTINUES,'
            LTEMP = LTEMP + 16
         ENDIF
C
C       FINALLY TELL WHETHER THERE SHOULD BE A TRACEBACK.
C
         IF (LKNTRL .GT. 0) THEN
            TEMP(LTEMP+1:LTEMP+20) = ' TRACEBACK REQUESTED'
            LTEMP = LTEMP + 20
         ELSE
            TEMP(LTEMP+1:LTEMP+24) = ' TRACEBACK NOT REQUESTED'
            LTEMP = LTEMP + 24
         ENDIF
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       NOW SEND OUT THE MESSAGE.
C
      CALL XERPRN (' *  ', -1, MESSG, 72)
C
C       IF LKNTRL IS POSITIVE, WRITE THE ERROR NUMBER AND REQUEST A
C          TRACEBACK.
C
      IF (LKNTRL .GT. 0) THEN
         WRITE (TEMP, '(''ERROR NUMBER = '', I8)') NERR
         DO 10 I=16,22
            IF (TEMP(I:I) .NE. ' ') GO TO 20
   10    CONTINUE
C
   20    CALL XERPRN (' *  ', -1, TEMP(1:15) // TEMP(I:23), 72)
         CALL FDUMP
      ENDIF
C
C       IF LKNTRL IS NOT ZERO, PRINT A BLANK LINE AND AN END OF MESSAGE.
C
      IF (LKNTRL .NE. 0) THEN
         CALL XERPRN (' *  ', -1, ' ', 72)
         CALL XERPRN (' ***', -1, 'END OF MESSAGE', 72)
         CALL XERPRN ('    ',  0, ' ', 72)
      ENDIF
C
C       IF THE ERROR IS NOT FATAL OR THE ERROR IS RECOVERABLE AND THE
C       CONTROL FLAG IS SET FOR RECOVERY, THEN RETURN.
C
   30 IF (LEVEL.LE.0 .OR. (LEVEL.EQ.1 .AND. MKNTRL.LE.1)) RETURN
C
C       THE PROGRAM WILL BE STOPPED DUE TO AN UNRECOVERED ERROR OR A
C       FATAL ERROR.  PRINT THE REASON FOR THE ABORT AND THE ERROR
C       SUMMARY IF THE CONTROL FLAG AND THE MAXIMUM ERROR COUNT PERMIT.
C
      IF (LKNTRL.GT.0 .AND. KOUNT.LT.MAX(1,MAXMES)) THEN
         IF (LEVEL .EQ. 1) THEN
            CALL XERPRN
     *         (' ***', -1, 'JOB ABORT DUE TO UNRECOVERED ERROR.', 72)
         ELSE
            CALL XERPRN(' ***', -1, 'JOB ABORT DUE TO FATAL ERROR.', 72)
         ENDIF
         CALL XERSVE (' ', ' ', ' ', -1, 0, 0, KDUMMY)
         CALL XERHLT (' ')
      ELSE
         CALL XERHLT (MESSG)
      ENDIF
      RETURN
      END

*DECK D9UPAK
      SUBROUTINE D9UPAK (X, Y, N)
C***BEGIN PROLOGUE  D9UPAK
C***PURPOSE  Unpack a floating point number X so that X = Y*2**N.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  A6B
C***TYPE      DOUBLE PRECISION (R9UPAK-S, D9UPAK-D)
C***KEYWORDS  FNLIB, UNPACK
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   Unpack a floating point number X so that X = Y*2.0**N, where
C   0.5 .LE. ABS(Y) .LT. 1.0.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   780701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900820  Corrected code to find Y between 0.5 and 1.0 rather than
C           between 0.05 and 1.0.  (WRB)
C***END PROLOGUE  D9UPAK
      DOUBLE PRECISION X,Y,ABSX
C***FIRST EXECUTABLE STATEMENT  D9UPAK
      ABSX = ABS(X)
      N = 0
      IF (X.EQ.0.0D0) GO TO 30
C
   10 IF (ABSX.GE.0.5D0) GO TO 20
      N = N-1
      ABSX = ABSX*2.0D0
      GO TO 10
C
   20 IF (ABSX.LT.1.0D0) GO TO 30
      N = N+1
      ABSX = ABSX*0.5D0
      GO TO 20
C
   30 Y = SIGN(ABSX,X)
      RETURN
C
      END

*DECK J4SAVE
      FUNCTION J4SAVE (IWHICH, IVALUE, ISET)
C***BEGIN PROLOGUE  J4SAVE
C***SUBSIDIARY
C***PURPOSE  Save or recall global variables needed by error
C            handling routines.
C***LIBRARY   SLATEC (XERROR)
C***TYPE      INTEGER (J4SAVE-I)
C***KEYWORDS  ERROR MESSAGES, ERROR NUMBER, RECALL, SAVE, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        J4SAVE saves and recalls several global variables needed
C        by the library error handling routines.
C
C     Description of Parameters
C      --Input--
C        IWHICH - Index of item desired.
C                = 1 Refers to current error number.
C                = 2 Refers to current error control flag.
C                = 3 Refers to current unit number to which error
C                    messages are to be sent.  (0 means use standard.)
C                = 4 Refers to the maximum number of times any
C                     message is to be printed (as set by XERMAX).
C                = 5 Refers to the total number of units to which
C                     each error message is to be written.
C                = 6 Refers to the 2nd unit for error messages
C                = 7 Refers to the 3rd unit for error messages
C                = 8 Refers to the 4th unit for error messages
C                = 9 Refers to the 5th unit for error messages
C        IVALUE - The value to be set for the IWHICH-th parameter,
C                 if ISET is .TRUE. .
C        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
C                 given the value, IVALUE.  If ISET=.FALSE., the
C                 IWHICH-th parameter will be unchanged, and IVALUE
C                 is a dummy parameter.
C      --Output--
C        The (old) value of the IWHICH-th parameter will be returned
C        in the function value, J4SAVE.
C
C***SEE ALSO  XERMSG
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900205  Minor modifications to prologue.  (WRB)
C   900402  Added TYPE section.  (WRB)
C   910411  Added KEYWORDS section.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  J4SAVE
      LOGICAL ISET
      INTEGER IPARAM(9)
      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
C***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) IPARAM(IWHICH) = IVALUE
      RETURN
      END

*DECK R1MACH
      REAL FUNCTION R1MACH (I)
C***BEGIN PROLOGUE  R1MACH
C***PURPOSE  Return floating point machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      SINGLE PRECISION (R1MACH-S, D1MACH-D)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   R1MACH can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument, and can be referenced as follows:
C
C        A = R1MACH(I)
C
C   where I=1,...,5.  The (output) value of A above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
C   R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   R1MACH(3) = B**(-T), the smallest relative spacing.
C   R1MACH(4) = B**(1-T), the largest relative spacing.
C   R1MACH(5) = LOG10(B)
C
C   Assume single precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in I1MACH as
C   follows:
C   I1MACH(10) = B, the base.
C   I1MACH(11) = T, the number of base-B digits.
C   I1MACH(12) = EMIN, the smallest exponent E.
C   I1MACH(13) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment, the desired
C   set of DATA statements should be activated by removing the C from
C   column 1.  Also, the values of R1MACH(1) - R1MACH(4) should be
C   checked for consistency with the local operating system.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790101  DATE WRITTEN
C   890213  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900618  Added DEC RISC constants.  (WRB)
C   900723  Added IBM RS 6000 constants.  (WRB)
C   910710  Added HP 730 constants.  (SMR)
C   911114  Added Convex IEEE constants.  (WRB)
C   920121  Added SUN -r8 compiler option constants.  (WRB)
C   920229  Added Touchstone Delta i860 constants.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920625  Added CONVEX -p8 and -pd8 compiler option constants.
C           (BKS, WRB)
C   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
C***END PROLOGUE  R1MACH
C
      INTEGER SMALL(2)
      INTEGER LARGE(2)
      INTEGER RIGHT(2)
      INTEGER DIVER(2)
      INTEGER LOG10(2)
C
      REAL RMACH(5)
      SAVE RMACH
C
      EQUIVALENCE (RMACH(1),SMALL(1))
      EQUIVALENCE (RMACH(2),LARGE(1))
      EQUIVALENCE (RMACH(3),RIGHT(1))
      EQUIVALENCE (RMACH(4),DIVER(1))
      EQUIVALENCE (RMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING THE 68020/68881 COMPILER OPTION
C
C     DATA SMALL(1) / Z'00800000' /
C     DATA LARGE(1) / Z'7F7FFFFF' /
C     DATA RIGHT(1) / Z'33800000' /
C     DATA DIVER(1) / Z'34000000' /
C     DATA LOG10(1) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING SOFTWARE FLOATING POINT
C
C     DATA SMALL(1) / Z'00800000' /
C     DATA LARGE(1) / Z'7EFFFFFF' /
C     DATA RIGHT(1) / Z'33800000' /
C     DATA DIVER(1) / Z'34000000' /
C     DATA LOG10(1) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA SMALL(1) / 16#00800000 /
C     DATA LARGE(1) / 16#7FFFFFFF /
C     DATA RIGHT(1) / 16#33800000 /
C     DATA DIVER(1) / 16#34000000 /
C     DATA LOG10(1) / 16#3E9A209B /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA RMACH(1) / Z400800000 /
C     DATA RMACH(2) / Z5FFFFFFFF /
C     DATA RMACH(3) / Z4E9800000 /
C     DATA RMACH(4) / Z4EA800000 /
C     DATA RMACH(5) / Z500E730E8 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS
C
C     DATA RMACH(1) / O1771000000000000 /
C     DATA RMACH(2) / O0777777777777777 /
C     DATA RMACH(3) / O1311000000000000 /
C     DATA RMACH(4) / O1301000000000000 /
C     DATA RMACH(5) / O1157163034761675 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA RMACH(1) / Z"3001800000000000" /
C     DATA RMACH(2) / Z"4FFEFFFFFFFFFFFE" /
C     DATA RMACH(3) / Z"3FD2800000000000" /
C     DATA RMACH(4) / Z"3FD3800000000000" /
C     DATA RMACH(5) / Z"3FFF9A209A84FBCF" /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA RMACH(1) / 00564000000000000000B /
C     DATA RMACH(2) / 37767777777777777776B /
C     DATA RMACH(3) / 16414000000000000000B /
C     DATA RMACH(4) / 16424000000000000000B /
C     DATA RMACH(5) / 17164642023241175720B /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA SMALL(1) / Z'00800000' /
C     DATA LARGE(1) / Z'7F7FFFFF' /
C     DATA RIGHT(1) / Z'33800000' /
C     DATA DIVER(1) / Z'34000000' /
C     DATA LOG10(1) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn COMPILER OPTION
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7FFFFFFF' /
C     DATA RMACH(3) / Z'34800000' /
C     DATA RMACH(4) / Z'35000000' /
C     DATA RMACH(5) / Z'3F9A209B' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 OR -pd8 COMPILER OPTION
C
C     DATA RMACH(1) / Z'0010000000000000' /
C     DATA RMACH(2) / Z'7FFFFFFFFFFFFFFF' /
C     DATA RMACH(3) / Z'3CC0000000000000' /
C     DATA RMACH(4) / Z'3CD0000000000000' /
C     DATA RMACH(5) / Z'3FF34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CRAY
C
C     DATA RMACH(1) / 200034000000000000000B /
C     DATA RMACH(2) / 577767777777777777776B /
C     DATA RMACH(3) / 377224000000000000000B /
C     DATA RMACH(4) / 377234000000000000000B /
C     DATA RMACH(5) / 377774642023241175720B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC RMACH(5)
C
C     DATA SMALL /    20K,       0 /
C     DATA LARGE / 77777K, 177777K /
C     DATA RIGHT / 35420K,       0 /
C     DATA DIVER / 36020K,       0 /
C     DATA LOG10 / 40423K,  42023K /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA RMACH(1) / '00000080'X /
C     DATA RMACH(2) / 'FFFF7FFF'X /
C     DATA RMACH(3) / '00003480'X /
C     DATA RMACH(4) / '00003500'X /
C     DATA RMACH(5) / '209B3F9A'X /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FLOAT
C
C     DATA RMACH(1) / '00800000'X /
C     DATA RMACH(2) / '7F7FFFFF'X /
C     DATA RMACH(3) / '33800000'X /
C     DATA RMACH(4) / '34000000'X /
C     DATA RMACH(5) / '3E9A209B'X /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
C
C     DATA SMALL(1) /       128 /
C     DATA LARGE(1) /    -32769 /
C     DATA RIGHT(1) /     13440 /
C     DATA DIVER(1) /     13568 /
C     DATA LOG10(1) / 547045274 /
C
C     DATA SMALL(1) / Z00000080 /
C     DATA LARGE(1) / ZFFFF7FFF /
C     DATA RIGHT(1) / Z00003480 /
C     DATA DIVER(1) / Z00003500 /
C     DATA LOG10(1) / Z209B3F9A /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C     (ASSUMING REAL*4 IS THE DEFAULT REAL)
C
C     DATA SMALL(1) / '00800000'X /
C     DATA LARGE(1) / '7F7FFFFF'X /
C     DATA RIGHT(1) / '33800000'X /
C     DATA DIVER(1) / '34000000'X /
C     DATA LOG10(1) / '3E9A209B'X /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1), LARGE(2) / '37777777, '00000177 /
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000352 /
C     DATA DIVER(1), DIVER(2) / '20000000, '00000353 /
C     DATA LOG10(1), LOG10(2) / '23210115, '00000377 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA RMACH(1) / O402400000000 /
C     DATA RMACH(2) / O376777777777 /
C     DATA RMACH(3) / O714400000000 /
C     DATA RMACH(4) / O716400000000 /
C     DATA RMACH(5) / O776464202324 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) / 40000B,       1 /
C     DATA LARGE(1), LARGE(2) / 77777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) / 40000B,    325B /
C     DATA DIVER(1), DIVER(2) / 40000B,    327B /
C     DATA LOG10(1), LOG10(2) / 46420B,  46777B /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA SMALL(1) / 00004000000B /
C     DATA LARGE(1) / 17677777777B /
C     DATA RIGHT(1) / 06340000000B /
C     DATA DIVER(1) / 06400000000B /
C     DATA LOG10(1) / 07646420233B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86  AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA RMACH(1) / Z00100000 /
C     DATA RMACH(2) / Z7FFFFFFF /
C     DATA RMACH(3) / Z3B100000 /
C     DATA RMACH(4) / Z3C100000 /
C     DATA RMACH(5) / Z41134413 /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C
C     DATA SMALL(1) / 1.18E-38      /
C     DATA LARGE(1) / 3.40E+38      /
C     DATA RIGHT(1) / 0.595E-07     /
C     DATA DIVER(1) / 1.19E-07      /
C     DATA LOG10(1) / 0.30102999566 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR)
C
C     DATA RMACH(1) / "000400000000 /
C     DATA RMACH(2) / "377777777777 /
C     DATA RMACH(3) / "146400000000 /
C     DATA RMACH(4) / "147400000000 /
C     DATA RMACH(5) / "177464202324 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1) /    8388608 /
C     DATA LARGE(1) / 2147483647 /
C     DATA RIGHT(1) /  880803840 /
C     DATA DIVER(1) /  889192448 /
C     DATA LOG10(1) / 1067065499 /
C
C     DATA RMACH(1) / O00040000000 /
C     DATA RMACH(2) / O17777777777 /
C     DATA RMACH(3) / O06440000000 /
C     DATA RMACH(4) / O06500000000 /
C     DATA RMACH(5) / O07746420233 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS  (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /   128,     0 /
C     DATA LARGE(1), LARGE(2) / 32767,    -1 /
C     DATA RIGHT(1), RIGHT(2) / 13440,     0 /
C     DATA DIVER(1), DIVER(2) / 13568,     0 /
C     DATA LOG10(1), LOG10(2) / 16282,  8347 /
C
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /
C     DATA RIGHT(1), RIGHT(2) / O032200, O000000 /
C     DATA DIVER(1), DIVER(2) / O032400, O000000 /
C     DATA LOG10(1), LOG10(2) / O037632, O020233 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA RMACH(1) / Z'00800000' /
C     DATA RMACH(2) / Z'7F7FFFFF' /
C     DATA RMACH(3) / Z'33800000' /
C     DATA RMACH(4) / Z'34000000' /
C     DATA RMACH(5) / Z'3E9A209B' /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA RMACH(1) / Z'0010000000000000' /
C     DATA RMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA RMACH(3) / Z'3CA0000000000000' /
C     DATA RMACH(4) / Z'3CB0000000000000' /
C     DATA RMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES
C
C     DATA RMACH(1) / O000400000000 /
C     DATA RMACH(2) / O377777777777 /
C     DATA RMACH(3) / O146400000000 /
C     DATA RMACH(4) / O147400000000 /
C     DATA RMACH(5) / O177464202324 /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA SMALL(1), SMALL(2) /     0,    256/
C     DATA LARGE(1), LARGE(2) /    -1,   -129/
C     DATA RIGHT(1), RIGHT(2) /     0,  26880/
C     DATA DIVER(1), DIVER(2) /     0,  27136/
C     DATA LOG10(1), LOG10(2) /  8347,  32538/
C
C***FIRST EXECUTABLE STATEMENT  R1MACH
      IF (I .LT. 1 .OR. I .GT. 5) CALL XERMSG ('SLATEC', 'R1MACH',
     +   'I OUT OF BOUNDS', 1, 2)
C
      R1MACH = RMACH(I)
      RETURN
C
      END

*DECK INITDS
      FUNCTION INITDS (OS, NOS, ETA)
C***BEGIN PROLOGUE  INITDS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (INITS-S, INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that INITDS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     double precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  INITDS
      DOUBLE PRECISION OS(*)
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS .LT. 1) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITDS = I
C
      RETURN
      END

*DECK I1MACH
      INTEGER FUNCTION I1MACH (I)
C***BEGIN PROLOGUE  I1MACH
C***PURPOSE  Return integer machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      INTEGER (I1MACH-I)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   I1MACH can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument and can be referenced as follows:
C
C        K = I1MACH(I)
C
C   where I=1,...,16.  The (output) value of K above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   I/O unit numbers:
C     I1MACH( 1) = the standard input unit.
C     I1MACH( 2) = the standard output unit.
C     I1MACH( 3) = the standard punch unit.
C     I1MACH( 4) = the standard error message unit.
C
C   Words:
C     I1MACH( 5) = the number of bits per integer storage unit.
C     I1MACH( 6) = the number of characters per integer storage unit.
C
C   Integers:
C     assume integers are represented in the S-digit, base-A form
C
C                sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C                where 0 .LE. X(I) .LT. A for I=0,...,S-1.
C     I1MACH( 7) = A, the base.
C     I1MACH( 8) = S, the number of base-A digits.
C     I1MACH( 9) = A**S - 1, the largest magnitude.
C
C   Floating-Point Numbers:
C     Assume floating-point numbers are represented in the T-digit,
C     base-B form
C                sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C                where 0 .LE. X(I) .LT. B for I=1,...,T,
C                0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
C     I1MACH(10) = B, the base.
C
C   Single-Precision:
C     I1MACH(11) = T, the number of base-B digits.
C     I1MACH(12) = EMIN, the smallest exponent E.
C     I1MACH(13) = EMAX, the largest exponent E.
C
C   Double-Precision:
C     I1MACH(14) = T, the number of base-B digits.
C     I1MACH(15) = EMIN, the smallest exponent E.
C     I1MACH(16) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment, the desired
C   set of DATA statements should be activated by removing the C from
C   column 1.  Also, the values of I1MACH(1) - I1MACH(4) should be
C   checked for consistency with the local operating system.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   891012  Added VAX G-floating constants.  (WRB)
C   891012  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900618  Added DEC RISC constants.  (WRB)
C   900723  Added IBM RS 6000 constants.  (WRB)
C   901009  Correct I1MACH(7) for IBM Mainframes. Should be 2 not 16.
C           (RWC)
C   910710  Added HP 730 constants.  (SMR)
C   911114  Added Convex IEEE constants.  (WRB)
C   920121  Added SUN -r8 compiler option constants.  (WRB)
C   920229  Added Touchstone Delta i860 constants.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920625  Added Convex -p8 and -pd8 compiler option constants.
C           (BKS, WRB)
C   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
C   930618  Corrected I1MACH(5) for Convex -p8 and -pd8 compiler
C           options.  (DWL, RWC and WRB).
C***END PROLOGUE  I1MACH
C
      INTEGER IMACH(16),OUTPUT
      SAVE IMACH
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        129 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1025 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA IMACH( 1) /          7 /
C     DATA IMACH( 2) /          2 /
C     DATA IMACH( 3) /          2 /
C     DATA IMACH( 4) /          2 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         33 /
C     DATA IMACH( 9) / Z1FFFFFFFF /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -256 /
C     DATA IMACH(13) /        255 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /       -256 /
C     DATA IMACH(16) /        255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /        -50 /
C     DATA IMACH(16) /         76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /     -32754 /
C     DATA IMACH(16) /      32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -4095 /
C     DATA IMACH(13) /       4094 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -4095 /
C     DATA IMACH(16) /       4094 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /    6LOUTPUT/
C     DATA IMACH( 5) /         60 /
C     DATA IMACH( 6) /         10 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         48 /
C     DATA IMACH( 9) / 00007777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /       -929 /
C     DATA IMACH(13) /       1070 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /       -929 /
C     DATA IMACH(16) /       1069 /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / Z'7FFFFFFF' /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16383 /
C     DATA IMACH(16) /      16383 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -pd8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 46 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         46 /
C     DATA IMACH( 9) / 1777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 64 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 777777777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     DATA IMACH( 1) /         11 /
C     DATA IMACH( 2) /         12 /
C     DATA IMACH( 3) /          8 /
C     DATA IMACH( 4) /         10 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING D_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING G_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         24 /
C     DATA IMACH( 6) /          3 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         23 /
C     DATA IMACH( 9) /    8388607 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         38 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /         43 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         63 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         39 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         55 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          7 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1015 /
C     DATA IMACH(16) /       1017 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) /  Z7FFFFFFF /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         54 /
C     DATA IMACH(15) /       -101 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         62 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1021 /
C     DATA IMACH(13) /       1024 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16381 /
C     DATA IMACH(16) /      16384 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          1 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /      -1024 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA IMACH( 1) /          1 /
C     DATA IMACH( 2) /          1 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10
C
      I1MACH = IMACH(I)
      RETURN
C
   10 CONTINUE
      WRITE (UNIT = OUTPUT, FMT = 9000)
 9000 FORMAT ('1ERROR    1 IN I1MACH - I OUT OF BOUNDS')
C
C     CALL FDUMP
C
      STOP
      END

*DECK D9PAK
      DOUBLE PRECISION FUNCTION D9PAK (Y, N)
C***BEGIN PROLOGUE  D9PAK
C***PURPOSE  Pack a base 2 exponent into a floating point number.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  A6B
C***TYPE      DOUBLE PRECISION (R9PAK-S, D9PAK-D)
C***KEYWORDS  FNLIB, PACK
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Pack a base 2 exponent into floating point number X.  This routine is
C almost the inverse of D9UPAK.  It is not exactly the inverse, because
C ABS(X) need not be between 0.5 and 1.0.  If both D9PAK and 2.d0**N
C were known to be in range we could compute
C               D9PAK = X *2.0d0**N
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9UPAK, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891009  Corrected error when XERROR called.  (WRB)
C   891009  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   901009  Routine used I1MACH(7) where it should use I1MACH(10),
C           Corrected (RWC)
C***END PROLOGUE  D9PAK
      DOUBLE PRECISION Y, A1N2B,A1N210,D1MACH
      LOGICAL FIRST
      SAVE NMIN, NMAX, A1N210, FIRST
      DATA A1N210 / 3.321928094 8873623478 7031942948 9 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9PAK
      IF (FIRST) THEN
         A1N2B = 1.0D0
         IF(I1MACH(10).NE.2) A1N2B=D1MACH(5)*A1N210
         NMIN = A1N2B*I1MACH(15)
         NMAX = A1N2B*I1MACH(16)
      ENDIF
      FIRST = .FALSE.
C
      CALL D9UPAK(Y,D9PAK,NY)
C
      NSUM=N+NY
      IF(NSUM.LT.NMIN)GO TO 40
      IF (NSUM .GT. NMAX) CALL XERMSG ('SLATEC', 'D9PAK',
     +   'PACKED NUMBER OVERFLOWS', 1, 2)
C
      IF (NSUM.EQ.0) RETURN
      IF(NSUM.GT.0) GO TO 30
C
 20   D9PAK = 0.5D0*D9PAK
      NSUM=NSUM+1
      IF(NSUM.NE.0) GO TO 20
      RETURN
C
 30   D9PAK = 2.0D0*D9PAK
      NSUM=NSUM - 1
      IF (NSUM.NE.0) GO TO 30
      RETURN
C
 40   CALL XERMSG ('SLATEC', 'D9PAK', 'PACKED NUMBER UNDERFLOWS', 1, 1)
      D9PAK = 0.0D0
      RETURN
C
      END

*DECK DCSEVL
      DOUBLE PRECISION FUNCTION DCSEVL (X, CS, N)
C***BEGIN PROLOGUE  DCSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DCSEVL
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X, D1MACH
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DCSEVL
      IF (FIRST) ONEPL = 1.0D0 + D1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2)
      IF (N .GT. 1000) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2)
      IF (ABS(X) .GT. ONEPL) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)
C
      B1 = 0.0D0
      B0 = 0.0D0
      TWOX = 2.0D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      DCSEVL = 0.5D0*(B0-B2)
C
      RETURN
      END

*DECK D1MACH
      DOUBLE PRECISION FUNCTION D1MACH (I)
C***BEGIN PROLOGUE  D1MACH
C***PURPOSE  Return floating point machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      DOUBLE PRECISION (R1MACH-S, D1MACH-D)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   D1MACH can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument, and can be referenced as follows:
C
C        D = D1MACH(I)
C
C   where I=1,...,5.  The (output) value of D above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C   D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   D1MACH( 3) = B**(-T), the smallest relative spacing.
C   D1MACH( 4) = B**(1-T), the largest relative spacing.
C   D1MACH( 5) = LOG10(B)
C
C   Assume double precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in I1MACH as
C   follows:
C   I1MACH(10) = B, the base.
C   I1MACH(14) = T, the number of base-B digits.
C   I1MACH(15) = EMIN, the smallest exponent E.
C   I1MACH(16) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment, the desired
C   set of DATA statements should be activated by removing the C from
C   column 1.  Also, the values of D1MACH(1) - D1MACH(4) should be
C   checked for consistency with the local operating system.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890213  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900618  Added DEC RISC constants.  (WRB)
C   900723  Added IBM RS 6000 constants.  (WRB)
C   900911  Added SUN 386i constants.  (WRB)
C   910710  Added HP 730 constants.  (SMR)
C   911114  Added Convex IEEE constants.  (WRB)
C   920121  Added SUN -r8 compiler option constants.  (WRB)
C   920229  Added Touchstone Delta i860 constants.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920625  Added CONVEX -p8 and -pd8 compiler option constants.
C           (BKS, WRB)
C   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
      SAVE DMACH
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING THE 68020/68881 COMPILER OPTION
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING SOFTWARE FLOATING POINT
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FDFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA SMALL(1), SMALL(2) / 16#00100000, 16#00000000 /
C     DATA LARGE(1), LARGE(2) / 16#7FFFFFFF, 16#FFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / 16#3CA00000, 16#00000000 /
C     DATA DIVER(1), DIVER(2) / 16#3CB00000, 16#00000000 /
C     DATA LOG10(1), LOG10(2) / 16#3FD34413, 16#509F79FF /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA SMALL(1) / ZC00800000 /
C     DATA SMALL(2) / Z000000000 /
C     DATA LARGE(1) / ZDFFFFFFFF /
C     DATA LARGE(2) / ZFFFFFFFFF /
C     DATA RIGHT(1) / ZCC5800000 /
C     DATA RIGHT(2) / Z000000000 /
C     DATA DIVER(1) / ZCC6800000 /
C     DATA DIVER(2) / Z000000000 /
C     DATA LOG10(1) / ZD00E730E7 /
C     DATA LOG10(2) / ZC77800DC0 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O0000000000000000 /
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O0007777777777777 /
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O7770000000000000 /
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O7777777777777777 /
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA SMALL(1) / Z"3001800000000000" /
C     DATA SMALL(2) / Z"3001000000000000" /
C     DATA LARGE(1) / Z"4FFEFFFFFFFFFFFE" /
C     DATA LARGE(2) / Z"4FFE000000000000" /
C     DATA RIGHT(1) / Z"3FD2800000000000" /
C     DATA RIGHT(2) / Z"3FD2000000000000" /
C     DATA DIVER(1) / Z"3FD3800000000000" /
C     DATA DIVER(2) / Z"3FD3000000000000" /
C     DATA LOG10(1) / Z"3FFF9A209A84FBCF" /
C     DATA LOG10(2) / Z"3FFFF7988F8959AC" /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA SMALL(1) / 00564000000000000000B /
C     DATA SMALL(2) / 00000000000000000000B /
C     DATA LARGE(1) / 37757777777777777777B /
C     DATA LARGE(2) / 37157777777777777777B /
C     DATA RIGHT(1) / 15624000000000000000B /
C     DATA RIGHT(2) / 00000000000000000000B /
C     DATA DIVER(1) / 15634000000000000000B /
C     DATA DIVER(2) / 00000000000000000000B /
C     DATA LOG10(1) / 17164642023241175717B /
C     DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn OR -pd8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CC0000000000000' /
C     DATA DMACH(4) / Z'3CD0000000000000' /
C     DATA DMACH(5) / Z'3FF34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'00010000000000000000000000000000' /
C     DATA DMACH(2) / Z'7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3F900000000000000000000000000000' /
C     DATA DMACH(4) / Z'3F910000000000000000000000000000' /
C     DATA DMACH(5) / Z'3FFF34413509F79FEF311F12B35816F9' /
C
C     MACHINE CONSTANTS FOR THE CRAY
C
C     DATA SMALL(1) / 201354000000000000000B /
C     DATA SMALL(2) / 000000000000000000000B /
C     DATA LARGE(1) / 577767777777777777777B /
C     DATA LARGE(2) / 000007777777777777774B /
C     DATA RIGHT(1) / 376434000000000000000B /
C     DATA RIGHT(2) / 000000000000000000000B /
C     DATA DIVER(1) / 376444000000000000000B /
C     DATA DIVER(2) / 000000000000000000000B /
C     DATA LOG10(1) / 377774642023241175717B /
C     DATA LOG10(2) / 000007571421742254654B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC DMACH(5)
C
C     DATA SMALL /    20K, 3*0 /
C     DATA LARGE / 77777K, 3*177777K /
C     DATA RIGHT / 31420K, 3*0 /
C     DATA DIVER / 32020K, 3*0 /
C     DATA LOG10 / 40423K, 42023K, 50237K, 74776K /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA DMACH(1) / '0000000000000010'X /
C     DATA DMACH(2) / 'FFFFFFFFFFFF7FFF'X /
C     DATA DMACH(3) / '0000000000003CC0'X /
C     DATA DMACH(4) / '0000000000003CD0'X /
C     DATA DMACH(5) / '79FF509F44133FF3'X /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FORMAT
C
C     DATA DMACH(1) / '0010000000000000'X /
C     DATA DMACH(2) / '7FEFFFFFFFFFFFFF'X /
C     DATA DMACH(3) / '3CA0000000000000'X /
C     DATA DMACH(4) / '3CB0000000000000'X /
C     DATA DMACH(5) / '3FD34413509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA SMALL(1), SMALL(2) / Z'00000000', Z'00100000'/
C     DATA LARGE(1), LARGE(2) / Z'FFFFFFFF', Z'7FEFFFFF'/
C     DATA RIGHT(1), RIGHT(2) / Z'00000000', Z'3CA00000'/
C     DATA DIVER(1), DIVER(2) / Z'00000000', Z'3CB00000'/
C     DATA LOG10(1), LOG10(2) / Z'509F79FF', Z'3FD34413'/
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING D_FLOATING
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
C
C     DATA SMALL(1), SMALL(2) /        128,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C     DATA DIVER(1), DIVER(2) /       9472,           0 /
C     DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING G_FLOATING
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
C
C     DATA SMALL(1), SMALL(2) /         16,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C     DATA DIVER(1), DIVER(2) /      15568,           0 /
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C     (ASSUMING REAL*8 IS THE DEFAULT DOUBLE PRECISION)
C
C     DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
C     DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
C     DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
C     DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
C     DATA LOG10(1), LOG10(2) / '3FD34413'X,'509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1), LARGE(2) / '37777777, '37777577 /
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000333 /
C     DATA DIVER(1), DIVER(2) / '20000000, '00000334 /
C     DATA LOG10(1), LOG10(2) / '23210115, '10237777 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA SMALL(1), SMALL(2) / O402400000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O376777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O604400000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O606400000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O776464202324, O117571775714 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     THREE WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
C     DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
C     DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
C     DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     FOUR WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) /  40000B,       0 /
C     DATA SMALL(3), SMALL(4) /       0,       1 /
C     DATA LARGE(1), LARGE(2) /  77777B, 177777B /
C     DATA LARGE(3), LARGE(4) / 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
C     DATA RIGHT(3), RIGHT(4) /       0,    225B /
C     DATA DIVER(1), DIVER(2) /  40000B,       0 /
C     DATA DIVER(3), DIVER(4) /       0,    227B /
C     DATA LOG10(1), LOG10(2) /  46420B,  46502B /
C     DATA LOG10(3), LOG10(4) /  76747B, 176377B /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
C     DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
C     DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
C     DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
C     DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA SMALL(1), SMALL(2) / Z00100000, Z00000000 /
C     DATA LARGE(1), LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z33100000, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z34100000, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C     ASSUMES THAT ALL ARITHMETIC IS DONE IN DOUBLE PRECISION
C     ON 8088, I.E., NOT IN 80 BIT FORM FOR THE 8087.
C
C     DATA SMALL(1) / 2.23D-308  /
C     DATA LARGE(1) / 1.79D+308  /
C     DATA RIGHT(1) / 1.11D-16   /
C     DATA DIVER(1) / 2.22D-16   /
C     DATA LOG10(1) / 0.301029995663981195D0 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
C
C     DATA SMALL(1), SMALL(2) / "033400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "344777777777 /
C     DATA RIGHT(1), RIGHT(2) / "113400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "114400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
C
C     DATA SMALL(1), SMALL(2) / "000400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "377777777777 /
C     DATA RIGHT(1), RIGHT(2) / "103400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "104400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "476747767461 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    8388608,           0 /
C     DATA LARGE(1), LARGE(2) / 2147483647,          -1 /
C     DATA RIGHT(1), RIGHT(2) /  612368384,           0 /
C     DATA DIVER(1), DIVER(2) /  620756992,           0 /
C     DATA LOG10(1), LOG10(2) / 1067065498, -2063872008 /
C
C     DATA SMALL(1), SMALL(2) / O00040000000, O00000000000 /
C     DATA LARGE(1), LARGE(2) / O17777777777, O37777777777 /
C     DATA RIGHT(1), RIGHT(2) / O04440000000, O00000000000 /
C     DATA DIVER(1), DIVER(2) / O04500000000, O00000000000 /
C     DATA LOG10(1), LOG10(2) / O07746420232, O20476747770 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    128,      0 /
C     DATA SMALL(3), SMALL(4) /      0,      0 /
C     DATA LARGE(1), LARGE(2) /  32767,     -1 /
C     DATA LARGE(3), LARGE(4) /     -1,     -1 /
C     DATA RIGHT(1), RIGHT(2) /   9344,      0 /
C     DATA RIGHT(3), RIGHT(4) /      0,      0 /
C     DATA DIVER(1), DIVER(2) /   9472,      0 /
C     DATA DIVER(3), DIVER(4) /      0,      0 /
C     DATA LOG10(1), LOG10(2) /  16282,   8346 /
C     DATA LOG10(3), LOG10(4) / -31493, -12296 /
C
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /
C     DATA SMALL(3), SMALL(4) / O000000, O000000 /
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /
C     DATA LARGE(3), LARGE(4) / O177777, O177777 /
C     DATA RIGHT(1), RIGHT(2) / O022200, O000000 /
C     DATA RIGHT(3), RIGHT(4) / O000000, O000000 /
C     DATA DIVER(1), DIVER(2) / O022400, O000000 /
C     DATA DIVER(3), DIVER(4) / O000000, O000000 /
C     DATA LOG10(1), LOG10(2) / O037632, O020232 /
C     DATA LOG10(3), LOG10(4) / O102373, O147770 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'00010000000000000000000000000000' /
C     DATA DMACH(2) / Z'7FFEFFFFFFFFFFFFFFFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3F8E0000000000000000000000000000' /
C     DATA DMACH(4) / Z'3F8F0000000000000000000000000000' /
C     DATA DMACH(5) / Z'3FFD34413509F79FEF311F12B35816F9' /
C
C     MACHINE CONSTANTS FOR THE SUN 386i
C
C     DATA SMALL(1), SMALL(2) / Z'FFFFFFFD', Z'000FFFFF' /
C     DATA LARGE(1), LARGE(2) / Z'FFFFFFB0', Z'7FEFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'000000B0', Z'3CA00000' /
C     DATA DIVER(1), DIVER(2) / Z'FFFFFFCB', Z'3CAFFFFF'
C     DATA LOG10(1), LOG10(2) / Z'509F79E9', Z'3FD34413' /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
C
C     DATA SMALL(1), SMALL(2) / O000040000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O377777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O170540000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O170640000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O177746420232, O411757177572 /
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1 .OR. I .GT. 5) CALL XERMSG ('SLATEC', 'D1MACH',
     +   'I OUT OF BOUNDS', 1, 2)
C
      D1MACH = DMACH(I)
      RETURN
C
      END

*DECK CARG
      FUNCTION CARG (Z)
C***BEGIN PROLOGUE  CARG
C***PURPOSE  Compute the argument of a complex number.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  A4A
C***TYPE      COMPLEX (CARG-C)
C***KEYWORDS  ARGUMENT OF A COMPLEX NUMBER, ELEMENTARY FUNCTIONS, FNLIB
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C CARG(Z) calculates the argument of the complex number Z.  Note
C that CARG returns a real result.  If Z = X+iY, then CARG is ATAN(Y/X),
C except when both X and Y are zero, in which case the result
C will be zero.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  CARG
      COMPLEX Z
C***FIRST EXECUTABLE STATEMENT  CARG
      CARG = 0.0
      IF (REAL(Z).NE.0. .OR. AIMAG(Z).NE.0.) CARG =
     1  ATAN2 (AIMAG(Z), REAL(Z))
C
      RETURN
      END

*DECK XERCLR
      SUBROUTINE XERCLR
C***BEGIN PROLOGUE  XERCLR
C***PURPOSE  Reset current error number to zero.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERCLR-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        This routine simply resets the current error number to zero.
C        This may be necessary in order to determine that a certain
C        error has occurred again since the last time NUMXER was
C        referenced.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERCLR
C***FIRST EXECUTABLE STATEMENT  XERCLR
      JUNK = J4SAVE(1,0,.TRUE.)
      RETURN
      END

*DECK R9UPAK
      SUBROUTINE R9UPAK (X, Y, N)
C***BEGIN PROLOGUE  R9UPAK
C***PURPOSE  Unpack a floating point number X so that X = Y*2**N.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  A6B
C***TYPE      SINGLE PRECISION (R9UPAK-S, D9UPAK-D)
C***KEYWORDS  FNLIB, UNPACK
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   Unpack a floating point number X so that X = Y*2.0**N, where
C   0.5 .LE. ABS(Y) .LT. 1.0.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   780701  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  R9UPAK
C***FIRST EXECUTABLE STATEMENT  R9UPAK
      ABSX = ABS(X)
      N = 0
      IF (X.EQ.0.0E0) GO TO 30
C
   10 IF (ABSX.GE.0.5E0) GO TO 20
      N = N-1
      ABSX = ABSX*2.0E0
      GO TO 10
C
   20 IF (ABSX.LT.1.0E0) GO TO 30
      N = N+1
      ABSX = ABSX*0.5E0
      GO TO 20
C
   30 Y = SIGN(ABSX,X)
      RETURN
C
      END

*DECK INITS
      FUNCTION INITS (OS, NOS, ETA)
C***BEGIN PROLOGUE  INITS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      SINGLE PRECISION (INITS-S, INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that INITS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     single precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  INITS
      REAL OS(*)
C***FIRST EXECUTABLE STATEMENT  INITS
      IF (NOS .LT. 1) CALL XERMSG ('SLATEC', 'INITS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(OS(I))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERMSG ('SLATEC', 'INITS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITS = I
C
      RETURN
      END

*DECK CSEVL
      FUNCTION CSEVL (X, CS, N)
C***BEGIN PROLOGUE  CSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      SINGLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  CSEVL
      REAL B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  CSEVL
      IF (FIRST) ONEPL = 1.0E0 + R1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2)
      IF (N .GT. 1000) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2)
      IF (ABS(X) .GT. ONEPL) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)
C
      B1 = 0.0E0
      B0 = 0.0E0
      TWOX = 2.0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      CSEVL = 0.5E0*(B0-B2)
C
      RETURN
      END



*DECK ACOS
      REAL FUNCTION ACOS (X)
C***BEGIN PROLOGUE  ACOS
C***PURPOSE  Compute the arccosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (ACOS-S, DACOS-D)
C***KEYWORDS  ACOS, ARCCOSINE, ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ACOS, X, Y
C      Y = ACOS(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 ACOS.
C
C *Function Return Values:
C
C    ACOS    :    the arccosine of X.
C
C *Description:
C
C    ACOS evaluates the arccosine of an argument.  The argument X
C    must be in the interval -1.0 .LE. X .LE. +1.0.  For arguments
C    outside the permitted interval, an error message is generated
C    by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ASIN, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900516  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ACOS
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL PI2
C     .. External Functions ..
      REAL ASIN
C      EXTERNAL ASIN
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     .. Data statements ..
      DATA PI2 / 1.5707963267 9489661923E0/
C***FIRST EXECUTABLE STATEMENT  ACOS
      IF (ABS(X) .GT. 1.0E0) CALL XERMSG ('SLATEC', 'ACOS',
     +  'ABS(X) GREATER THAN 1', 1, 2)
C
      ACOS = PI2 - ASIN(X)
C
      RETURN
      END
*DECK ALOG
      REAL FUNCTION ALOG (X)
C***BEGIN PROLOGUE  ALOG
C***PURPOSE  Compute the natural logarithm of a number.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      SINGLE PRECISION (ALOG-S, DLOG-D, CLOG-C)
C***KEYWORDS  ALOG, ELEMENTARY FUNCTION, FORTRAN INTRINSIC,
C             NATURAL LOGARITHM
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ALOG, X, Y
C      Y = ALOG(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 ALOG.
C
C *Function Return Values:
C
C    ALOG    :    the natural logarithm of X.
C
C *Description:
C
C    ALOG evaluates the natural logarithm of an argument.  The
C    argument X must be positive.  For a non-positive argument, an
C    error message is generated by XERMSG and the routine aborts.
C
C
C    SERIES FOR ALN     ON THE INTERVAL  0.          TO  3.46021D-03
C                                        WITH WEIGHTED ERROR   1.50E-16
C                                         LOG WEIGHTED ERROR  15.82
C                               SIGNIFICANT FIGURES REQUIRED  15.65
C                                    DECIMAL PLACES REQUIRED  16.21
C
C    ALN2 = ALOG(2.0) - 0.625
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, R9UPAK, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900517  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ALOG
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL ALN2, T, T2, XN, Y
      INTEGER N, NTERMS, NTRVAL
C     .. Local Arrays ..
      REAL ALNCEN(5), ALNCS(6), CENTER(4)
C     .. External Functions ..
C      REAL CSEVL, R1MACH
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, INITS
C     .. External Subroutines ..
      EXTERNAL XERMSG
C      EXTERNAL R9UPAK, XERMSG
C     .. Save statement ..
      SAVE NTERMS
C     .. Data statements ..
      DATA ALNCS(1) / 1.3347199877 973882E0 /
      DATA ALNCS(2) /  .0006937562 83284112E0 /
      DATA ALNCS(3) /  .0000004293 40390204E0 /
      DATA ALNCS(4) /  .0000000002 89338477E0 /
      DATA ALNCS(5) /  .0000000000 00205125E0 /
      DATA ALNCS(6) /  .0000000000 00000150E0 /
      DATA CENTER(1) / 1.0 /
      DATA CENTER(2) / 1.25 /
      DATA CENTER(3) / 1.50 /
      DATA CENTER(4) / 1.75 /
      DATA ALNCEN(1) / 0.0E0 /
      DATA ALNCEN(2) / +.2231435513 14209755 E+0 /
      DATA ALNCEN(3) / +.4054651081 08164381 E+0 /
      DATA ALNCEN(4) / +.5596157879 35422686 E+0 /
      DATA ALNCEN(5) / +.6931471805 59945309 E+0 /
      DATA ALN2 / 0.0681471805 59945309E0 /
      DATA NTERMS / 0 /
C***FIRST EXECUTABLE STATEMENT  ALOG
      IF (NTERMS .EQ. 0) NTERMS = INITS(ALNCS, 6, 28.9*R1MACH(3))
C
      IF (X .LE. 0.0E0) CALL XERMSG ('SLATEC', 'ALOG',
     +  'X IS ZERO OR NEGATIVE', 1, 2)
C
      CALL R9UPAK (X, Y, N)
C
      XN = N - 1
      Y = 2.0E0*Y
      NTRVAL = 4.0E0*Y - 2.5E0
      IF (NTRVAL .EQ. 5) T = ((Y-1.0E0)-1.0E0) / (Y+2.0E0)
      IF (NTRVAL .LT. 5) T = (Y-CENTER(NTRVAL))/(Y+CENTER(NTRVAL))
      T2 = T*T
C
      ALOG = 0.625E0*XN + (ALN2*XN + ALNCEN(NTRVAL) + 2.0E0*T +
     +  T*T2*CSEVL(578.0E0*T2-1.0E0, ALNCS, NTERMS) )
C
      RETURN
      END
*DECK ALOG10
      REAL FUNCTION ALOG10 (X)
C***BEGIN PROLOGUE  ALOG10
C***PURPOSE  Compute the common base 10 logarithm.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      SINGLE PRECISION (ALOG10-S, DLOG10-D)
C***KEYWORDS  ALOG10, BASE 10 LOGARITHM, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ALOG10, X, Y
C      Y = ALOG10(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 ALOG.
C
C
C *Function Return Values:
C
C    ALOG10  :    the  base 10, i.e. common, logarithm.
C
C *Description:
C
C    ALOG10 evaluates the base 10 logarithm of an argument.  The
C    argument X must be positive.  For a non-positive argument, an
C    error message is generated by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALOG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900517  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ALOG10
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL ALOGE
C     .. External Functions ..
      REAL ALOG
C      EXTERNAL ALOG
C     .. Data statements ..
      DATA ALOGE / 0.4342944819 0325182765E0 /
C***FIRST EXECUTABLE STATEMENT  ALOG10
      ALOG10 = ALOGE * ALOG(X)
C
      RETURN
      END
*DECK ASIN
      REAL FUNCTION ASIN (X)
C***BEGIN PROLOGUE  ASIN
C***PURPOSE  Compute the arcsine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (ASIN-S, DASIN-D)
C***KEYWORDS  ARCSINE, ASIN, ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ASIN, X, Y
C      Y = ASIN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 ASIN.
C
C *Function Return Values:
C
C    ASIN    :    the arcsine of X.
C
C *Description:
C
C    ASIN evaluates the arcsine of an argument.  The absolute
C    value of the argument X must be less than or equal to 1.0.
C    For arguments outside the permitted interval, an error message
C    is generated by XERMSG and the routine aborts.
C
C    SERIES FOR ASIN    ON THE INTERVAL  0.          TO  5.00000D-01
C                                        WITH WEIGHTED ERROR   1.60E-17
C                                         LOG WEIGHTED ERROR  16.79
C                               SIGNIFICANT FIGURES REQUIRED  15.67
C                                    DECIMAL PLACES REQUIRED  17.45
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, SQRT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ASIN
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL PI2, SQEPS, Y, Z
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL ASINCS(20)
C     .. External Functions ..
      REAL CSEVL, R1MACH, SQRT      
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, SIGN
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS
C     .. Data statements ..
      DATA ASINCS( 1) / .1024639175 3227159E0 /
      DATA ASINCS( 2) / .0549464872 21245833E0 /
      DATA ASINCS( 3) / .0040806303 92544969E0 /
      DATA ASINCS( 4) / .0004078900 68546044E0 /
      DATA ASINCS( 5) / .0000469853 67432203E0 /
      DATA ASINCS( 6) / .0000058809 75813970E0 /
      DATA ASINCS( 7) / .0000007773 23124627E0 /
      DATA ASINCS( 8) / .0000001067 74233400E0 /
      DATA ASINCS( 9) / .0000000150 92399536E0 /
      DATA ASINCS(10) / .0000000021 80972408E0 /
      DATA ASINCS(11) / .0000000003 20759842E0 /
      DATA ASINCS(12) / .0000000000 47855369E0 /
      DATA ASINCS(13) / .0000000000 07225128E0 /
      DATA ASINCS(14) / .0000000000 01101833E0 /
      DATA ASINCS(15) / .0000000000 00169476E0 /
      DATA ASINCS(16) / .0000000000 00026261E0 /
      DATA ASINCS(17) / .0000000000 00004095E0 /
      DATA ASINCS(18) / .0000000000 00000642E0 /
      DATA ASINCS(19) / .0000000000 00000101E0 /
      DATA ASINCS(20) / .0000000000 00000016E0 /
      DATA PI2 / 1.5707963267 9489661923E0/
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  ASIN
      IF (FIRST) THEN
        NTERMS = INITS(ASINCS, 20, 0.1*R1MACH(3))
        SQEPS = SQRT(6.0E0*R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GT. 1.0E0) CALL XERMSG ('SLATEC', 'ASIN',
     +    'ABS(X) GREATER THAN 1', 1, 2)
C
      Z = 0.0E0
      IF (Y .GT. SQEPS) Z = Y*Y
      IF (Z .LE. 0.5E0) THEN
        ASIN = X*(1.0E0 + CSEVL(4.0E0*Z-1.0E0, ASINCS, NTERMS))
      ELSE
        ASIN = PI2 - SQRT(1.0E0-Z)*(1.0E0 + CSEVL(3.0E0-4.0E0*Z,
     +    ASINCS, NTERMS))
      ENDIF
      IF (X .NE. 0.0E0) ASIN = SIGN (ASIN, X)
C
      RETURN
      END
*DECK ATAN
      REAL FUNCTION ATAN (X)
C***BEGIN PROLOGUE  ATAN
C***PURPOSE  Compute the arctangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (ATAN-S, DATAN-D)
C***KEYWORDS  ARCTANGENT, ATAN, ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ATAN, X, Y
C      Y = ATAN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 ATAN.
C
C *Function Return Values:
C
C    ATAN    :    the arctangent.
C
C *Description:
C
C    ATAN evaluates the arctangent of an argument.
C
C    SERIES FOR ATAN    ON THE INTERVAL  0.          TO  4.00000D-02
C                                        WITH WEIGHTED ERROR   1.00E-17
C                                         LOG WEIGHTED ERROR  17.00
C                               SIGNIFICANT FIGURES REQUIRED  16.38
C                                    DECIMAL PLACES REQUIRED  17.48
C
C    XBNDN = TAN((2*N-1)*PI/16.0)
C
C    TANP8(N) = TAN(N*PI/8.)
C
C    CONPI8(N) + PI8(N) = N*PI/8.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, SQRT
C***REVISION HISTORY  (YYMMDD)
C   780101  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ATAN
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL SQEPS, T, XBIG, XBND1, XBND2, XBND3, XBND4, Y
      INTEGER N, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL ATANCS(9), CONPI8(4), PI8(4), TANP8(3)
C     .. External Functions ..
      REAL CSEVL, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS, SIGN
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XBIG
C     .. Data statements ..
      DATA ATANCS(1) /  .4869011034 9241406E0 /
      DATA ATANCS(2) / -.0065108316 36717464E0 /
      DATA ATANCS(3) /  .0000383458 28265245E0 /
      DATA ATANCS(4) / -.0000002687 22128762E0 /
      DATA ATANCS(5) /  .0000000020 50093098E0 /
      DATA ATANCS(6) / -.0000000000 16450717E0 /
      DATA ATANCS(7) /  .0000000000 00136509E0 /
      DATA ATANCS(8) / -.0000000000 00001160E0 /
      DATA ATANCS(9) /  .0000000000 00000010E0 /
      DATA XBND1 /  .1989123673 79658006 E+0 /
      DATA XBND2 /  .6681786379 19298919 E+0 /
      DATA XBND3 / 1.496605762 66548901 E+0 /
      DATA XBND4 / 5.027339492 12584810 E+0 /
      DATA TANP8(1) /  .4142135623 73095048 E+0 /
      DATA TANP8(2) / 1.0 E0 /
      DATA TANP8(3) / 2.414213562 37309504 E+0 /
      DATA CONPI8(1) / 0.375 E0 /
      DATA CONPI8(2) / 0.75 E0 /
      DATA CONPI8(3) / 1.125 E0 /
      DATA CONPI8(4) / 1.5   E0 /
      DATA PI8(1) / 0.1769908169 87241548 E-1 /
      DATA PI8(2) / 0.3539816339 74483096 E-1 /
      DATA PI8(3) / 0.5309724509 61724644 E-1 /
      DATA PI8(4) / 0.0707963267 948966192 E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  ATAN
      IF (FIRST) THEN
        NTERMS = INITS(ATANCS, 9, 0.1*R1MACH(3))
        SQEPS = SQRT(6.0E0*R1MACH(3))
        XBIG = 1.0E0/R1MACH(3)
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .LE. XBND1) THEN
C
        ATAN = X
        IF (Y .GT. SQEPS) ATAN = X*(0.75E0+CSEVL(50.0E0*Y**2-1.0E0,
     +    ATANCS, NTERMS))
      ELSE
C
        IF (Y .LE. XBND4) THEN
C
          N = 1
          IF (Y .GT. XBND2) N = 2
          IF (Y .GT. XBND3) N = 3
C
          T = (Y - TANP8(N)) / (1.0E0 + Y*TANP8(N))
          ATAN = SIGN(CONPI8(N) + (PI8(N) + T*(0.75E0 +
     +      CSEVL(50.0E0*T**2-1.0E0, ATANCS, NTERMS))), X)
        ELSE
C
          ATAN = CONPI8(4) + PI8(4)
          IF (Y .LT. XBIG) ATAN = CONPI8(4) + (PI8(4) - (0.75E0 +
     +      CSEVL(50.0E0/Y**2-1.0E0, ATANCS, NTERMS))/Y )
          ATAN = SIGN(ATAN, X)
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK ATAN2
      REAL FUNCTION ATAN2 (SN, CS)
C***BEGIN PROLOGUE  ATAN2
C***PURPOSE  Compute the arctangent in the proper quadrant.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (ATAN2-S, DATAN2-D)
C***KEYWORDS  ARCTANGENT, ATAN, ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL ATAN2, SN, CS, Y
C      Y = ATAN2(SN, CS)
C
C *Arguments:
C
C    SN      :IN  This is an argument.  It will not be modified by
C                 ATAN2.
C    CS      :IN  This is an argument.  It will not be modified by
C                 ATAN2.
C
C *Function Return Values:
C
C    ATAN2   :    the arctangent of SN and CS in the proper quadrant.
C
C *Description:
C
C    ATAN2 evaluates the arctangent of two arguments in the proper
C    quadrant.  The arguments SN and CS must not both be zero.  If
C    both of the arguments are zero, an error message is generated
C    by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ATAN, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  ATAN2
C     .. Scalar Arguments ..
      REAL CS, SN
C     .. Local Scalars ..
      REAL PI
C     .. External Functions ..
      REAL ATAN
C      EXTERNAL ATAN
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC SIGN
C     .. Data statements ..
      DATA PI / 3.1415926535 8979323846E0/
C***FIRST EXECUTABLE STATEMENT  ATAN2
      IF (CS .NE. 0.0E0) THEN
        ATAN2 = ATAN(SN/CS)
        IF (CS .LT. 0.0E0) ATAN2 = ATAN2 + PI
        IF (ATAN2 .GT. PI) ATAN2 = ATAN2 - 2.0*PI
      ELSE
        IF (SN .EQ. 0.0E0) CALL XERMSG ('SLATEC', 'ATAN2',
     +    'BOTH ARGUMENTS ARE ZERO', 1, 2)
        ATAN2 = SIGN(0.5E0*PI, SN)
      ENDIF
C
      RETURN
      END
*DECK CABS
      REAL FUNCTION CABS (Z)
C***BEGIN PROLOGUE  CABS
C***PURPOSE  Compute the absolute value of a complex number.
C***LIBRARY   FNLIB
C***CATEGORY  C4
C***TYPE      COMPLEX (CABS-C)
C***KEYWORDS  CABS, COMPLEX ABSOLUTE VALUE, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL A, CABS
C      COMPLEX Z
C      A = CABS(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CABS.
C
C *Function Return Values:
C
C    CABS    :    the absolute value of a complex argument.
C
C *Description:
C
C    CABS evaluates the absolute value of a complex argument.  If the
C    real part of the argument Z is X and the imaginary part is Y, then
C    CABS(Z) mathematically is SQRT(X**2+Y**2).
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  R1MACH, SQRT
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CABS
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. Local Scalars ..
      REAL R1, R2, SQEPS, X, Y
      LOGICAL FIRST
C     .. External Functions ..
      EXTERNAL SQRT
C      REAL R1MACH, SQRT
C      EXTERNAL R1MACH, SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS, AIMAG, MAX, MIN, REAL
C     .. Save statement ..
      SAVE FIRST, SQEPS
C     .. Data statements ..
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  CABS
      IF (FIRST) THEN
        SQEPS = SQRT(R1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      X = ABS(REAL(Z))
      Y = ABS(AIMAG(Z))
      R1 = MIN(X, Y)
      R2 = MAX(X, Y)
C
      CABS = R2
      IF (R1 .GT. R2*SQEPS) CABS = R2*SQRT(1.0E0+(R1/R2)**2)
C
      RETURN
      END
*DECK CCOS
      COMPLEX FUNCTION CCOS (Z)
C***BEGIN PROLOGUE  CCOS
C***PURPOSE  Compute the cosine of a complex argument.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      COMPLEX (COS-S, DCOS-D, CCOS-C)
C***KEYWORDS  CCOS, COMPLEX COSINE, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX CCOS, Y, Z
C      Y = CCOS(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CCOS.
C
C *Function Return Values:
C
C    CCOS    :    the cosine of a complex argument.
C
C *Description:
C
C    CCOS evaluates the cosine of a complex argument.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  COS, COSH, SIN, SINH, XERCLR
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CCOS
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. Local Scalars ..
      REAL CS, X, Y
C     .. External Functions ..
      REAL COS, COSH, SIN, SINH
C      EXTERNAL COS, COSH, SIN, SINH
C     .. External Subroutines ..
C      EXTERNAL XERCLR
C     .. Intrinsic Functions ..
      INTRINSIC AIMAG, CMPLX, REAL
C***FIRST EXECUTABLE STATEMENT  CCOS
      X = REAL(Z)
      Y = AIMAG(Z)
C
      CS = COS(X)
      CALL XERCLR
C
      CCOS = CMPLX(CS*COSH(Y), -SIN(X)*SINH(Y))
C
      RETURN
      END
*DECK CEXP
      COMPLEX FUNCTION CEXP (Z)
C***BEGIN PROLOGUE  CEXP
C***PURPOSE  Compute the complex exponential.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      COMPLEX (EXP-S, DEXP-D, CEXP-C)
C***KEYWORDS  CEXP, COMPLEX EXPONENTIAL, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX CEXP, Y, Z
C      Y = CEXP(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CEXP.
C
C *Function Return Values:
C
C    CEXP    :    the exponential of a complex argument.
C
C *Description:
C
C    CEXP evaluates the exponential, i.e., exp(Z) or e**Z.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  COS, EXP, SIN
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CEXP
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. Local Scalars ..
      REAL R, Y
C     .. External Functions ..
      REAL COS, EXP, SIN
C      EXTERNAL COS, EXP, SIN
C     .. Intrinsic Functions ..
      INTRINSIC AIMAG, CMPLX, REAL
C***FIRST EXECUTABLE STATEMENT  CEXP
      R = EXP(REAL(Z))
      IF (R .EQ. 0.0E0) THEN
        CEXP = (0.0E0, 0.0E0)
      ELSE
        Y = AIMAG(Z)
        CEXP = CMPLX(R*COS(Y), R*SIN(Y))
      ENDIF
C
      RETURN
      END
*DECK CLOG
      COMPLEX FUNCTION CLOG (Z)
C***BEGIN PROLOGUE  CLOG
C***PURPOSE  Compute the complex natural logarithm.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      COMPLEX (ALOG-S, DLOG-D, CLOG-C)
C***KEYWORDS  CLOG, COMPLEX NATURAL LOGARITHM, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX CLOG, Y, Z
C      Y = CLOG(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CLOG.
C
C *Function Return Values:
C
C    CLOG    :    the natural logarithm of a complex argument.
C
C *Description:
C
C    CLOG evaluates the natural logarithm of a complex argument.
C
C***REFERENCES  CARG
C***ROUTINES CALLED  ALOG, CABS, CARG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CLOG
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. External Functions ..
      REAL ALOG, CABS, CARG
C      EXTERNAL ALOG, CABS, CARG
C     .. Intrinsic Functions ..
      INTRINSIC CMPLX
C***FIRST EXECUTABLE STATEMENT  CLOG
      CLOG = CMPLX(ALOG(CABS(Z)), CARG(Z))
C
      RETURN
      END
*DECK COS
      REAL FUNCTION COS (X)
C***BEGIN PROLOGUE  COS
C***PURPOSE  Compute the cosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (COS-S, DCOS-D, CCOS-C)
C***KEYWORDS  COS, COSINE, ELEMENTARY FUNCTION, FORTRAN INTRNSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL COS, X, Y
C      Y = COS(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 COS.
C
C *Function Return Values:
C
C    COS     :    the cosine of X.
C
C *Description:
C
C    COS evaluates the cosine of an argument.  If the argument X is
C    greater than the reciprocal of R1MACH(4), an error message is
C    generated by XERMSG and the routine aborts.
C
C    SERIES FOR SIN     ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   2.47E-18
C                                         LOG WEIGHTED ERROR  17.61
C                               SIGNIFICANT FIGURES REQUIRED  16.20
C                                    DECIMAL PLACES REQUIRED  18.06
C
C
C    SERIES FOR COS     ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   4.71E-17
C                                         LOG WEIGHTED ERROR  16.33
C                               SIGNIFICANT FIGURES REQUIRED  15.58
C                                    DECIMAL PLACES REQUIRED  16.78
C
C    PI4REC = 4.0/PI - 1.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, SQRT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  COS
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL AINTY, AINTY2, ETA, PI4REC, XMAX, XSML, XWARN, Y, YREM, Z
      INTEGER IFN, NOCTNT, NTCS, NTSN
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL COSCS(8), SINCS(8)
C     .. External Functions ..
      REAL CSEVL, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, AINT, AMOD, MOD
C     .. Save statement ..
      SAVE FIRST, NTCS, NTSN, XMAX, XSML, XWARN
C     .. Data statements ..
      DATA SINCS( 1) / -.0081040790 85448715E0 /
      DATA SINCS( 2) / -.0391445675 27081957E0 /
      DATA SINCS( 3) /  .0003045094 20678944E0 /
      DATA SINCS( 4) / -.0000011235 74976796E0 /
      DATA SINCS( 5) /  .0000000024 14039972E0 /
      DATA SINCS( 6) / -.0000000000 03391636E0 /
      DATA SINCS( 7) /  .0000000000 00003358E0 /
      DATA SINCS( 8) / -.0000000000 00000002E0 /
      DATA COSCS( 1) /  .2032638274 0961603E0 /
      DATA COSCS( 2) / -.1464366443 9083686E0 /
      DATA COSCS( 3) /  .0019214493 11814646E0 /
      DATA COSCS( 4) / -.0000099649 68489829E0 /
      DATA COSCS( 5) /  .0000000275 76595607E0 /
      DATA COSCS( 6) / -.0000000000 47399498E0 /
      DATA COSCS( 7) /  .0000000000 00055495E0 /
      DATA COSCS( 8) / -.0000000000 00000047E0 /
      DATA PI4REC / +.2732395447 35162686 E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  COS
      IF (FIRST) THEN
        ETA = 0.1E0*R1MACH(3)
        NTSN = INITS(SINCS, 8, ETA)
        NTCS = INITS(COSCS, 8, ETA)
        XSML = SQRT (2.0E0*R1MACH(3))
        XMAX = 1.0E0/R1MACH(4)
        XWARN = SQRT(XMAX)
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'COS',
     +  'NO PRECISION BECAUSE ABS(X) IS TOO BIG', 2, 2)
      IF (Y .GT. XWARN) CALL XERMSG ('SLATEC', 'COS',
     +  'ANSWER LT HALF PRECISION BECAUSE ABS(X) IS BIG', 1, 1)
C
      IF (Y  .LT.  XSML) THEN
        COS = 1.0E0
      ELSE
C
C       CAREFULLY COMPUTE Y * (4/PI)
C         = (AINT(Y) + REM(Y)) * (1.0 + PI4REC)
C         = AINT(Y) + REM(Y) + Y*PI4REC
C         = AINT(Y) + AINT(REM(Y)+Y*PI4REC) + REM(REM(Y)+Y*PI4REC)
C
        AINTY = AINT(Y)
        YREM = Y - AINTY
        Y = YREM + Y*PI4REC
        AINTY2 = AINT(Y)
        AINTY = AINTY + AINTY2
        Y = Y - AINTY2
C
        NOCTNT = AMOD(AINTY, 8.0E0)
        IFN = MOD((NOCTNT+1)/2, 2)
        IF (MOD(NOCTNT,2) .EQ. 1) Y = 1.0E0 - Y
        Z = 2.0E0*Y**2 - 1.0E0
C
        IF (IFN .NE. 1) THEN
          COS = 0.75E0 +CSEVL(Z,COSCS, NTCS)
        ELSE
          COS = Y*(0.75E0 + CSEVL (Z, SINCS, NTSN))
        ENDIF
        IF (NOCTNT .GT. 1 .AND. NOCTNT  .LT.  6) COS = -COS
      ENDIF
C
      RETURN
      END
*DECK COSH
      REAL FUNCTION COSH (X)
C***BEGIN PROLOGUE  COSH
C***PURPOSE  Compute the hyperbolic cosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      SINGLE PRECISION (COSH-S, DCOSH-D)
C***KEYWORDS  COSH, ELEMENTARY FUNCTION, FORTRAN INTRINSIC,
C             HYPERBOLIC COSINE
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS NATIONAL LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX COSH, X, Y
C      Y = COSH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 COSH.
C
C *Function Return Values:
C
C    COSH    :    the hyperbolic cosine of X.
C
C *Description:
C
C    COSH evaluates the hyperbolic cosine of an argument.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  EXP, R1MACH, SQRT
C***REVISION HISTORY  (YYMMDD)
C   770810  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  COSH
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL Y, YMAX
C     .. External Functions ..
      REAL EXP, R1MACH, SQRT
C      EXTERNAL EXP, R1MACH, SQRT
      EXTERNAL SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C     .. Data statements ..
      DATA YMAX / 0.0 /
C***FIRST EXECUTABLE STATEMENT  COSH
      IF (YMAX .EQ. 0.0E0) YMAX = 1.0E0/SQRT(R1MACH(3))
C
      Y = EXP(ABS(X))
      COSH = 0.5E0*Y
      IF (Y .LT. YMAX) COSH = 0.5E0 * (Y + 1.0E0/Y)
C
      RETURN
      END
*DECK CSIN
      COMPLEX FUNCTION CSIN (Z)
C***BEGIN PROLOGUE  CSIN
C***PURPOSE  Compute the complex sine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      COMPLEX (SIN-S, DSIN-D, CSIN-C)
C***KEYWORDS  COMPLEX SINE, CSIN, ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  Fullerton, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX CSIN, Y, Z
C      Y = CSIN(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CSIN.
C
C *Function Return Values:
C
C    CSIN    :    the sine of a complex argument.
C
C *Description:
C
C    CSIN evaluates the sine of a complex argument.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  COS, COSH, SIN, SINH, XERCLR
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900518  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CSIN
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. Local Scalars ..
      REAL SN, X, Y
C     .. External Functions ..
      REAL COS, COSH, SIN, SINH
C      EXTERNAL COS, COSH, SIN, SINH
C     .. External Subroutines ..
C      EXTERNAL XERCLR
C     .. Intrinsic Functions ..
      INTRINSIC AIMAG, CMPLX, REAL
C***FIRST EXECUTABLE STATEMENT  CSIN
      X = REAL(Z)
      Y = AIMAG(Z)
C
      SN = SIN(X)
      CALL XERCLR
C
      CSIN = CMPLX(SN*COSH(Y), COS(X)*SINH(Y))
C
      RETURN
      END
*DECK CSQRT
      COMPLEX FUNCTION CSQRT (Z)
C***BEGIN PROLOGUE  CSQRT
C***PURPOSE  Compute the complex square root.
C***LIBRARY   FNLIB
C***CATEGORY  C2
C***TYPE      COMPLEX (SQRT-S, DSQRT-D, CSQRT-C)
C***KEYWORDS  COMPLEX SQUARE ROOT, CSQRT, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      COMPLEX CSQRT, Y, Z
C      Y = CSQRT(Z)
C
C *Arguments:
C
C    Z       :IN  This is the argument.  It will not be modified by
C                 CSQRT.
C
C *Function Return Values:
C
C    CSQRT   :    the square root of a complex argument.
C
C *Description:
C
C    CSQRT evaluates the square root of a complex argument.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CABS, SQRT
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  CSQRT
C     .. Scalar Arguments ..
      COMPLEX Z
C     .. Local Scalars ..
      REAL R, X, XTMP, Y, YTMP
C     .. External Functions ..
      REAL CABS, SQRT
      EXTERNAL SQRT
C      EXTERNAL CABS, SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS, AIMAG, CMPLX, REAL, SIGN
C***FIRST EXECUTABLE STATEMENT  CSQRT
      X = REAL(Z)
      Y = AIMAG(Z)
      R = CABS(Z)
C
      IF (R .EQ. 0.0E0) THEN
        CSQRT = (0.0E0, 0.0E0)
      ELSE
C
        XTMP = SQRT((R+ABS(X))*0.5E0)
        YTMP = Y*0.5E0/XTMP
C
        IF (X .GE. 0.0E0) THEN
          CSQRT = CMPLX(XTMP, YTMP)
        ELSEIF (Y .EQ. 0.0E0) THEN
          Y = 1.0E0
        ELSEIF (X .LT. 0.0E0) THEN
          CSQRT = CMPLX(ABS(YTMP), SIGN(XTMP, Y))
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK DACOS
      DOUBLE PRECISION FUNCTION DACOS (X)
C***BEGIN PROLOGUE  DACOS
C***PURPOSE  Compute the double precision arccosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (ACOS-S, DACOS-D)
C***KEYWORDS  DACOS, DOUBLE PRECISION ARCCOSINE, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DACOS, X, Y
C      Y = DACOS(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DACOS.
C
C *Function Return Values:
C
C    DACOS   :    the double precision arccosine of X.
C
C *Description:
C
C    DACOS evaluates the double precision arccosine of a double
C    precision argument.  The argument X must be in the interval -1.0
C    .LE. X .LE. +1.0.  For arguments outside the permitted interval,
C    an error message is generated by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DASIN, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DACOS
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION PI2
C     .. External Functions ..
      DOUBLE PRECISION DASIN
C      EXTERNAL DASIN
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC DABS
C     .. Data statements ..
      DATA PI2 / 1.5707963267 9489661923 1321691639 75 D0 /
C***FIRST EXECUTABLE STATEMENT  DACOS
      IF (DABS(X) .GT. 1.0D0) CALL XERMSG ('SLATEC', 'DACOS',
     +  'DABS(X) GREATER THAN ', 1, 2)
C
      DACOS = PI2 - DASIN(X)
C
      RETURN
      END
*DECK DASIN
      DOUBLE PRECISION FUNCTION DASIN (X)
C***BEGIN PROLOGUE  DASIN
C***PURPOSE  Compute the double precision arcsine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (ASIN-S, DASIN-D)
C***KEYWORDS  DASIN, DOUBLE PRECISION ARCSINE, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DASIN, X, Y
C      Y = DASIN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DASIN.
C
C *Function Return Values:
C
C    DASIN   :    the double precision arcsine of X.
C
C *Description:
C
C    DASIN evaluates the double precision arcsine of a double precision
C    argument.  The value for the argument X must be in the interval
C    -1.0 .LE. X .LE. +1.0.  For arguments outside the permitted
C    interval, an error message is generated by XERMSG and the
C    routine aborts.
C
C    SERIES FOR ASIN    ON THE INTERVAL  0.          TO  5.00000E-01
C                                        WITH WEIGHTED ERROR   1.62E-32
C                                         LOG WEIGHTED ERROR  31.79
C                               SIGNIFICANT FIGURES REQUIRED  30.67
C                                    DECIMAL PLACES REQUIRED  32.59
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DSQRT, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DASIN
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION PI2, SQEPS, Y, Z
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION ASINCS(39)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DSQRT
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DSQRT, INITDS
      EXTERNAL DSQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DSIGN, SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS
C     .. Data statements ..
      DATA ASINCS( 1) / +.1024639175 3227159336 5731483057 85 D+0 /
      DATA ASINCS( 2) / +.5494648722 1245833306 0111959029 24 D-1 /
      DATA ASINCS( 3) / +.4080630392 5449692851 3070561492 46 D-2 /
      DATA ASINCS( 4) / +.4078900685 4604435455 5988239056 12 D-3 /
      DATA ASINCS( 5) / +.4698536743 2203691616 0485301362 18 D-4 /
      DATA ASINCS( 6) / +.5880975813 9708058986 4543855520 74 D-5 /
      DATA ASINCS( 7) / +.7773231246 2777632750 5575281637 95 D-6 /
      DATA ASINCS( 8) / +.1067742334 0082039235 0475049565 87 D-6 /
      DATA ASINCS( 9) / +.1509239953 6022808262 3864344010 64 D-7 /
      DATA ASINCS(10) / +.2180972408 0055385496 6096147139 30 D-8 /
      DATA ASINCS(11) / +.3207598426 2789614433 2619596673 76 D-9 /
      DATA ASINCS(12) / +.4785536964 6781034461 4931339189 53 D-10 /
      DATA ASINCS(13) / +.7225128736 2910432263 8487545371 12 D-11 /
      DATA ASINCS(14) / +.1101833474 2255783705 3727013349 87 D-11 /
      DATA ASINCS(15) / +.1694763253 9203354877 4237456510 78 D-12 /
      DATA ASINCS(16) / +.2626155866 7348224162 2832415024 16 D-13 /
      DATA ASINCS(17) / +.4095829981 3281178408 8280692911 10 D-14 /
      DATA ASINCS(18) / +.6424479310 8803655891 7279448870 91 D-15 /
      DATA ASINCS(19) / +.1012814219 8228221693 9733612220 41 D-15 /
      DATA ASINCS(20) / +.1603922189 7380787560 0505974647 46 D-16 /
      DATA ASINCS(21) / +.2550350135 5807141715 2987896763 73 D-17 /
      DATA ASINCS(22) / +.4070140379 7862382855 4871656721 06 D-18 /
      DATA ASINCS(23) / +.6517267171 2881144437 8892675754 66 D-19 /
      DATA ASINCS(24) / +.1046745303 7096796954 2448917162 66 D-19 /
      DATA ASINCS(25) / +.1685872556 3380328094 9890951850 66 D-20 /
      DATA ASINCS(26) / +.2722193630 5040227625 1643412479 99 D-21 /
      DATA ASINCS(27) / +.4405929390 0347550617 1268300799 99 D-22 /
      DATA ASINCS(28) / +.7146668524 3375937853 0631680000 00 D-23 /
      DATA ASINCS(29) / +.1161579334 3859516051 7989717333 33 D-23 /
      DATA ASINCS(30) / +.1891523455 2354685801 1841877333 33 D-24 /
      DATA ASINCS(31) / +.3085577204 4244342399 8279680000 00 D-25 /
      DATA ASINCS(32) / +.5041636602 2162453412 9704959999 99 D-26 /
      DATA ASINCS(33) / +.8250272550 2400865081 7536000000 00 D-27 /
      DATA ASINCS(34) / +.1352003263 1020947208 0554666666 66 D-27 /
      DATA ASINCS(35) / +.2218432687 6541720216 6442666666 66 D-28 /
      DATA ASINCS(36) / +.3644249405 4085079212 5781333333 33 D-29 /
      DATA ASINCS(37) / +.5992021855 8643813307 7333333333 33 D-30 /
      DATA ASINCS(38) / +.9858481205 9573785810 2613333333 33 D-31 /
      DATA ASINCS(39) / +.1622250116 6399014393 1733333333 33 D-31 /
      DATA PI2 / 1.5707963267 9489661923 1321691639 75 D0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DASIN
      IF (FIRST) THEN
        NTERMS = INITDS(ASINCS, 39, 0.1*SNGL(D1MACH(3)))
        SQEPS = DSQRT(6.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
      Y = DABS(X)
      IF (Y .GT. 1.0D0) CALL XERMSG ('SLATEC', 'DASIN',
     +  'DABS(X) GREATER THAN 1', 1, 2)
C
      Z = 0.0D0
      IF (Y .GT. SQEPS) Z = Y*Y
      IF (Z .LE. 0.5D0) THEN
        DASIN = X*(1.0D0 + DCSEVL(4.0D0*Z-1.0D0, ASINCS, NTERMS))
      ELSE
        DASIN = PI2 - DSQRT(1.0D0-Z) * (1.0D0 +
     +    DCSEVL(3.0D0-4.0D0*Z, ASINCS, NTERMS))
      ENDIF
      IF (X .NE. 0.0D0) DASIN = DSIGN(DASIN, X)
C
      RETURN
      END
*DECK DATAN
      DOUBLE PRECISION FUNCTION DATAN (X)
C***BEGIN PROLOGUE  DATAN
C***PURPOSE  Compute the double precision arctangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (ATAN-S, DATAN-D)
C***KEYWORDS  DATAN, DOUBLE PRECISION ARCTANGENT, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DATAN, X, Y
C      Y = DATAN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DATAN.
C
C *Function Return Values:
C
C    DATAN   :    the double precision arctangent of X.
C
C *Description:
C
C    DATAN evaluates the double precision arctangent of a double
C    precision argument.
C
C    SERIES FOR ATAN    ON THE INTERVAL  0.          TO  4.00000E-02
C                                        WITH WEIGHTED ERROR   4.83E-32
C                                         LOG WEIGHTED ERROR  31.32
C                               SIGNIFICANT FIGURES REQUIRED  30.70
C                                    DECIMAL PLACES REQUIRED  31.92
C
C    XBNDN = TAN((2*N-1)*PI/16.0)
C
C    TANP8(N) = TAN(N*PI/8.0)
C
C    CONPI8(N) + PI8(N) = N*PI/8.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DSQRT, INITDS
C***REVISION HISTORY  (YYMMDD)
C   780101  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.
C***END PROLOGUE  DATAN
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION SQEPS, T, XBIG, XBND1, XBND2, XBND3, XBND4, Y
      INTEGER N, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION ATANCS(16), CONPI8(4), PI8(4), TANP8(3)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DSQRT
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DSQRT, INITDS
      EXTERNAL DSQRT	  
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DSIGN, SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XBIG
C     .. Data statements ..
      DATA ATANCS( 1) / +.4869011034 9241406474 6369159028 91 D+0 /
      DATA ATANCS( 2) / -.6510831636 7174641818 8697949459 74 D-2 /
      DATA ATANCS( 3) / +.3834582826 5245177653 5699924304 56 D-4 /
      DATA ATANCS( 4) / -.2687221287 6223146539 5954105187 88 D-6 /
      DATA ATANCS( 5) / +.2050093098 5824269846 6365146866 88 D-8 /
      DATA ATANCS( 6) / -.1645071739 5484269455 7341352853 48 D-10 /
      DATA ATANCS( 7) / +.1365097527 4390773423 8135284844 28 D-12 /
      DATA ATANCS( 8) / -.1160177959 1998246322 8913098346 66 D-14 /
      DATA ATANCS( 9) / +.1003833394 3866273835 7976574026 66 D-16 /
      DATA ATANCS(10) / -.8807274715 2163859327 0736960000 00 D-19 /
      DATA ATANCS(11) / +.7813632100 5661722180 5802666666 66 D-21 /
      DATA ATANCS(12) / -.6995453514 8267456086 6133333333 33 D-23 /
      DATA ATANCS(13) / +.6310590571 3702136004 2666666666 66 D-25 /
      DATA ATANCS(14) / -.5729607537 0213874346 6666666666 66 D-27 /
      DATA ATANCS(15) / +.5227479628 0602282666 6666666666 66 D-29 /
      DATA ATANCS(16) / -.4832790391 1608320000 0000000000 00 D-31 /
      DATA XBND1 / +.1989123673 7965800691 1597622644 67 D+0 /
      DATA XBND2 / +.6681786379 1929891999 7757686523 08 D+0 /
      DATA XBND3 / +1.496605762 6654890176 0113513494 24 D+0 /
      DATA XBND4 / +5.027339492 1258481045 1497507106 40 D+0 /
      DATA TANP8(1) / +.4142135623 7309504880 1688724209 69 D+0 /
      DATA TANP8(2) / +1.0D0 /
      DATA TANP8(3) / +2.414213562 3730950488 0168872420 96 D+0 /
      DATA CONPI8(1) / 0.375 D0 /
      DATA CONPI8(2) / 0.75  D0 /
      DATA CONPI8(3) / 1.125 D0 /
      DATA CONPI8(4) / 1.5   D0 /
      DATA PI8(1) / +.1769908169 8724154807 8304229099 37 D-1 /
      DATA PI8(2) / +.3539816339 7448309615 6608458198 75 D-1 /
      DATA PI8(3) / +.5309724509 6172464423 4912687298 13 D-1 /
      DATA PI8(4) / +.7079632679 4896619231 3216916397 51 D-1 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DATAN
      IF (FIRST) THEN
        NTERMS = INITDS(ATANCS, 16, 0.1*SNGL(D1MACH(3)))
        SQEPS = DSQRT(6.0D0*D1MACH(3))
        XBIG = 1.0D0/D1MACH(3)
      ENDIF
      FIRST = .FALSE.
C
      Y = DABS(X)
      IF (Y .LE. XBND1) THEN
C
        DATAN = X
        IF (Y .GT. SQEPS) DATAN = X*(0.75D0 + DCSEVL(50.0D0*Y*Y-1.0D0,
     +    ATANCS, NTERMS))
      ELSE
C
        IF (Y .LE. XBND4) THEN
C
          N = 1
          IF (Y .GT. XBND2) N = 2
          IF (Y .GT. XBND3) N = 3
C
          T = (Y - TANP8(N)) / (1.0D0 + Y*TANP8(N))
          DATAN = DSIGN(CONPI8(N) + (PI8(N) + T*(0.75D0 +
     +      DCSEVL(50.0D0*T*T-1.0D0, ATANCS, NTERMS)) ), X)
        ELSE
C
          DATAN = CONPI8(4) + PI8(4)
          IF (Y .LT. XBIG) DATAN = CONPI8(4) + (PI8(4) - (0.75D0 +
     +      DCSEVL(50.0D0/Y**2-1.0D0, ATANCS, NTERMS))/Y )
          DATAN = DSIGN(DATAN, X)
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK DATAN2
      DOUBLE PRECISION FUNCTION DATAN2 (SN, CS)
C***BEGIN PROLOGUE  DATAN2
C***PURPOSE  Compute the double precision arctangent in the
C            proper quadrant.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (ATAN2-S, DATAN2-D)
C***KEYWORDS  DATAN2, DOUBLE PRECISION ARCTANGENT, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DATAN2, SN, CS, Y
C      Y = DATAN2(SN, CS)
C
C *Arguments:
C
C    SN      :IN  This is an argument.  It will not be modified by
C                 DATAN2.
C    CS      :IN  This is an argument.  It will not be modified by
C                 DATAN2.
C
C *Function Return Values:
C
C    DATAN2  :    the double precision arctangent in the proper
C                 quadrant.
C
C *Description:
C
C    DATAN2 evaluates the double precision arctangent of two double
C    precison arguments in the proper quadrant.  The arguments SN and CS
C    must not both be zero.  If both of the two arguments are zero,
C    an error message is generated by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DATAN, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DATAN2
C     .. Scalar Arguments ..
      DOUBLE PRECISION CS, SN
C     .. Local Scalars ..
      DOUBLE PRECISION PI
C     .. External Functions ..
      DOUBLE PRECISION DATAN
C      EXTERNAL DATAN
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC DSIGN
C     .. Data statements ..
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
C***FIRST EXECUTABLE STATEMENT  DATAN2
      IF (CS .NE. 0.0D0) THEN
        DATAN2 = DATAN(SN/CS)
        IF (CS .LT. 0.0D0) DATAN2 = DATAN2 + PI
        IF (DATAN2 .GT. PI) DATAN2 = DATAN2 - 2.0D0*PI
      ELSE
        IF (SN .EQ. 0.0D0) CALL XERMSG ('SLATEC', 'DATAN2',
     +    'BOTH ARGUMENTS ARE ZERO', 1, 2)
C
        DATAN2 = DSIGN(0.5D0*PI, SN)
      ENDIF
C
      RETURN
      END
*DECK DCOS
      DOUBLE PRECISION FUNCTION DCOS (X)
C***BEGIN PROLOGUE  DCOS
C***PURPOSE  Compute the double precision cosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (COS-S, DCOS-D, CCOS-C)
C***KEYWORDS  DCOS, DOUBLE PRECISION COSINE, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DCOS, X, Y
C      Y = DCOS(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DCOS.
C
C *Function Return Values:
C
C    DCOS    :    the double precision cosine of X.
C
C *Description:
C
C    DCOS evaluates the double precision cosine of a double precision
C    argument.  If the argument X is greater than the reciprocal
C    of D1MACH(4), an error message is generated by XERMSG and
C    the routine aborts.
C
C
C    SERIES FOR SIN     ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   7.08E-32
C                                         LOG WEIGHTED ERROR  31.15
C                               SIGNIFICANT FIGURES REQUIRED  29.74
C                                    DECIMAL PLACES REQUIRED  31.69
C
C    SERIES FOR COS     ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   5.80E-34
C                                         LOG WEIGHTED ERROR  33.24
C                               SIGNIFICANT FIGURES REQUIRED  32.49
C                                    DECIMAL PLACES REQUIRED  33.79
C
C    PI4REC = 4.0/PI - 1.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DINT, DSQRT, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DCOS
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION AINTY, AINTY2, PI4REC, XMAX, XSML, XWARN, Y,
     +                 YREM, Z
      REAL ETA
      INTEGER IFN, NOCTNT, NTCS, NTSN
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION COSCS(13), SINCS(12)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DINT, DSQRT
      
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DINT, DSQRT, INITDS
      EXTERNAL DSQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DMOD, MOD
C     .. Save statement ..
      SAVE FIRST, NTCS, NTSN, XSML, XMAX, XWARN
C     .. Data statements ..
      DATA SINCS( 1) / -.8104079085 4487158001 9646994355 9 D-2 /
      DATA SINCS( 2) / -.3914456752 7081957017 4285389007 4 D-1 /
      DATA SINCS( 3) / +.3045094206 7894440558 1569559087 8 D-3 /
      DATA SINCS( 4) / -.1123574976 7964159582 1420362498 4 D-5 /
      DATA SINCS( 5) / +.2414039972 4137496071 0579107817 4 D-8 /
      DATA SINCS( 6) / -.3391636705 0375354740 0061371281 0 D-11 /
      DATA SINCS( 7) / +.3358087618 5142034459 0158836665 0 D-14 /
      DATA SINCS( 8) / -.2468983320 9931822571 0320561066 6 D-17 /
      DATA SINCS( 9) / +.1401147833 5107299361 7514666666 6 D-20 /
      DATA SINCS(10) / -.6322858930 2247096804 2666666666 6 D-24 /
      DATA SINCS(11) / +.2323065248 0719532160 0000000000 0 D-27 /
      DATA SINCS(12) / -.7083757638 6623999999 9999999999 9 D-31 /
      DATA COSCS( 1) / +.2032638274 0961602540 0812030121 852 D+0 /
      DATA COSCS( 2) / -.1464366443 9083686332 0796360139 993 D+0 /
      DATA COSCS( 3) / +.1921449311 8146467969 0714543745 079 D-2 /
      DATA COSCS( 4) / -.9964968489 8293000686 6910618423 658 D-5 /
      DATA COSCS( 5) / +.2757659560 7187395186 4383935301 797 D-7 /
      DATA COSCS( 6) / -.4739949808 1648440374 4229510321 398 D-10 /
      DATA COSCS( 7) / +.5549548541 4851827408 2726416274 480 D-13 /
      DATA COSCS( 8) / -.4709704906 5175559566 0385183093 333 D-16 /
      DATA COSCS( 9) / +.3029897608 0793731338 8574986666 666 D-19 /
      DATA COSCS(10) / -.1528414934 2146153361 4912000000 000 D-22 /
      DATA COSCS(11) / +.6207451543 5782725397 3333333333 333 D-26 /
      DATA COSCS(12) / -.2073330722 9836159999 9999999999 999 D-29 /
      DATA COSCS(13) / +.5795385333 6746666666 6666666666 666 D-33 /
      DATA PI4REC / +.2732395447 3516268615 1070106980 11 D0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DCOS
      IF (FIRST) THEN
        ETA = 0.1D0*D1MACH(3)
        NTSN = INITDS(SINCS, 12, ETA)
        NTCS = INITDS(COSCS, 13, ETA )
        XSML = DSQRT(2.0D0*D1MACH(3))
        XMAX = 1.0D0/D1MACH(4)
        XWARN = DSQRT(XMAX)
      ENDIF
      FIRST = .FALSE.
C
      Y = DABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DCOS',
     +  'NO PRECISION BECAUSE DABS(X) IS TOO BIG', 2, 2)
      IF (Y .GT. XWARN) CALL XERMSG ('SLATEC', 'DCOS',
     +  'ANSWER LT HALF PRECISION BECAUSE DABS(X) IS BIG', 1, 1)
C
      IF (Y .LT. XSML) THEN
        DCOS = 1.0D0
      ELSE
C
C       CAREFULLY COMPUTE Y * (4/PI)
C         = (AINT(Y) + REM(Y)) * (1. + PI4REC)
C         = AINT(Y) + REM(Y) + Y*PI4REC
C         = AINT(Y) + AINT(REM(Y)+Y*PI4REC) + REM(REM(Y)+Y*PI4REC)
C
        AINTY = DINT(Y)
        YREM = Y - AINTY
        Y = YREM + Y*PI4REC
        AINTY2 = DINT(Y)
        AINTY = AINTY + AINTY2
        Y = Y - AINTY2
C
        NOCTNT = DMOD(AINTY, 8.0D0)
        IFN = MOD((NOCTNT+1)/2, 2)
        IF (MOD(NOCTNT,2) .EQ. 1) Y = 1.0D0 - Y
        Z = 2.0D0*Y*Y - 1.0D0
C
        IF (IFN .NE. 1) THEN
          DCOS = 0.75D0 +DCSEVL(Z, COSCS, NTCS)
        ELSE
          DCOS = Y*(0.75D0 + DCSEVL(Z, SINCS, NTSN))
        ENDIF
        IF (NOCTNT .GT. 1 .AND. NOCTNT .LT. 6) DCOS = -DCOS
      ENDIF
C
      RETURN
      END
*DECK DCOSH
      DOUBLE PRECISION FUNCTION DCOSH (X)
C***BEGIN PROLOGUE  DCOSH
C***PURPOSE  Compute the double precision hyperbolic cosine.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      DOUBLE PRECISION (COSH-S, DCOSH-D)
C***KEYWORDS  DCOSH, DOUBLE PRECISION HYPERBOLIC COSINE,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DCOSH, X, Y
C      Y = DCOSH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DCOSH.
C
C *Function Return Values:
C
C    DCOSH   :    the double precision hyperbolic cosine of X.
C
C *Description:
C
C    DCOSH evaluates the double precision hyperbolic cosine of a
C    double precision argument.  If the magnitude of the argument
C    X is too big (i.e., greater than ln(D1MACH(2) - 0.01),
C    an error message is generated by XERMSG and the routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DEXP, DSQRT
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DCOSH
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION Y, YMAX
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DEXP, DSQRT
C      EXTERNAL D1MACH, DEXP, DSQRT
C     .. Intrinsic Functions ..
      INTRINSIC DABS
C     .. Save statement ..
      SAVE YMAX
C     .. Data statements ..
      DATA YMAX / 0.0D0 /
C***FIRST EXECUTABLE STATEMENT  DCOSH
      IF (YMAX .EQ. 0.0D0) YMAX = 1.0D0/DSQRT(D1MACH(3))
C
      Y = DEXP(DABS(X))
      DCOSH = 0.5D0*Y
      IF (Y .LT. YMAX) DCOSH = 0.5D0*(Y + 1.0D0/Y)
C
      RETURN
      END
*DECK DEXP
      DOUBLE PRECISION FUNCTION DEXP (X)
C***BEGIN PROLOGUE  DEXP
C***PURPOSE  Compute the double precision exponential.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (DEXP-D, CEXP-C)
C***KEYWORDS  DEXP, DOUBLE PRECISION EXPONENTIAL LOGARITHM,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DEXP, X, Y
C      Y = DEXP(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DEXP.
C
C *Function Return Values:
C
C    DEXP    :    the double precision exponential of X.
C
C *Description:
C
C    DEXP evaluates the double precision exponential of a double
C    precision argument, i.e. exp(Z) or e**Z.  If the argument X is
C    too big, (ln(D1MACH(2)) - 0.01), an error message is generated by
C    XERMSG and the routine aborts.
C
C    SERIES FOR EXP     ON THE INTERVAL -1.00000E+00 TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   2.30E-34
C                                         LOG WEIGHTED ERROR  33.64
C                               SIGNIFICANT FIGURES REQUIRED  32.28
C                                    DECIMAL PLACES REQUIRED  34.21
C
C    TWON16(I) IS 2.0**((I-1)/16) - 1.0
C
C    ALN216 IS 16.0/ALOG(2.) - 23.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9PAK, DCSEVL, DINT, DLOG, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DEXP
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION ALN216, F, XINT, XMAX, XMIN, Y
      INTEGER N, N16, NDX, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION EXPCS(14), TWON16(17)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, D9PAK, DCSEVL, DINT, DLOG
      INTEGER INITDS
C      EXTERNAL D1MACH, D9PAK, DCSEVL, DINT, DLOG, INITDS
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, XMIN, XMAX
C     .. Data statements ..
      DATA EXPCS( 1) / +.8665694933 1498571273 3404647266 231 D-1 /
      DATA EXPCS( 2) / +.9384948692 9983956189 6336579701 203 D-3 /
      DATA EXPCS( 3) / +.6776039709 9816826407 4353014653 601 D-5 /
      DATA EXPCS( 4) / +.3669312003 9380592780 1891250687 610 D-7 /
      DATA EXPCS( 5) / +.1589590536 1746184464 1928517821 508 D-9 /
      DATA EXPCS( 6) / +.5738598786 3020660125 2990815262 106 D-12 /
      DATA EXPCS( 7) / +.1775744485 9142151180 2306980226 000 D-14 /
      DATA EXPCS( 8) / +.4807991668 4237242267 5950244533 333 D-17 /
      DATA EXPCS( 9) / +.1157163768 8182857280 9260000000 000 D-19 /
      DATA EXPCS(10) / +.2506506102 5549771993 2458666666 666 D-22 /
      DATA EXPCS(11) / +.4935717081 4049582848 0000000000 000 D-25 /
      DATA EXPCS(12) / +.8909295727 4063424000 0000000000 000 D-28 /
      DATA EXPCS(13) / +.1484480629 0799786666 6666666666 666 D-30 /
      DATA EXPCS(14) / +.2296789166 3018666666 6666666666 666 D-33 /
      DATA TWON16( 1) / 0.0D0 /
      DATA TWON16( 2) / +.4427378242 7413840321 9664787399 29 D-1 /
      DATA TWON16( 3) / +.9050773266 5257659207 0106557607 07 D-1 /
      DATA TWON16( 4) / +.1387886347 5669165370 3830283841 51 D+0 /
      DATA TWON16( 5) / +.1892071150 0272106671 7499970560 47 D+0 /
      DATA TWON16( 6) / +.2418578120 7348404859 3677468726 59 D+0 /
      DATA TWON16( 7) / +.2968395546 5100966593 3754117792 45 D+0 /
      DATA TWON16( 8) / +.3542555469 3689272829 8014740140 70 D+0 /
      DATA TWON16( 9) / +.4142135623 7309504880 1688724209 69 D+0 /
      DATA TWON16(10) / +.4768261459 3949931138 6907480374 04 D+0 /
      DATA TWON16(11) / +.5422108254 0794082361 2291862090 73 D+0 /
      DATA TWON16(12) / +.6104903319 4925430817 9520667357 40 D+0 /
      DATA TWON16(13) / +.6817928305 0742908606 2250952466 42 D+0 /
      DATA TWON16(14) / +.7562521603 7329948311 2160619375 31 D+0 /
      DATA TWON16(15) / +.8340080864 0934246348 7083189588 28 D+0 /
      DATA TWON16(16) / +.9152065613 9714729387 2611270295 83 D+0 /
      DATA TWON16(17) / 1.0D0 /
      DATA ALN216 / +.8312065422 3414517758 7948960302 74 D-1 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DEXP
      IF (FIRST) THEN
        NTERMS = INITDS(EXPCS, 14, 0.1*SNGL(D1MACH(3)))
        XMIN = DLOG(D1MACH(1)) + 0.01D0
        XMAX = DLOG(D1MACH(2)) - 0.01D0
      ENDIF
      FIRST = .FALSE.
C
      IF (X .GE. XMIN) THEN
        IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DEXP',
     +    'X SO BIG DEXP OVERFLOWS', 2, 2)
C
        XINT = DINT(X)
        Y = X - XINT
C
        Y = 23.0D0*Y + X*ALN216
        N = Y
        F = Y - N
        N = 23.0D0*XINT + N
        N16 = N/16
        IF (N .LT. 0) N16 = N16 - 1
        NDX = N - 16*N16 + 1
C
        DEXP = 1.0D0 + (TWON16(NDX) + F*(1.0D0 + TWON16(NDX)) *
     +    DCSEVL(F, EXPCS, NTERMS) )
C
        DEXP = D9PAK(DEXP, N16)
      ELSE
C
        CALL XERMSG ('SLATEC', 'DEXP', 'X SO SMALL DEXP UNDERFLOWS',
     +    1, 1)
        DEXP = 0.0D0
      ENDIF
C
      RETURN
      END
*DECK DINT
      DOUBLE PRECISION FUNCTION DINT (X)
C***BEGIN PROLOGUE  DINT
C***PURPOSE  Find the largest integer whose magnitude does not
C            exceed the argument and convert to double precision.
C***LIBRARY   FNLIB
C***CATEGORY  C1
C***TYPE      DOUBLE PRECISION (DINT-D)
C***KEYWORDS  DINT, DOUBLE PRECISION INTEGER, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DINT, X, Y
C      Y = DINT(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DINT.
C
C *Function Return Values:
C
C    DINT    :    the truncation value of X.
C
C *Description:
C
C    DINT evaluates the integer value of a double precision argument.
C    The real and integer bases are compared.  If the bases are not
C    equal, an error message is generated by XERMSG and the routine
C    aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, I1MACH, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DINT
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION PART, SCALE, XBIG, XMAX, XSCL
      INTEGER I, IBASE, IPART, NDIGD, NDIGI, NPART
      LOGICAL FIRST
C     .. External Functions ..
      DOUBLE PRECISION D1MACH
      REAL R1MACH
      INTEGER I1MACH
C      EXTERNAL D1MACH, R1MACH, I1MACH
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC AMIN1, DABS, INT, MIN0, REAL, SNGL
C     .. Save statement ..
      SAVE FIRST, NPART, SCALE, XBIG, XMAX
C     .. Data statements ..
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT    DINT
      IF (FIRST) THEN
        IBASE = I1MACH(7)
        NDIGD = I1MACH(14)
        NDIGI = MIN0(I1MACH(8), I1MACH(11))-1
        XMAX = 1.0D0/D1MACH(4)
        XBIG = AMIN1(REAL(I1MACH(9)),1.0/R1MACH(4))
        IF (IBASE .NE. I1MACH(10)) CALL XERMSG ('SLATEC', 'DINT',
     +    'ALGORITHM ERROR.  INTEGER BASE NE REAL BASE', 2, 2)
        NPART = (NDIGD + NDIGI - 1)/NDIGI
        SCALE = IBASE**NDIGI
      ENDIF
      FIRST = .FALSE.
C
      IF (X .GE. -XBIG .AND. X .LE. XBIG) THEN
        DINT = INT(SNGL(X))
      ELSE
        XSCL = DABS(X)
        IF (XSCL .LE. XMAX) THEN
          DO 30 I = 1,NPART
            XSCL = XSCL/SCALE
   30     CONTINUE
C
          DO 40 I = 1,NPART
            XSCL = XSCL*SCALE
            IPART = XSCL
            PART = IPART
            XSCL = XSCL-PART
            DINT = DINT*SCALE+PART
   40     CONTINUE
C
          IF (X .LT. 0.0D0) DINT = -DINT
        ELSE
          CALL XERMSG ('SLATEC', 'DINT',
     +      'DABS(X) MAY BE TOO BIG TO BE REPRESENTED AS' //
     +      'AN EXACT INTEGER', 1, 1)
          DINT = X
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK DLOG
      DOUBLE PRECISION FUNCTION DLOG (X)
C***BEGIN PROLOGUE  DLOG
C***PURPOSE  Compute the double precision natural logarithm.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (ALOG-S, DLOG-D, CLOG-C)
C***KEYWORDS  DLOG, DOUBLE PRECISION NATURAL LOGARITHM,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DLOG, X, Y
C      Y = DLOG(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DLOG.
C
C *Function Return Values:
C
C    DLOG    :    the double precision natural logarithm of X.
C
C *Description:
C
C    DLOG evaluates the double precision natural logarithm of a double
C    precision argument.  The argument X must be positive.  For a
C    non-positive argument, an error message is generated by XERMSG
C    and the routine aborts.
C
C    SERIES FOR ALN     ON THE INTERVAL  0.          TO  3.46021E-03
C                                        WITH WEIGHTED ERROR   4.15E-32
C                                         LOG WEIGHTED ERROR  31.38
C                               SIGNIFICANT FIGURES REQUIRED  31.21
C                                    DECIMAL PLACES REQUIRED  31.90
C
C    ALN2 = ALOG(2.0) - 0.625
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9UPAK, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DLOG
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION ALN2, T, T2, XN, Y
      INTEGER N, NTERMS, NTRVAL
C     .. Local Arrays ..
      DOUBLE PRECISION ALNCEN(5), ALNCS(11), CENTER(4)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, INITDS
C     .. External Subroutines ..
C      EXTERNAL D9UPAK, XERMSG
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC SNGL
C     .. Save statement ..
      SAVE NTERMS
C     .. Data statements ..
      DATA ALNCS( 1) / +.1334719987 7973881561 6893860471 87 D+1 /
      DATA ALNCS( 2) / +.6937562832 8411286281 3724383542 25 D-3 /
      DATA ALNCS( 3) / +.4293403902 0450834506 5592108036 62 D-6 /
      DATA ALNCS( 4) / +.2893384779 5432594580 4664403875 87 D-9 /
      DATA ALNCS( 5) / +.2051251753 0340580901 7418134477 26 D-12 /
      DATA ALNCS( 6) / +.1503971705 5497386574 6151533199 99 D-15 /
      DATA ALNCS( 7) / +.1129454069 5636464284 5216133333 33 D-18 /
      DATA ALNCS( 8) / +.8635578867 1171868881 9466666666 66 D-22 /
      DATA ALNCS( 9) / +.6695299053 4350370613 3333333333 33 D-25 /
      DATA ALNCS(10) / +.5249155744 8151466666 6666666666 66 D-28 /
      DATA ALNCS(11) / +.4153054068 0362666666 6666666666 66 D-31 /
      DATA CENTER(1) / 1.0D0 /
      DATA CENTER(2) / 1.25D0 /
      DATA CENTER(3) / 1.50D0 /
      DATA CENTER(4) / 1.75D0 /
      DATA ALNCEN(1) / 0.0D0 /
      DATA ALNCEN(2) / +.2231435513 1420975576 6295090309 83 D+0 /
      DATA ALNCEN(3) / +.4054651081 0816438197 8013115464 34 D+0 /
      DATA ALNCEN(4) / +.5596157879 3542268627 0888500526 82 D+0 /
      DATA ALNCEN(5) / +.6931471805 5994530941 7232121458 17 D+0 /
      DATA ALN2 / 0.0681471805 5994530941 7232121458 18D0 /
      DATA NTERMS / 0 /
C***FIRST EXECUTABLE STATEMENT  DLOG
      IF (NTERMS .EQ. 0) NTERMS = INITDS(ALNCS, 11, 28.9*
     +  SNGL(D1MACH(3)))
C
      IF (X .LE. 0.0D0) CALL XERMSG('SLATEC', 'DLOG',
     +  'X IS ZERO OR NEGATIVE', 1, 2)
C
      CALL D9UPAK (X, Y, N)
C
      XN = N - 1
      Y = 2.0D0*Y
      NTRVAL = 4.0D0*Y - 2.5D0
C
      IF (NTRVAL .EQ. 5) T = ((Y-1.0D0)-1.0D0) / (Y+2.0D0)
      IF (NTRVAL .LT. 5) T = (Y-CENTER(NTRVAL)) / (Y+CENTER(NTRVAL))
      T2 = T*T
      DLOG = 0.625D0*XN + (ALN2*XN + ALNCEN(NTRVAL) + 2.0D0*T +
     +  T*T2*DCSEVL(578.D0*T2-1.0D0, ALNCS, NTERMS) )
C
      RETURN
      END
*DECK DLOG10
      DOUBLE PRECISION FUNCTION DLOG10 (X)
C***BEGIN PROLOGUE  DLOG10
C***PURPOSE  Compute the double precision common base 10 logarithm.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (ALOG10-S, DLOG10-D)
C***KEYWORDS  DLOG10, DOUBLE PRECISION BASE 10 LOGARITHM,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DLOG10, X, Y
C      Y = DLOG10(X)
C
C *Arguments:
C
C    X       :IN  This is an argument.  It will not be modified by
C                 DLOG10.
C
C *Function Return Values:
C
C    DLOG10  :    the double precision base 10, i.e. common, logarithm.
C
C *Description:
C
C    DLOG10 evaluates the double precision base 10 logarithm of a double
C    precision argument.  The argument X must be positive.  For a non-
C    positive argument, an error message is generated by XERMSG and the
C    routine aborts.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DLOG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DLOG10
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION DLOGE
C     .. External Functions ..
      DOUBLE PRECISION DLOG
C      EXTERNAL DLOG
C     .. Data statements ..
      DATA DLOGE / 0.4342944819 0325182765 1128918916 61 D0 /
C***FIRST EXECUTABLE STATEMENT  DLOG10
      DLOG10 = DLOGE * DLOG(X)
C
      RETURN
      END
*DECK DSIN
      DOUBLE PRECISION FUNCTION DSIN (X)
C***BEGIN PROLOGUE  DSIN
C***PURPOSE  Compute the double precision sine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (SIN-S, DSIN-D, CSIN-C)
C***KEYWORDS  DOUBLE PRECISION SINE, DSIN, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DSIN, X, Y
C      Y = DSIN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DSIN.
C
C *Function Return Values:
C
C    DSIN    :    the double precision sine of a double precision
C                 argument.
C
C *Description:
C
C    DSIN evaluates the double precision sine of a double precision
C    argument.  If the magnitude of the argument X is greater than the
C    reciprocal of D1MACH(4), an error message is generated by XERMSG
C    and the routine aborts.
C
C    SERIES FOR SIN     ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   7.08E-32
C                                         LOG WEIGHTED ERROR  31.15
C                               SIGNIFICANT FIGURES REQUIRED  29.74
C                                    DECIMAL PLACES REQUIRED  31.69
C
C    SERIES FOR COS     ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   5.80E-34
C                                         LOG WEIGHTED ERROR  33.24
C                               SIGNIFICANT FIGURES REQUIRED  32.49
C                                    DECIMAL PLACES REQUIRED  33.79
C
C    PI4REC = 4.0/PI - 1.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DINT, DSQRT, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C   901219  Logic in code corrected.  (WRB)
C***END PROLOGUE  DSIN
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION AINTY, AINTY2, PI4REC, XMAX, XSML, XWARN, Y,
     +                 YREM, Z
      REAL ETA
      INTEGER IFN, NOCTNT, NTCS, NTSN
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION COSCS(13), SINCS(12)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DINT, DSQRT     
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DINT, DSQRT, INITDS
      EXTERNAL DSQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, DMOD, MOD
C     .. Save statement ..
      SAVE FIRST, NTSN, NTCS, XMAX, XSML, XWARN
C     .. Data statements ..
      DATA SINCS( 1) / -.8104079085 4487158001 9646994355 9 D-2 /
      DATA SINCS( 2) / -.3914456752 7081957017 4285389007 4 D-1 /
      DATA SINCS( 3) / +.3045094206 7894440558 1569559087 8 D-3 /
      DATA SINCS( 4) / -.1123574976 7964159582 1420362498 4 D-5 /
      DATA SINCS( 5) / +.2414039972 4137496071 0579107817 4 D-8 /
      DATA SINCS( 6) / -.3391636705 0375354740 0061371281 0 D-11 /
      DATA SINCS( 7) / +.3358087618 5142034459 0158836665 0 D-14 /
      DATA SINCS( 8) / -.2468983320 9931822571 0320561066 6 D-17 /
      DATA SINCS( 9) / +.1401147833 5107299361 7514666666 6 D-20 /
      DATA SINCS(10) / -.6322858930 2247096804 2666666666 6 D-24 /
      DATA SINCS(11) / +.2323065248 0719532160 0000000000 0 D-27 /
      DATA SINCS(12) / -.7083757638 6623999999 9999999999 9 D-31 /
      DATA COSCS( 1) / +.2032638274 0961602540 0812030121 852 D+0 /
      DATA COSCS( 2) / -.1464366443 9083686332 0796360139 993 D+0 /
      DATA COSCS( 3) / +.1921449311 8146467969 0714543745 079 D-2 /
      DATA COSCS( 4) / -.9964968489 8293000686 6910618423 658 D-5 /
      DATA COSCS( 5) / +.2757659560 7187395186 4383935301 797 D-7 /
      DATA COSCS( 6) / -.4739949808 1648440374 4229510321 398 D-10 /
      DATA COSCS( 7) / +.5549548541 4851827408 2726416274 480 D-13 /
      DATA COSCS( 8) / -.4709704906 5175559566 0385183093 333 D-16 /
      DATA COSCS( 9) / +.3029897608 0793731338 8574986666 666 D-19 /
      DATA COSCS(10) / -.1528414934 2146153361 4912000000 000 D-22 /
      DATA COSCS(11) / +.6207451543 5782725397 3333333333 333 D-26 /
      DATA COSCS(12) / -.2073330722 9836159999 9999999999 999 D-29 /
      DATA COSCS(13) / +.5795385333 6746666666 6666666666 666 D-33 /
      DATA PI4REC / +.2732395447 3516268615 1070106980 11 D0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DSIN
      IF (FIRST) THEN
        ETA = 0.1D0*D1MACH(3)
        NTSN = INITDS(SINCS, 12, ETA)
        NTCS = INITDS(COSCS, 13, ETA)
        XSML = DSQRT(2.0D0*D1MACH(3))
        XMAX = 1.0D0/D1MACH(4)
        XWARN = DSQRT(XMAX)
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DSIN',
     +  'NO PRECISION BECAUSE DABS(X) IS TOO BIG', 2, 2)
      IF (Y .GT. XWARN) CALL XERMSG ('SLATEC', 'DSIN',
     +  'ANSWER LT HALF PRECISION BECAUSE DABS(X) IS BIG', 1, 1)
C
      IF (Y .LT. XSML) THEN
        DSIN = X
      ELSE
C
C       CAREFULLY COMPUTE Y * (4/PI)
C         = (AINT(Y) + REM(Y)) * (1. + PI4REC)
C         = AINT(Y) + REM(Y) + Y*PI4REC
C         = AINT(Y) + AINT(REM(Y)+Y*PI4REC) + REM(REM(Y)+Y*PI4REC)
C
        AINTY = DINT(Y)
        YREM = Y - AINTY
        Y = YREM + Y*PI4REC
        AINTY2 = DINT(Y)
        AINTY = AINTY + AINTY2
        Y = Y - AINTY2
C
        NOCTNT = DMOD(AINTY, 8.0D0)
        IFN = MOD((NOCTNT+1)/2, 2)
        IF (MOD(NOCTNT,2) .EQ. 1) Y = 1.0D0 - Y
        Z = 2.0D0*Y*Y - 1.0D0
C
        IF (IFN .NE. 1) THEN
          DSIN = Y*(0.75D0 + DCSEVL(Z, SINCS, NTSN))
        ELSE
          DSIN = 0.75D0 + DCSEVL(Z, COSCS, NTCS)
        ENDIF
        IF (NOCTNT .GT. 3) DSIN = -DSIN
        IF (X .LT. 0.0D0) DSIN = -DSIN
      ENDIF
C
      RETURN
      END
*DECK DSINH
      DOUBLE PRECISION FUNCTION DSINH (X)
C***BEGIN PROLOGUE  DSINH
C***PURPOSE  Compute the double precision hyperbolic sine.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      DOUBLE PRECISION (SINH-S, DSINH-D)
C***KEYWORDS  DOUBLE PRECISION HYPERBOLIC SINE, DSINH,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DSINH, X, Y
C      Y = DSINH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DSINH.
C
C *Function Return Values:
C
C    DSINH   :    the double precision hyperbolic sine of X.
C
C *Description:
C
C    DSINH evaluates the double precision hyperbolic sine of a double
C    precision argument.
C
C    SERIES FOR SINH    ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   7.76E-33
C                                         LOG WEIGHTED ERROR  32.11
C                               SIGNIFICANT FIGURES REQUIRED  31.20
C                                    DECIMAL PLACES REQUIRED  32.67
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DEXP, DSQRT, INITDS
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C   901219  Code corrected to compute DSINH correctly when ABS(X) .GE.
C           YMAX.  (WRB)
C***END PROLOGUE  DSINH
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION SQEPS, Y, YMAX
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION SINHCS(13)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DEXP, DSQRT
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DEXP, DSQRT, INITDS
      EXTERNAL DSQRT
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DSIGN, SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, YMAX
C     .. Data statements ..
      DATA SINHCS( 1) / +.1730421940 4717963167 5883846985 01 D+0 /
      DATA SINHCS( 2) / +.8759422192 2760477154 9002634544 40 D-1 /
      DATA SINHCS( 3) / +.1079477774 5671327502 4272706515 79 D-2 /
      DATA SINHCS( 4) / +.6374849260 7547504815 6855545718 50 D-5 /
      DATA SINHCS( 5) / +.2202366404 9230530159 1904960195 02 D-7 /
      DATA SINHCS( 6) / +.4987940180 4158493149 4258072036 61 D-10 /
      DATA SINHCS( 7) / +.7973053554 1157304814 4114804411 86 D-13 /
      DATA SINHCS( 8) / +.9473158713 0725443342 9273172266 66 D-16 /
      DATA SINHCS( 9) / +.8693492050 4480078871 0230986666 66 D-19 /
      DATA SINHCS(10) / +.6346939440 3318040457 3973333333 33 D-22 /
      DATA SINHCS(11) / +.3774033787 0858485738 6666666666 66 D-25 /
      DATA SINHCS(12) / +.1863021371 9570056533 3333333333 33 D-28 /
      DATA SINHCS(13) / +.7756843716 6506666666 6666666666 66 D-32 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DSINH
      IF (FIRST) THEN
        NTERMS = INITDS(SINHCS, 13, 0.1*SNGL(D1MACH(3)) )
        SQEPS = DSQRT(6.0D0*D1MACH(3))
        YMAX = 1.0D0/DSQRT(D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = DABS(X)
      IF (Y .LE. 1.0D0) THEN
        DSINH = X
        IF (Y .GT. SQEPS) DSINH = X*(1.0D0 + DCSEVL(2.0D0*X*X-1.0D0,
     +    SINHCS, NTERMS))
      ELSE
        Y = DEXP(Y)
        IF (Y .GE. YMAX) THEN
          DSINH = DSIGN(0.5D0*Y, X)
        ELSE
          DSINH = DSIGN(0.5D0*(Y-1.0D0/Y), X)
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK DSQRT
      DOUBLE PRECISION FUNCTION DSQRT (X)
C***BEGIN PROLOGUE  DSQRT
C***PURPOSE  Compute the double precision square root.
C***LIBRARY   FNLIB
C***CATEGORY  C2
C***TYPE      DOUBLE PRECISION (SQRT-S, DSQRT-D, CSQRT-C)
C***KEYWORDS  DOUBLE PRECISION SQUARE ROOT, DSQRT, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DSQRT, X, Y
C      Y = DSQRT(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DSQRT.
C
C *Function Return Values:
C
C    DSQRT   :    the double precision square root of X.
C
C *Description:
C
C    DSQRT evaluates the double precision square root of a double
C    precision argument.  The argument X must be positive.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALOG, D1MACH, D9PAK, D9UPAK, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DSQRT
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION Y
      REAL Z
      INTEGER IREM, ITER, IXPNT, N, NITER
C     .. Local Arrays ..
      DOUBLE PRECISION SQRT2(3)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, D9PAK
      REAL ALOG
C      EXTERNAL D1MACH, D9PAK, ALOG
C     .. External Subroutines ..
C      EXTERNAL D9UPAK, XERMSG
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC SNGL
C     .. Save statement ..
      SAVE NITER
C     .. Data statements ..
      DATA SQRT2(1) / 0.7071067811 8654752440 0844362104 85 D0 /
      DATA SQRT2(2) / 1.0D0 /
      DATA SQRT2(3) / 1.4142135623 7309504880 1688724209 70 D0 /
      DATA NITER / 0 /
C***FIRST EXECUTABLE STATEMENT  DSQRT
      IF (NITER .EQ. 0) NITER = 1.443*ALOG(-0.104*
     +  ALOG(0.1*SNGL(D1MACH(3)))) + 1.0
C
      IF (X .GT. 0.0D0) THEN
C
        CALL D9UPAK (X, Y, N)
        IXPNT = N/2
        IREM = N - 2*IXPNT + 2
C
C THE APPROXIMATION BELOW HAS ACCURACY OF 4.16 DIGITS.
C
        Z = Y
        DSQRT = 0.261599E0 + Z*(1.114292E0 + Z*(-0.516888E0 +
     +    Z*0.141067E0))
C
        DO 10 ITER=1,NITER
          DSQRT = DSQRT + 0.5D0*(Y - DSQRT*DSQRT) / DSQRT
   10   CONTINUE
C
        DSQRT = D9PAK(SQRT2(IREM)*DSQRT, IXPNT)
      ELSE
        DSQRT = 0.0D0
        IF (X .LT. 0.0D0) CALL XERMSG ('SLATEC', 'DSQRT',
     +    'X IS NEGATIVE', 1, 1)
      ENDIF
C
      RETURN
      END
*DECK DTAN
      DOUBLE PRECISION FUNCTION DTAN (X)
C***BEGIN PROLOGUE  DTAN
C***PURPOSE  Compute the double precision tangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (TAN-S, DTAN-D)
C***KEYWORDS  DOUBLE PRECISION TANGENT, DTAN, ELEMENTARY FUNCTION,
C             FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DTAN, X, Y
C      Y = DTAN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DTAN.
C
C *Function Return Values:
C
C    DTAN    :    the double precision tangent of X.
C
C *Description:
C
C    DTAN evaluates the double precision tangent of a double precision
C    argument.  If the magnitude of the argument X is greater than
C    the reciprocal of D1MACH(4), an error message is generated by
C    XERMSG and the routine aborts.
C
C    SERIES FOR TAN     ON THE INTERVAL  0.          TO  6.25000E-02
C                                        WITH WEIGHTED ERROR   1.44E-32
C                                         LOG WEIGHTED ERROR  31.84
C                               SIGNIFICANT FIGURES REQUIRED  30.92
C                                    DECIMAL PLACES REQUIRED  32.48
C
C    PI2REC = 2/PI - 0.625
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DINT, DSQRT, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DTAN
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION AINTY, AINTY2, PI2REC, PRODBG, SQEPS, XMAX, XSML,
     +                 Y, YREM
      INTEGER IFN, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION TANCS(19)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DINT, DSQRT
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DINT, DSQRT, INITDS
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DMOD, DSIGN, SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XMAX, XSML
C     .. Data statements ..
      DATA TANCS( 1) / +.2262793276 3129357846 5786365317 52 D+0 /
      DATA TANCS( 2) / +.4301791314 6548961775 5834107480 67 D-1 /
      DATA TANCS( 3) / +.6854461068 2565088756 9294736234 61 D-3 /
      DATA TANCS( 4) / +.1104532694 7597098383 5788493696 96 D-4 /
      DATA TANCS( 5) / +.1781747790 3926312943 2385125889 40 D-6 /
      DATA TANCS( 6) / +.2874496858 2365265947 5296468324 71 D-8 /
      DATA TANCS( 7) / +.4637485419 5902995494 1374782343 63 D-10 /
      DATA TANCS( 8) / +.7481760904 1556138502 3416333082 15 D-12 /
      DATA TANCS( 9) / +.1207049700 2957544801 6445169478 24 D-13 /
      DATA TANCS(10) / +.1947361081 2823019305 5138585845 33 D-15 /
      DATA TANCS(11) / +.3141722487 4732446504 6145860266 66 D-17 /
      DATA TANCS(12) / +.5068613255 5800153941 9048917333 33 D-19 /
      DATA TANCS(13) / +.8177310515 9836540043 9799466666 66 D-21 /
      DATA TANCS(14) / +.1319264341 2147384408 9514666666 66 D-22 /
      DATA TANCS(15) / +.2128399549 7042377309 8666666666 66 D-24 /
      DATA TANCS(16) / +.3433796019 2345945292 8000000000 00 D-26 /
      DATA TANCS(17) / +.5539822212 1173811200 0000000000 00 D-28 /
      DATA TANCS(18) / +.8937522779 4352810666 6666666666 66 D-30 /
      DATA TANCS(19) / +.1441911137 1369130666 6666666666 66 D-31 /
      DATA PI2REC / .01161977236 7581343075 5350534900 57 D0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DTAN
      IF (FIRST) THEN
        NTERMS = INITDS(TANCS, 19, 0.1*SNGL(D1MACH(3)))
        XMAX = 1.0D0/D1MACH(4)
        XSML = DSQRT(3.0D0*D1MACH(3))
        SQEPS = DSQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = DABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DTAN',
     +  'NO PRECISION BECAUSE DABS(X) IS TOO BIG', 2, 2)
C
C     CAREFULLY COMPUTE Y * (2/PI)
C       = (AINT(Y) + REM(Y)) * (.625 + PI2REC)
C       = AINT(.625*Y) + REM(.625*Y) + Y*PI2REC  =  AINT(.625*Y) + Z
C       = AINT(.625*Y) + AINT(Z) + REM(Z)
C
      AINTY = DINT(Y)
      YREM = Y - AINTY
      PRODBG = 0.625D0*AINTY
      AINTY = DINT(PRODBG)
      Y = (PRODBG-AINTY) + 0.625D0*YREM + PI2REC*Y
      AINTY2 = DINT(Y)
      AINTY = AINTY + AINTY2
      Y = Y - AINTY2
C
      IFN = DMOD(AINTY, 2.0D0)
      IF (IFN .EQ. 1) Y = 1.0D0 - Y
      IF (1.0D0-Y .LT. DABS(X)*SQEPS) CALL XERMSG ('SLATEC', 'DTAN',
     +  'ANSWER LT HALF PRECISION, DABS(X) BIG OR NEAR PI/2 OR 3*PI/2',
     +  1, 1)
      IF (Y .EQ. 1.0D0) CALL XERMSG ('SLATEC', 'DTAN',
     +  'X IS PI/2 OR 3*PI/2', 3, 2)
C
      IF (Y .LE. 0.25D0) THEN
        DTAN = X
        IF (Y .GT. XSML) DTAN = Y*(1.5D0 + DCSEVL(32.D0*Y*Y-1.0D0,
     +    TANCS, NTERMS))
      ELSE
C
        IF (Y .LE. 0.5D0) THEN
          DTAN = 0.5D0*Y*(1.5D0 + DCSEVL(8.0D0*Y*Y-1.0D0, TANCS,
     +    NTERMS))
          DTAN = 2.0*DTAN / (1.0D0-DTAN*DTAN)
        ELSE
C
          DTAN = 0.25D0*Y*(1.5D0 + DCSEVL(2.D0*Y*Y-1.0D0, TANCS,
     +      NTERMS))
          DTAN = 2.0D0*DTAN / (1.0D0-DTAN*DTAN)
          DTAN = 2.0D0*DTAN / (1.0D0-DTAN*DTAN)
C
        ENDIF
      ENDIF
      IF (X .NE. 0.0D0) DTAN = DSIGN(DTAN, X)
      IF (IFN .EQ. 1) DTAN = -DTAN
C
      RETURN
      END
*DECK DTANH
      DOUBLE PRECISION FUNCTION DTANH (X)
C***BEGIN PROLOGUE  DTANH
C***PURPOSE  Compute the double precision hyperbolic tangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      DOUBLE PRECISION (TANH-S, DTANH-D)
C***KEYWORDS  DOUBLE PRECISION HYPERBOLIC TANGENT, DTANH,
C             ELEMENTARY FUNCTION, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION DTANH, X, Y
C      Y = DTANH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 DTANH.
C
C *Function Return Values:
C
C    DTANH   :    the double precision hyperbolic tangent of X.
C
C
C *Description:
C
C    DTANH evaluates the double precision hyperbolic tangent of a
C    double precision argument.
C
C    SERIES FOR TANH    ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   9.92E-33
C                                         LOG WEIGHTED ERROR  32.00
C                               SIGNIFICANT FIGURES REQUIRED  31.25
C                                    DECIMAL PLACES REQUIRED  32.75
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, DEXP, DLOG, DSQRT, INITDS
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900524  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  DTANH
C     .. Scalar Arguments ..
      DOUBLE PRECISION X
C     .. Local Scalars ..
      DOUBLE PRECISION SQEPS, XMAX, Y
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      DOUBLE PRECISION TANHCS(31)
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DCSEVL, DEXP, DLOG, DSQRT
      INTEGER INITDS
C      EXTERNAL D1MACH, DCSEVL, DEXP, DLOG, DSQRT, INITDS
      EXTERNAL DSQRT
C     .. Intrinsic Functions ..
      INTRINSIC DABS, DSIGN, SNGL
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XMAX
C     .. Data statements ..
      DATA TANHCS( 1) / -.2582875664 3634710438 3381514506 05 D+0 /
      DATA TANHCS( 2) / -.1183610633 0053496535 3836719402 04 D+0 /
      DATA TANHCS( 3) / +.9869442648 0063988762 8273079996 81 D-2 /
      DATA TANHCS( 4) / -.8357986623 4458257836 1636903986 38 D-3 /
      DATA TANHCS( 5) / +.7090432119 8943582626 7780343634 13 D-4 /
      DATA TANHCS( 6) / -.6016424318 1207040390 7434790010 10 D-5 /
      DATA TANHCS( 7) / +.5105241908 0064402965 1362977234 11 D-6 /
      DATA TANHCS( 8) / -.4332072907 7584087216 5454673871 92 D-7 /
      DATA TANHCS( 9) / +.3675999055 3445306144 9300762337 14 D-8 /
      DATA TANHCS(10) / -.3119284961 2492011117 2156514809 53 D-9 /
      DATA TANHCS(11) / +.2646882819 9718962579 3777584453 81 D-10 /
      DATA TANHCS(12) / -.2246023930 7504140621 8709970061 96 D-11 /
      DATA TANHCS(13) / +.1905873376 8288196054 3194683961 39 D-12 /
      DATA TANHCS(14) / -.1617237144 6432292391 3307692797 01 D-13 /
      DATA TANHCS(15) / +.1372313614 2294289632 8977612893 86 D-14 /
      DATA TANHCS(16) / -.1164482687 0554194634 4396472937 81 D-15 /
      DATA TANHCS(17) / +.9881268497 1669738285 5405143381 33 D-17 /
      DATA TANHCS(18) / -.8384793367 7744865122 2692290559 99 D-18 /
      DATA TANHCS(19) / +.7114952886 9124351310 7235061760 00 D-19 /
      DATA TANHCS(20) / -.6037424222 9442045413 2888371199 99 D-20 /
      DATA TANHCS(21) / +.5123082587 7768084883 4046634666 66 D-21 /
      DATA TANHCS(22) / -.4347214015 7782110106 0478293333 33 D-22 /
      DATA TANHCS(23) / +.3688847363 9031328479 4231466666 66 D-23 /
      DATA TANHCS(24) / -.3130187477 4939399883 3254399999 99 D-24 /
      DATA TANHCS(25) / +.2656134200 6551994468 4885333333 33 D-25 /
      DATA TANHCS(26) / -.2253874230 4145029883 4943999999 99 D-26 /
      DATA TANHCS(27) / +.1912534782 7973995102 2080000000 00 D-27 /
      DATA TANHCS(28) / -.1622889709 6543663117 6533333333 33 D-28 /
      DATA TANHCS(29) / +.1377110122 9854738786 9866666666 66 D-29 /
      DATA TANHCS(30) / -.1168552784 0188950118 3999999999 99 D-30 /
      DATA TANHCS(31) / +.9915805538 4640389120 0000000000 00 D-32 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  DTANH
      IF (FIRST) THEN
        NTERMS = INITDS(TANHCS, 31, 0.1*SNGL(D1MACH(3)) )
        SQEPS = DSQRT(3.0D0*D1MACH(3))
        XMAX = -0.5D0*DLOG(D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = DABS(X)
      IF (Y .LE. 1.0D0) THEN
        DTANH = X
        IF (Y .GT. SQEPS) DTANH = X*(1.0D0 + DCSEVL(2.0D0*X*X-1.0D0,
     +    TANHCS, NTERMS) )
      ELSE
        IF (Y .LE. XMAX) THEN
          Y = DEXP(X)
          DTANH = DSIGN((Y-1.0D0/Y)/(Y+1.0D0/Y), X)
        ELSE
          DTANH = DSIGN(1.0D0, X)
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK EXP
      REAL FUNCTION EXP (X)
C***BEGIN PROLOGUE  EXP
C***PURPOSE  Compute the exponential.
C***LIBRARY   FNLIB
C***CATEGORY  C4B
C***TYPE      SINGLE PRECISION (EXP-S, DEXP-D, CEXP-C)
C***KEYWORDS  ELEMENTARY FUNCTION, EXP, FORTRAN INTRINSIC
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL EXP, X, Y
C      Y = EXP(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 EXP.
C
C *Function Return Values:
C
C    EXP     :    the exponential of X.
C
C *Description:
C
C    EXP evaluates the exponential of an argument, i.e. exp(Z) or
C    e**Z.  If the argument X is too big (ln(R1MACH(2)) - 0.01),
C    an error message is generated by XERMSG and the routine aborts.
C
C    SERIES FOR EXP     ON THE INTERVAL -1.00000D+00 TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   4.81E-18
C                                         LOG WEIGHTED ERROR  17.32
C                               SIGNIFICANT FIGURES REQUIRED  15.95
C                                    DECIMAL PLACES REQUIRED  17.77
C
C    ALN216 IS 16.0/ALOG(2.) - 23.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALOG, CSEVL, INITS, R1MACH, R9PAK, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  EXP
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL ALN216, F, XINT, XMAX, XMIN, Y
      INTEGER N, N16, NDX, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL EXPCS(8), TWON16(17)
C     .. External Functions ..
      REAL ALOG, CSEVL, R1MACH, R9PAK
      INTEGER INITS
C      EXTERNAL ALOG, CSEVL, R1MACH, R9PAK, INITS
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC AINT
C     .. Save statement ..
      SAVE FIRST, NTERMS, XMIN, XMAX
C     .. Data statements ..
      DATA EXPCS(1) / .0866569493 31498571E0 /
      DATA EXPCS(2) / .0009384948 69299839E0 /
      DATA EXPCS(3) / .0000067760 39709981E0 /
      DATA EXPCS(4) / .0000000366 93120039E0 /
      DATA EXPCS(5) / .0000000001 58959053E0 /
      DATA EXPCS(6) / .0000000000 00573859E0 /
      DATA EXPCS(7) / .0000000000 00001775E0 /
      DATA EXPCS(8) / .0000000000 00000004E0 /
      DATA TWON16( 1) / 0.0E0 /
      DATA TWON16( 2) / .44273782427413840E-1 /
      DATA TWON16( 3) / .90507732665257659E-1 /
      DATA TWON16( 4) / .13878863475669165E0 /
      DATA TWON16( 5) / .18920711500272107E0 /
      DATA TWON16( 6) / .24185781207348405E0 /
      DATA TWON16( 7) / .29683955465100967E0 /
      DATA TWON16( 8) / .35425554693689273E0 /
      DATA TWON16( 9) / .41421356237309505E0 /
      DATA TWON16(10) / .47682614593949931E0 /
      DATA TWON16(11) / .54221082540794082E0 /
      DATA TWON16(12) / .61049033194925431E0 /
      DATA TWON16(13) / .68179283050742909E0 /
      DATA TWON16(14) / .75625216037329948E0 /
      DATA TWON16(15) / .83400808640934246E0 /
      DATA TWON16(16) / .91520656139714729E0 /
      DATA TWON16(17) / 1.0E0 /
      DATA ALN216 / .083120654223414518E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  EXP
      IF (FIRST) THEN
        NTERMS = INITS(EXPCS, 8, 0.1*R1MACH(3))
        XMIN = ALOG(R1MACH(1)) + 0.01E0
        XMAX = ALOG(R1MACH(2)) - 0.01E0
      ENDIF
      FIRST = .FALSE.
C
      IF (X .GE. XMIN) THEN
        IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'EXP',
     +    'X SO BIG EXP OVERFLOWS', 2, 2)
C
        XINT = AINT(X)
        Y = X - XINT
C
        Y = 23.0*Y + X*ALN216
        N = Y
        F = Y - N
        N = 23.0*XINT + N
        N16 = N/16
        IF (N .LT. 0) N16 = N16 - 1
        NDX = N - 16*N16 + 1
C
        EXP = 1.0 + (TWON16(NDX) + F*(1.0+TWON16(NDX)) *
     +    CSEVL(F, EXPCS, NTERMS))
C
        EXP = R9PAK(EXP, N16)
      ELSE
C
        CALL XERMSG ('SLATEC', 'EXP', 'X SO SMALL EXP UNDERFLOWS', 1, 1)
        EXP = 0.0
      ENDIF
C
      RETURN
      END
*DECK SIN
      REAL FUNCTION SIN (X)
C***BEGIN PROLOGUE  SIN
C***PURPOSE  Compute the sine.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (SIN-S, DSIN-D, CSIN-C)
C***KEYWORDS  ELEMENTARY FUNCTION, FORTRAN INTRINSIC, SIN, SINE
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL SIN, X, Y
C      Y = SIN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 SIN.
C
C *Function Return Values:
C
C    SIN     :    the sine of a X.
C
C *Description:
C
C    SIN evaluates the sine of an argument.  If the argument X is
C    greater than the reciprocal of R1MACH(4), an error message is
C    generated by XERMSG and the routine aborts.
C
C    SERIES FOR SIN     ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   2.47E-18
C                                         LOG WEIGHTED ERROR  17.61
C                               SIGNIFICANT FIGURES REQUIRED  16.20
C                                    DECIMAL PLACES REQUIRED  18.06
C
C    SERIES FOR COS     ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   4.71E-17
C                                         LOG WEIGHTED ERROR  16.33
C                               SIGNIFICANT FIGURES REQUIRED  15.58
C                                    DECIMAL PLACES REQUIRED  16.78
C
C    PI4REC = 4.0/PI - 1.0
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, SQRT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  SIN
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL AINTY, AINTY2, PI4REC, XMAX, XSML, XWARN, Y, YREM, Z
      INTEGER IFN, NOCTNT, NTCS, NTSN
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL COSCS(8), SINCS(8)
C     .. External Functions ..
      REAL CSEVL, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, AINT, AMOD, MOD
C     .. Save statement ..
      SAVE FIRST, NTSN, NTCS, XMAX, XSML, XWARN
C     .. Data statements ..
      DATA SINCS(1) / -.0081040790 85448715E0 /
      DATA SINCS(2) / -.0391445675 27081957E0 /
      DATA SINCS(3) /  .0003045094 20678944E0 /
      DATA SINCS(4) / -.0000011235 74976796E0 /
      DATA SINCS(5) /  .0000000024 14039972E0 /
      DATA SINCS(6) / -.0000000000 03391636E0 /
      DATA SINCS(7) /  .0000000000 00003358E0 /
      DATA SINCS(8) / -.0000000000 00000002E0 /
      DATA COSCS(1) /  .2032638274 0961603E0 /
      DATA COSCS(2) / -.1464366443 9083686E0 /
      DATA COSCS(3) /  .0019214493 11814646E0 /
      DATA COSCS(4) / -.0000099649 68489829E0 /
      DATA COSCS(5) /  .0000000275 76595607E0 /
      DATA COSCS(6) / -.0000000000 47399498E0 /
      DATA COSCS(7) /  .0000000000 00055495E0 /
      DATA COSCS(8) / -.0000000000 00000047E0 /
      DATA PI4REC / +.2732395447 35162686 E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  SIN
      IF (FIRST) THEN
        NTSN = INITS(SINCS, 8, 0.1*R1MACH(3))
        NTCS = INITS(COSCS, 8, 0.1*R1MACH(3))
        XSML = SQRT(6.0E0*R1MACH(3))
        XMAX = 1.0E0/R1MACH(4)
        XWARN = SQRT(XMAX)
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'SIN',
     +  'NO PRECISION BECAUSE ABS(X) IS TOO BIG', 2, 2)
      IF (Y .GT. XWARN) CALL XERMSG ('SLATEC', 'SIN',
     +  'ANSWER LT HALF PRECISION BECAUSE ABS(X) IS BIG', 1, 1)
C
      IF (Y .LT. XSML) THEN
        SIN = X
      ELSE
C
C       CAREFULLY COMPUTE Y * (4/PI)
C         = (AINT(Y) + REM(Y)) * (1. + PI4REC)
C         = AINT(Y) + REM(Y) + Y*PI4REC
C         = AINT(Y) + AINT(REM(Y)+Y*PI4REC) + REM(REM(Y)+Y*PI4REC)
C
        AINTY = AINT(Y)
        YREM = Y - AINTY
        Y = YREM + Y*PI4REC
        AINTY2 = AINT(Y)
        AINTY = AINTY + AINTY2
        Y = Y - AINTY2
C
        NOCTNT = AMOD (AINTY, 8.0E0)
        IFN = MOD ((NOCTNT+1)/2, 2)
        IF (MOD(NOCTNT,2) .EQ. 1) Y = 1.0E0 - Y
        Z = 2.0E0*Y**2 - 1.0E0
C
        IF (IFN .NE. 1) THEN
          SIN = Y*(0.75E0 + CSEVL(Z, SINCS, NTSN))
        ELSE
          SIN = 0.75E0 + CSEVL (Z, COSCS, NTCS)
        ENDIF
        IF (NOCTNT .GT. 3) SIN = -SIN
        IF (X .LT. 0.0E0) SIN = -SIN
      ENDIF
C
      RETURN
      END
*DECK SINH
      REAL FUNCTION SINH (X)
C***BEGIN PROLOGUE  SINH
C***PURPOSE  Compute the hyperbolic sine.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      SINGLE PRECISION (SINH-S, DSINH-D)
C***KEYWORDS  ELEMENTARY FUNCTION, FORTRAN INTRINSIC, HYPERBOLIC SINE,
C             SINH
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL SINH, X, Y
C      Y = SINH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 SINH.
C
C *Function Return Values:
C
C    SINH    :    the hyperbolic sine of a complex argument.
C
C *Description:
C
C    SINH evaluates the hyperbolic sine of an argument.
C
C    SERIES FOR SINH    ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   9.47E-17
C                                         LOG WEIGHTED ERROR  16.02
C                               SIGNIFICANT FIGURES REQUIRED  15.11
C                                    DECIMAL PLACES REQUIRED  16.48
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, EXP, INITS, R1MACH, SQRT
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  SINH
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL SQEPS, Y, YMAX
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL SINHCS(8)
C     .. External Functions ..
      REAL CSEVL, EXP, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL CSEVL, EXP, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS, SIGN
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, YMAX
C     .. Data statements ..
      DATA SINHCS(1) / .1730421940 471796E0 /
      DATA SINHCS(2) / .0875942219 2276048E0 /
      DATA SINHCS(3) / .0010794777 7456713E0 /
      DATA SINHCS(4) / .0000063748 4926075E0 /
      DATA SINHCS(5) / .0000000220 2366404E0 /
      DATA SINHCS(6) / .0000000000 4987940E0 /
      DATA SINHCS(7) / .0000000000 0007973E0 /
      DATA SINHCS(8) / .0000000000 0000009E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  SINH
      IF (FIRST) THEN
        NTERMS = INITS(SINHCS, 8, 0.1*R1MACH(3))
        SQEPS = SQRT(6.0E0*R1MACH(3))
        YMAX = 1.0E0/SQRT(R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .LE. 1.0E0) THEN
        SINH = X
        IF (Y .GT. SQEPS) SINH = X*(1.0E0 + CSEVL(2.0E0*X*X-1.0E0,
     +    SINHCS, NTERMS))
      ELSE
        Y = EXP(Y)
        IF (Y .GE. YMAX) THEN
          SINH = SIGN(0.5E0*Y,X)
        ELSE
         SINH = SIGN(0.5E0*(Y-1.0E0/Y), X)
        ENDIF
      ENDIF
C
      RETURN
      END
*DECK SQRT
      REAL FUNCTION SQRT (X)
C***BEGIN PROLOGUE  SQRT
C***PURPOSE  Compute the square root.
C***LIBRARY   FNLIB
C***CATEGORY  C2
C***TYPE      SINGLE PRECISION (SQRT-S, DSQRT-D, CSQRT-C)
C***KEYWORDS  ELEMENTARY FUNCTION, FORTRAN INTRINSIC, SQRT, SQUARE ROOT
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL SQRT, X, Y
C      Y = SQRT(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 SQRT.
C
C *Function Return Values:
C
C    SQRT    :    the square root of X.
C
C *Description:
C
C    SQRT evaluates the square root of an argument.  The argument X
C    must be positive.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALOG, R1MACH, R9PAK, R9UPAK, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  SQRT
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL Y
      INTEGER IREM, ITER, IXPNT, N, NITER
C     .. Local Arrays ..
      REAL SQRT2(3)
C     .. External Functions ..
      REAL ALOG, R1MACH, R9PAK
C      EXTERNAL ALOG, R1MACH, R9PAK
C     .. External Subroutines ..
C      EXTERNAL R9UPAK, XERMSG
      EXTERNAL XERMSG
C     .. Save statement ..
      SAVE NITER
C     .. Data statements ..
      DATA SQRT2(1) / 0.7071067811 8654752E0 /
      DATA SQRT2(2) / 1.0E0 /
      DATA SQRT2(3) / 1.4142135623 7309505 E0 /
      DATA NITER / 0 /
C***FIRST EXECUTABLE STATEMENT  SQRT
      IF (NITER .EQ. 0) NITER = 1.443*ALOG(-0.104*ALOG(0.1*
     +  R1MACH(3)))+1.0
C
      IF (X .GT. 0.0E0) THEN
C
        CALL R9UPAK (X, Y, N)
        IXPNT = N/2
        IREM = N - 2*IXPNT + 2
C
C THE APPROXIMATION BELOW HAS ACCURACY OF 4.16 DIGITS.
C
        SQRT = .261599E0 + Y*(1.114292E0 + Y*(-.516888E0 + Y*.141067E0))
C
        DO 10 ITER=1,NITER
          SQRT = SQRT + 0.5E0*(Y - SQRT**2) / SQRT
   10   CONTINUE
C
        SQRT = R9PAK(SQRT2(IREM)*SQRT, IXPNT)
C
      ELSE
        SQRT = 0.0E0
        IF (X .LT. 0.0E0) CALL XERMSG ('SLATEC', 'SQRT',
     +    'X IS NEGATIVE', 1, 1)
      ENDIF
C
      RETURN
      END
*DECK TAN
      REAL FUNCTION TAN (X)
C***BEGIN PROLOGUE  TAN
C***PURPOSE  Compute the tangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4A
C***TYPE      SINGLE PRECISION (TAN-S, DTAN-D)
C***KEYWORDS  ELEMENTARY FUNCTION, FORTRAN INTRINSIC, TAN, TANGENT
C***AUTHOR  FULLERTON, W., C-3, LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      DOUBLE PRECISION TAN, X, Y
C      Y = TAN(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 TAN.
C
C *Function Return Values:
C
C    TAN     :    the tangent of X.
C
C *Description:
C
C    TAN evaluates the tangent of an argument.  If the magnitude of the
C    argument X is greater than the reciprocal of D1MACH(4), an error
C    message is generated and the routine aborts.
C
C    SERIES FOR TAN     ON THE INTERVAL  0.          TO  6.25000D-02
C                                        WITH WEIGHTED ERROR   3.14E-18
C                                         LOG WEIGHTED ERROR  17.50
C                               SIGNIFICANT FIGURES REQUIRED  16.59
C                                    DECIMAL PLACES REQUIRED  18.02
C
C    PI2REC = 2/PI - 0.625
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, SQRT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  TAN
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL AINTY, AINTY2, PI2REC, PRODBG, SQEPS, XMAX, XSML, Y, YREM
      INTEGER IFN, NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL TANCS(11)
C     .. External Functions ..
      REAL CSEVL, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL CSEVL, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, AINT, AMOD, SIGN
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XMAX, XSML
C     .. Data statements ..
      DATA TANCS( 1) / .2262793276 31293578E0 /
      DATA TANCS( 2) / .0430179131 465489618E0 /
      DATA TANCS( 3) / .0006854461 068256508E0 /
      DATA TANCS( 4) / .0000110453 269475970E0 /
      DATA TANCS( 5) / .0000001781 747790392E0 /
      DATA TANCS( 6) / .0000000028 744968582E0 /
      DATA TANCS( 7) / .0000000000 463748541E0 /
      DATA TANCS( 8) / .0000000000 007481760E0 /
      DATA TANCS( 9) / .0000000000 000120704E0 /
      DATA TANCS(10) / .0000000000 000001947E0 /
      DATA TANCS(11) / .0000000000 000000031E0 /
      DATA PI2REC / .01161977236 75813430 E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  TAN
      IF (FIRST) THEN
        NTERMS = INITS(TANCS, 11, 0.1*R1MACH(3))
        XMAX = 1.0E0/R1MACH(4)
        XSML = SQRT(3.0E0*R1MACH(3))
        SQEPS = SQRT(R1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'TAN',
     +  'NO PRECISION BECAUSE ABS(X) IS TOO BIG', 2, 2)
C
C     CAREFULLY COMPUTE Y * (2/PI)
C       = (AINT(Y) + REM(Y)) * (.625 + PI2REC)
C       = AINT(.625*Y) + REM(.625*Y) + Y*PI2REC  =  AINT(.625*Y) + Z
C       = AINT(.625*Y) + AINT(Z) + REM(Z)
C
      AINTY = AINT(Y)
      YREM = Y - AINTY
      PRODBG = 0.625E0*AINTY
      AINTY = AINT(PRODBG)
      Y = (PRODBG-AINTY) + 0.625E0*YREM + Y*PI2REC
      AINTY2 = AINT(Y)
      AINTY = AINTY + AINTY2
      Y = Y - AINTY2
C
      IFN = AMOD(AINTY, 2.0E0)
      IF (IFN .EQ. 1) Y = 1.0E0 - Y
C
      IF (1.0E0-Y .LT. ABS(X)*SQEPS) CALL XERMSG ('SLATEC', 'TAN',
     +  'ANSWER LT HALF PRECISION, ABS(X) BIG OR NEAR PI/2 OR 3*PI/2',
     +  1, 1)
      IF (Y .EQ. 1.0E0) CALL XERMSG ('SLATEC', 'TAN',
     +  'X IS PI/2 OR 3*PI/2', 3, 2)
C
      IF (Y .LE. 0.25E0) THEN
        TAN = X
        IF (Y .GT. XSML) TAN = Y*(1.5E0 + CSEVL(32.0E0*Y**2-1.0E0,
     +    TANCS, NTERMS))
      ELSE
C
        IF (Y .LE. 0.5E0) THEN
          TAN = 0.5E0*Y*(1.5E0 + CSEVL(8.0E0*Y**2-1.0E0, TANCS, NTERMS))
          TAN = 2.0E0*TAN/(1.0E0-TAN**2)
        ELSE
C
          TAN = 0.25E0*Y*(1.5E0 + CSEVL(2.0E0*Y**2-1.0E0, TANCS,
     +      NTERMS))
          TAN = 2.0E0*TAN/(1.0E0-TAN**2)
          TAN = 2.0E0*TAN/(1.0E0-TAN**2)
C
        ENDIF
      ENDIF
      IF (X .NE. 0.0E0) TAN = SIGN(TAN, X)
      IF (IFN .EQ. 1) TAN = -TAN
C
      RETURN
      END
*DECK TANH
      REAL FUNCTION TANH (X)
C***BEGIN PROLOGUE  TANH
C***PURPOSE  Compute the hyperbolic tangent.
C***LIBRARY   FNLIB
C***CATEGORY  C4C
C***TYPE      SINGLE PRECISION (TANH-S, DTANH-D)
C***KEYWORDS  ELEMENTARY FUNCTION, FORTRAN INTRINSIC,
C             HYPERBOLIC TANGENT, TANH
C***AUTHOR  FULLERTON, W., C-3. LOS ALAMOS SCIENTIFIC LABORATORY
C***DESCRIPTION
C
C *Usage:
C
C      REAL TANH, X, Y
C      Y = TANH(X)
C
C *Arguments:
C
C    X       :IN  This is the argument.  It will not be modified by
C                 TANH.
C
C *Function Return Values:
C
C    TANH    :    the hyperbolic tangent of a complex argument.
C
C *Description:
C
C    TANH evaluates the hyperbolic tangent of an argument.
C
C    SERIES FOR TANH    ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   9.88E-18
C                                         LOG WEIGHTED ERROR  17.01
C                               SIGNIFICANT FIGURES REQUIRED  16.25
C                                    DECIMAL PLACES REQUIRED  17.62
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALOG, CSEVL, EXP, INITS, R1MACH, SQRT
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   801001  Revisions by W. Fullerton.
C   900525  Prologue and code revised to conform to 1990 SLATEC
C           standards.  (SMR)
C***END PROLOGUE  TANH
C     .. Scalar Arguments ..
      REAL X
C     .. Local Scalars ..
      REAL SQEPS, XMAX, Y
      INTEGER NTERMS
      LOGICAL FIRST
C     .. Local Arrays ..
      REAL TANHCS(17)
C     .. External Functions ..
      REAL ALOG, CSEVL, EXP, R1MACH, SQRT
      INTEGER INITS
C      EXTERNAL ALOG, CSEVL, EXP, R1MACH, SQRT, INITS
      EXTERNAL SQRT
C     .. Intrinsic Functions ..
      INTRINSIC ABS, SIGN
C     .. Save statement ..
      SAVE FIRST, NTERMS, SQEPS, XMAX
C     .. Data statements ..
      DATA TANHCS( 1) / -.2582875664 3634710E0 /
      DATA TANHCS( 2) / -.1183610633 0053497E0 /
      DATA TANHCS( 3) /  .0098694426 48006398E0 /
      DATA TANHCS( 4) / -.0008357986 62344582E0 /
      DATA TANHCS( 5) /  .0000709043 21198943E0 /
      DATA TANHCS( 6) / -.0000060164 24318120E0 /
      DATA TANHCS( 7) /  .0000005105 24190800E0 /
      DATA TANHCS( 8) / -.0000000433 20729077E0 /
      DATA TANHCS( 9) /  .0000000036 75999055E0 /
      DATA TANHCS(10) / -.0000000003 11928496E0 /
      DATA TANHCS(11) /  .0000000000 26468828E0 /
      DATA TANHCS(12) / -.0000000000 02246023E0 /
      DATA TANHCS(13) /  .0000000000 00190587E0 /
      DATA TANHCS(14) / -.0000000000 00016172E0 /
      DATA TANHCS(15) /  .0000000000 00001372E0 /
      DATA TANHCS(16) / -.0000000000 00000116E0 /
      DATA TANHCS(17) /  .0000000000 00000009E0 /
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  TANH
      IF (FIRST) THEN
        NTERMS = INITS(TANHCS, 17, 0.1*R1MACH(3))
        SQEPS = SQRT(3.0E0*R1MACH(3))
        XMAX = -0.5E0*ALOG(R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .LE. 1.0E0) THEN
        TANH = X
        IF (Y .GT. SQEPS) TANH = X*(1.0E0 + CSEVL(2.0E0*X*X-1.0E0,
     +    TANHCS, NTERMS))
      ELSE
        IF (Y .LE. XMAX) THEN
          Y = EXP(Y)
          TANH = SIGN((Y-1.0E0/Y)/(Y+1.0E0/Y), X)
        ELSE
          TANH = SIGN(1.0E0, X)
        ENDIF
      ENDIF
C
      RETURN
      END


