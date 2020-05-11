C------------------------------------------------------------------
C FORTRAN 77 program to test EI, EONE, and EXPEI.
C
C  Method:
C
C     Accuracy test compare function values against local Taylor's
C     series expansions.  Derivatives for Ei(x) are generated from
C     the recurrence relation using a technique due to Gautschi
C     (see references).  Special argument tests are run with the
C     related functions E1(x) and exp(-x)Ei(x).
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - An environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C              IBETA  - The radix of the floating-point system
C              IT     - The number of base-ibeta digits in the
C                       significant of a floating-point number
C              XMAX   - The largest finite floating-point number
C
C     REN(K) - A function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C      ABS, AINT, DBLE, LOG, MAX, REAL, SQRT
C
C  References: "The use of Taylor series to test accuracy of
C               function programs", Cody, W. J., and Stoltz, L.,
C               submitted for publication.
C
C              "Recursive computation of certain derivatives -
C               A study of error propagation", Gautschi, W., and
C               Klein, B. J., Comm. ACM 13 (1970), 7-9.
C
C              "Remark on Algorithm 282", Gautschi, W., and Klein,
C               B. J., Comm. ACM 13 (1970), 53-54.
C
C  Latest modification: May 30, 1989
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NEGEP,NGRD,N1
CS    REAL
CD    DOUBLE PRECISION
     1    A,AIT,ALBETA,B,BETA,CONV,C1,D,DEL,DX,EN,EI,EONE,EPS,EPSNEG,
     2    EXPEI,FIV12,FOUR,FOURTH,ONE,P0625,REM,REN,R6,R7,SIX,SUM,
     3    TEN,TWO,U,V,W,X,XBIG,XC,XDEN,XL,XLGE,XMAX,XMIN,XN,XNP1,
     4    XNUM,X0,X99,Y,Z,ZERO
      DIMENSION D(0:25)
C------------------------------------------------------------------
CS    DATA ZERO,FOURTH,ONE,FOUR,SIX/0.0E0,0.25E0,1.0E0,4.0E0,6.0E0/,
CS   1     TEN,X0,X99,P0625/10.0E0,0.3725E0,-999.0E0,0.0625E0/,
CS   2     FIV12,REM/512.0E0,-7.424779065800051695596E-5/
CD    DATA ZERO,FOURTH,ONE,FOUR,SIX/0.0D0,0.25D0,1.0D0,4.0D0,6.0D0/,
CD   1     TEN,X0,X99,P0625/10.0D0,0.3725D0,-999.0D0,0.0625D0/,
CD   2     FIV12,REM/512.0D0,-7.424779065800051695596D-5/
      DATA IOUT/6/
C------------------------------------------------------------------
C  Statement functions for conversion 
C------------------------------------------------------------------
CS    CONV(I) = REAL(I)
CD    CONV(I) = DBLE(I)
C------------------------------------------------------------------
C  Determine machine parameters and set constants
C------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      DX = -P0625
      A = FOURTH + DX
      B = X0 + DX
      N = 2000
      N1 = 25
      XN = CONV(N)
      JT = 0
C-----------------------------------------------------------------
C  Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 8
         K1 = 0
         K3 = 0
         XC = ZERO
         R6 = ZERO
         R7 = ZERO
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            Y = DEL * REN(JT) + XL
            X = Y - DX
            Y = X + DX
C-----------------------------------------------------------------
C  Test Ei against series expansion
C-----------------------------------------------------------------
            V = EI(X)
            Z = EI(Y)
            SUM = ZERO
            U = X
            CALL DSUBN(U,N1,XMAX,D)
            EN = CONV(N1)+ONE
            SUM = D(N1)*DX/EN
            DO 100 K = N1,1,-1
               EN = EN-ONE
               SUM = (SUM + D(K-1))*DX/EN
  100       CONTINUE
            U = V + SUM
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = Z - U
            W = W / Z
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  XC = Y
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C------------------------------------------------------------------
C  Gather and print statistics for test
C------------------------------------------------------------------
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1
         WRITE (IOUT,1015) K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,XC
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = X99
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C------------------------------------------------------------------
C  Initialize for next test
C------------------------------------------------------------------
         IF (J .EQ. 1) THEN
               DX = -DX
               A = X0 + DX
               B = SIX
            ELSE IF (J .LE. 4) THEN
               A = B
               B = B+B
            ELSE IF (J .EQ. 5) THEN
               A = -FOURTH
               B = -ONE
            ELSE IF (J .EQ. 6) THEN
               A = B
               B = -FOUR
            ELSE 
               A = B
               B = -TEN
         END IF
  300 CONTINUE
C-----------------------------------------------------------------
C  Special tests.  First, check accuracy near the zero of Ei(x)
C-----------------------------------------------------------------
      WRITE (IOUT,1040)
      X = (FOUR - ONE) / (FOUR + FOUR)
      Y = EI(X)
      WRITE (IOUT,1041) X,Y
      Z = ((Y - (FOUR+ONE)/(FIV12)) - REM)/Y
      IF (Z .NE. ZERO) THEN
            W = LOG(ABS(Z))/ALBETA
         ELSE
            W = X99
      END IF
      WRITE (IOUT,1042) Z,IBETA,W
      W = MAX(AIT+W,ZERO)
      WRITE (IOUT,1022) IBETA,W
C-----------------------------------------------------------------
C  Check near XBIG, the largest argument acceptable to EONE, i.e.,
C    the negative of the smallest argument acceptable to EI.
C    Determine XBIG with Newton iteration on the equation
C                  EONE(x) = XMIN.
C---------------------------------------------------------------------
      WRITE (IOUT,1050)
      TWO = ONE+ONE
      V = SQRT(EPS)
      C1 = CONV(MINEXP) * LOG(BETA)
      XN = -C1
  320 XNUM = -XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = -(XN*XN+XN+XN+TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 320
      END IF
      XBIG = XNP1
      X = AINT(TEN*XBIG) / TEN
      WRITE (IOUT,1052) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
      X = XBIG * (ONE+V)
      WRITE (IOUT,1053) X
      Y = EONE(X)
      WRITE (IOUT,1062) Y
C---------------------------------------------------------------------
C  Check near XMAX, the largest argument acceptable to EI.  Determine
C    XLGE with Newton iteration on the equation
C                  EI(x) = XMAX.
C---------------------------------------------------------------------
      C1 = CONV(MAXEXP) * LOG(BETA)
      XN = C1
  330 XNUM = XN - LOG(XN) + LOG(ONE+ONE/XN) - C1
      XDEN = (XN*XN-TWO) / (XN*(XN+ONE))
      XNP1 = XN - XNUM/XDEN
      W = (XN-XNP1)/XNP1
      IF (ABS(W) .GT. V) THEN
         XN = XNP1
         GO TO 330
      END IF
      XLGE = XNP1
      X = AINT(TEN*XLGE) / TEN
      WRITE (IOUT,1054) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      X = XLGE * (ONE+V)
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
C---------------------------------------------------------------------
C  Check with XHUGE, the largest acceptable argument for EXPEI
C---------------------------------------------------------------------
      IF (XMIN*XMAX .LE. ONE) THEN
            X = XMAX
         ELSE
            X = ONE/XMIN 
      END IF
      WRITE (IOUT,1056) X
      Y = EXPEI(X)
      WRITE (IOUT,1065) Y
      X = ZERO
      WRITE (IOUT,1055) X
      Y = EI(X)
      WRITE (IOUT,1064) Y
      WRITE (IOUT,1100)
      STOP
C-----------------------------------------------------------------
 1000 FORMAT('1Test of Ei(x) vs series expansion'//)
 1010 FORMAT(I7,' Random arguments were tested from the interval (',
     1    F7.3,',',F7.3,')'//)
 1011 FORMAT('     EI(X) was larger',I6,' times,')
 1015 FORMAT(14X,' agreed',I6,' times, and'/
     1    10X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1040 FORMAT(//' Test of special arguments'//)
 1041 FORMAT('   EI (',E13.6,') = ',E13.6//)
 1042 FORMAT(' The relative error is',E15.4,' = ',I4,' **',F7.2/)
 1050 FORMAT(' Test of Error Returns'///)
 1052 FORMAT(' EONE will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1053 FORMAT(' EONE will be called with the argument',E13.6,/
     1    ' This should underflow'//)
 1054 FORMAT(' EI will be called with the argument',E13.6,/
     1    ' This should not overflow'//)
 1055 FORMAT(' EI will be called with the argument',E13.6,/
     1    ' This should overflow'//)
 1056 FORMAT(' EXPEI will be called with the argument',E13.6,/
     1    ' This should not underflow'//)
 1062 FORMAT(' EONE returned the value',E13.6///)
 1064 FORMAT(' EI returned the value',E13.6///)
 1065 FORMAT(' EXPEI returned the value',E13.6///)
 1100 FORMAT(' This concludes the tests')
C---------- Last line of EI test program ----------
      END
      SUBROUTINE DSUBN(X,NMAX,XMAX,D)
C-------------------------------------------------------------------
C Translation of Gautschi'f CACM Algorithm 282 for
C   derivatives of Ei(x).
C
C  Intrinsic functions required are:
C
C      ABS, EXP, INT, LOG, MIN
C
C-------------------------------------------------------------------
      LOGICAL BOOL1, BOOL2
      INTEGER J,NMAX,N0,MINI,N,N1,LIM
CS    REAL
CD    DOUBLE PRECISION
     1     B0,B1,B2,B3,B4,B5,B6,C0,C1,D,E,EN,ONE,P,Q,T,TEN,
     2     TWO,X,XMAX,X1,Z,ZERO
      DIMENSION D(0:NMAX)
CS    DATA ZERO/0.0E0/,ONE/1.0E0/,TWO/2.0E0/,TEN/10.0E0/
CS    DATA C0/2.7183E0/,C1/4.67452E1/
CS    DATA B0/5.7941E-5/,B1/-1.76148E-3/,B2/2.08645E-2/,
CS   1     B3/-1.29013E-1/,B4/8.5777E-1/,B5/1.0125E0/,B6/7.75E-1/
CD    DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,TEN/10.0D0/
CD    DATA C0/2.7183D0/,C1/4.67452D1/
CD    DATA B0/5.7941D-5/,B1/-1.76148D-3/,B2/2.08645D-2/,
CD   1     B3/-1.29013D-1/,B4/8.5777D-1/,B5/1.0125D0/,B6/7.75D-1/
C-------------------------------------------------------------------
      X1 = ABS(X)
      N0 = INT(X1)
      E = EXP(X)
      D(0) = E/X
      BOOL1 = (X .LT. ZERO) .OR. (X1 .LE. TWO)
      BOOL2 = N0 .LT. NMAX
      MINI = MIN(N0,NMAX)
      IF (BOOL1) THEN
            LIM = NMAX
         ELSE
            LIM = MINI
      END IF
      N = 1
      EN = ONE
   50 D(N) = (E - EN*D(N-1))/X
         N = N +1
         EN = EN + ONE
         IF (X1 .LT. ONE) THEN
               IF ((ABS(D(N-1)) .LT. ABS(XMAX*X/EN)) .AND.
     1            (N .LE. LIM)) GO TO 50
            ELSE
               IF ((ABS(D(N-1)/X) .LT. XMAX/EN) .AND. (N .LE. LIM))
     1            GO TO 50
         END IF
         DO 100 J = N, LIM
            D(N) = ZERO
  100    CONTINUE
      IF ((.NOT. BOOL1) .AND. BOOL2) THEN
         T = (X1+C1)/(C0*X1)
         IF (T .LT. TEN) THEN
               T = ((((B0*T + B1)*T + B2)*T + B3)*T + B4)*T + B5
            ELSE
               Z = LOG(T) - B6
               P = (B6-LOG(Z))/(ONE+Z)
               P = ONE/(ONE+P)
               T = T*P/Z
         END IF
         N1 = C0*X1*T - ONE
         IF (N1 .LT. NMAX) N1 = NMAX
         Q = ONE/X
         EN = ONE
         DO 120 N = 1,N1+1
            Q = -EN*Q/X
            EN = EN+ONE
  120    CONTINUE
         DO 140 N = N1,N0+1,-1
            EN = EN - ONE
            Q = (E-X*Q)/EN
            IF (N .LE. NMAX) D(N) = Q
  140    CONTINUE
      END IF
      RETURN
C---------- Last line of DSUBN ----------
      END
      SUBROUTINE MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1                   MAXEXP,EPS,EPSNEG,XMIN,XMAX)
C----------------------------------------------------------------------
C  This Fortran 77 subroutine is intended to determine the parameters
C   of the floating-point arithmetic system specified below.  The
C   determination of the first three uses an extension of an algorithm
C   due to M. Malcolm, CACM 15 (1972), pp. 949-951, incorporating some,
C   but not all, of the improvements suggested by M. Gentleman and S.
C   Marovich, CACM 17 (1974), pp. 276-277.  An earlier version of this
C   program was published in the book Software Manual for the
C   Elementary Functions by W. J. Cody and W. Waite, Prentice-Hall,
C   Englewood Cliffs, NJ, 1980.
C
C  The program as given here must be modified before compiling.  If
C   a single (double) precision version is desired, change all
C   occurrences of CS (CD) in columns 1 and 2 to blanks.
C
C  Parameter values reported are as follows:
C
C       IBETA   - the radix for the floating-point representation
C       IT      - the number of base IBETA digits in the floating-point
C                 significand
C       IRND    - 0 if floating-point addition chops
C                 1 if floating-point addition rounds, but not in the
C                   IEEE style
C                 2 if floating-point addition rounds in the IEEE style
C                 3 if floating-point addition chops, and there is
C                   partial underflow
C                 4 if floating-point addition rounds, but not in the
C                   IEEE style, and there is partial underflow
C                 5 if floating-point addition rounds in the IEEE style,
C                   and there is partial underflow
C       NGRD    - the number of guard digits for multiplication with
C                 truncating arithmetic.  It is
C                 0 if floating-point arithmetic rounds, or if it
C                   truncates and only  IT  base  IBETA digits
C                   participate in the post-normalization shift of the
C                   floating-point significand in multiplication;
C                 1 if floating-point arithmetic truncates and more
C                   than  IT  base  IBETA  digits participate in the
C                   post-normalization shift of the floating-point
C                   significand in multiplication.
C       MACHEP  - the largest negative integer such that
C                 1.0+FLOAT(IBETA)**MACHEP .NE. 1.0, except that
C                 MACHEP is bounded below by  -(IT+3)
C       NEGEPS  - the largest negative integer such that
C                 1.0-FLOAT(IBETA)**NEGEPS .NE. 1.0, except that
C                 NEGEPS is bounded below by  -(IT+3)
C       IEXP    - the number of bits (decimal places if IBETA = 10)
C                 reserved for the representation of the exponent
C                 (including the bias or sign) of a floating-point
C                 number
C       MINEXP  - the largest in magnitude negative integer such that
C                 FLOAT(IBETA)**MINEXP is positive and normalized
C       MAXEXP  - the smallest positive power of  BETA  that overflows
C       EPS     - FLOAT(IBETA)**MACHEP.
C       EPSNEG  - FLOAT(IBETA)**NEGEPS.
C       XMIN    - the smallest non-vanishing normalized floating-point
C                 power of the radix, i.e.,  XMIN = FLOAT(IBETA)**MINEXP
C       XMAX    - the largest finite floating-point number.  In
C                 particular  XMAX = (1.0-EPSNEG)*FLOAT(IBETA)**MAXEXP
C                 Note - on some machines  XMAX  will be only the
C                 second, or perhaps third, largest number, being
C                 too small by 1 or 2 units in the last digit of
C                 the significand.
C
C  Latest modification: May 30, 1989
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C----------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IRND,IT,ITEMP,IZ,J,K,MACHEP,MAXEXP,
     1        MINEXP,MX,NEGEP,NGRD,NXRES
CS    REAL
CD    DOUBLE PRECISION
     1   A,B,BETA,BETAIN,BETAH,CONV,EPS,EPSNEG,ONE,T,TEMP,TEMPA,
     2   TEMP1,TWO,XMAX,XMIN,Y,Z,ZERO
C----------------------------------------------------------------------
CS    CONV(I) = REAL(I)
CD    CONV(I) = DBLE(I)
      ONE = CONV(1)
      TWO = ONE + ONE
      ZERO = ONE - ONE
C----------------------------------------------------------------------
C  Determine IBETA, BETA ala Malcolm.
C----------------------------------------------------------------------
      A = ONE
   10 A = A + A
         TEMP = A+ONE
         TEMP1 = TEMP-A
         IF (TEMP1-ONE .EQ. ZERO) GO TO 10
      B = ONE
   20 B = B + B
         TEMP = A+B
         ITEMP = INT(TEMP-A)
         IF (ITEMP .EQ. 0) GO TO 20
      IBETA = ITEMP
      BETA = CONV(IBETA)
C----------------------------------------------------------------------
C  Determine IT, IRND.
C----------------------------------------------------------------------
      IT = 0
      B = ONE
  100 IT = IT + 1
         B = B * BETA
         TEMP = B+ONE
         TEMP1 = TEMP-B
         IF (TEMP1-ONE .EQ. ZERO) GO TO 100
      IRND = 0
      BETAH = BETA / TWO
      TEMP = A+BETAH
      IF (TEMP-A .NE. ZERO) IRND = 1
      TEMPA = A + BETA
      TEMP = TEMPA+BETAH
      IF ((IRND .EQ. 0) .AND. (TEMP-TEMPA .NE. ZERO)) IRND = 2
C----------------------------------------------------------------------
C  Determine NEGEP, EPSNEG.
C----------------------------------------------------------------------
      NEGEP = IT + 3
      BETAIN = ONE / BETA
      A = ONE
      DO 200 I = 1, NEGEP
         A = A * BETAIN
  200 CONTINUE
      B = A
  210 TEMP = ONE-A
         IF (TEMP-ONE .NE. ZERO) GO TO 220
         A = A * BETA
         NEGEP = NEGEP - 1
      GO TO 210
  220 NEGEP = -NEGEP
      EPSNEG = A
C----------------------------------------------------------------------
C  Determine MACHEP, EPS.
C----------------------------------------------------------------------
      MACHEP = -IT - 3
      A = B
  300 TEMP = ONE+A
         IF (TEMP-ONE .NE. ZERO) GO TO 320
         A = A * BETA
         MACHEP = MACHEP + 1
      GO TO 300
  320 EPS = A
C----------------------------------------------------------------------
C  Determine NGRD.
C----------------------------------------------------------------------
      NGRD = 0
      TEMP = ONE+EPS
      IF ((IRND .EQ. 0) .AND. (TEMP*ONE-ONE .NE. ZERO)) NGRD = 1
C----------------------------------------------------------------------
C  Determine IEXP, MINEXP, XMIN.
C
C  Loop to determine largest I and K = 2**I such that
C         (1/BETA) ** (2**(I))
C  does not underflow.
C  Exit from loop is signaled by an underflow.
C----------------------------------------------------------------------
      I = 0
      K = 1
      Z = BETAIN
      T = ONE + EPS
      NXRES = 0
  400 Y = Z
         Z = Y * Y
C----------------------------------------------------------------------
C  Check for underflow here.
C----------------------------------------------------------------------
         A = Z * ONE
         TEMP = Z * T
         IF ((A+A .EQ. ZERO) .OR. (ABS(Z) .GE. Y)) GO TO 410
         TEMP1 = TEMP * BETAIN
         IF (TEMP1*BETA .EQ. Z) GO TO 410
         I = I + 1
         K = K + K
      GO TO 400
  410 IF (IBETA .EQ. 10) GO TO 420
      IEXP = I + 1
      MX = K + K
      GO TO 450
C----------------------------------------------------------------------
C  This segment is for decimal machines only.
C----------------------------------------------------------------------
  420 IEXP = 2
      IZ = IBETA
  430 IF (K .LT. IZ) GO TO 440
         IZ = IZ * IBETA
         IEXP = IEXP + 1
      GO TO 430
  440 MX = IZ + IZ - 1
C----------------------------------------------------------------------
C  Loop to determine MINEXP, XMIN.
C  Exit from loop is signaled by an underflow.
C----------------------------------------------------------------------
  450 XMIN = Y
         Y = Y * BETAIN
C----------------------------------------------------------------------
C  Check for underflow here.
C----------------------------------------------------------------------
         A = Y * ONE
         TEMP = Y * T
         IF (((A+A) .EQ. ZERO) .OR. (ABS(Y) .GE. XMIN)) GO TO 460
         K = K + 1
         TEMP1 = TEMP * BETAIN
         IF ((TEMP1*BETA .NE. Y) .OR. (TEMP .EQ. Y)) THEN
               GO TO 450
            ELSE
               NXRES = 3
               XMIN = Y
         END IF
  460 MINEXP = -K
C----------------------------------------------------------------------
C  Determine MAXEXP, XMAX.
C----------------------------------------------------------------------
      IF ((MX .GT. K+K-3) .OR. (IBETA .EQ. 10)) GO TO 500
      MX = MX + MX
      IEXP = IEXP + 1
  500 MAXEXP = MX + MINEXP
C----------------------------------------------------------------------
C  Adjust IRND to reflect partial underflow.
C----------------------------------------------------------------------
      IRND = IRND + NXRES
C----------------------------------------------------------------------
C  Adjust for IEEE-style machines.
C----------------------------------------------------------------------
      IF (IRND .GE. 2) MAXEXP = MAXEXP - 2
C----------------------------------------------------------------------
C  Adjust for machines with implicit leading bit in binary
C  significand, and machines with radix point at extreme
C  right of significand.
C----------------------------------------------------------------------
      I = MAXEXP + MINEXP
      IF ((IBETA .EQ. 2) .AND. (I .EQ. 0)) MAXEXP = MAXEXP - 1
      IF (I .GT. 20) MAXEXP = MAXEXP - 1
      IF (A .NE. Y) MAXEXP = MAXEXP - 2
      XMAX = ONE - EPSNEG
      IF (XMAX*ONE .NE. XMAX) XMAX = ONE - BETA * EPSNEG
      XMAX = XMAX / (BETA * BETA * BETA * XMIN)
      I = MAXEXP + MINEXP + 3
      IF (I .LE. 0) GO TO 520
      DO 510 J = 1, I
          IF (IBETA .EQ. 2) XMAX = XMAX + XMAX
          IF (IBETA .NE. 2) XMAX = XMAX * BETA
  510 CONTINUE
  520 RETURN
C---------- Last line of MACHAR ----------
      END
      FUNCTION REN(K)
C---------------------------------------------------------------------
C  Random number generator - based on Algorithm 266 by Pike and
C   Hill (modified by Hansson), Communications of the ACM,
C   Vol. 8, No. 10, October 1965.
C
C  This subprogram is intended for use on computers with
C   fixed point wordlength of at least 29 bits.  It is
C   best if the floating-point significand has at most
C   29 bits.
C
C  Latest modification: May 30, 1989
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C---------------------------------------------------------------------
      INTEGER IY,J,K
CS    REAL             CONV,C1,C2,C3,ONE,REN
CD    DOUBLE PRECISION CONV,C1,C2,C3,ONE,REN
      DATA IY/100001/
CS    DATA ONE,C1,C2,C3/1.0E0,2796203.0E0,1.0E-6,1.0E-12/
CD    DATA ONE,C1,C2,C3/1.0D0,2796203.0D0,1.0D-6,1.0D-12/
C---------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C---------------------------------------------------------------------
CS    CONV(J) = REAL(J)
CD    CONV(J) = DBLE(J)
C---------------------------------------------------------------------
      J = K
      IY = IY * 125
      IY = IY - (IY/2796203) * 2796203
      REN = CONV(IY) / C1 * (ONE + C2 + C3)
      RETURN
C---------- Last card of REN ----------
      END
