C--------------------------------------------------------------------
C  Fortran 77 program to test PSI
C
C  Data required
C
C     None
C
C  Subprograms required from this package
C
C     MACHAR - an environmental inquiry program providing
C              information on the floating-point arithmetic
C              system.  Note that the call to MACHAR can
C              be deleted provided the following five
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MINEXP - the largest in magnitude negative
C                          integer such that  FLOAT(IBETA)**MINEXP
C                          is a positive floating-point number
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 EPSNEG - the smallest positive floating-point
C                          number such that 1.0-EPSNEG .NE. 1.0
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic Fortran functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs related
C              to the real gamma function", W. J. Cody, 
C              submitted for publication.
C
C  Latest modification: May 30, 1989
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,IBETA,IEXP,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
CD    DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,B,BETA,CONV,DEL,EIGHT,EPS,
     2   EPSNEG,HALF,ONE,ONE7,ONE6,PSI,REN,R6,R7,THREE,
     3   TWENTY,Y,V0,W,X,XH,XL,XL2,XMAX,XMIN,XN,XX,X0,
     4   X01,X1,Z,ZERO,ZH,ZZ
CS    DATA ZERO,ONE,THREE/0.0E0,1.0E0,3.0E0/,
CS   1   HALF,EIGHT,TWENTY,ALL9/0.5E0,8.0E0,20.0E0,-999.0E0/,
CS   2   XL2/6.9314718055994530942E-1/,
CS   3   ONE7,ONE6/-17.625E0,-16.875E0/,
CS   4   X0,X01,V0/374.0E0,256.0E0,-6.7240239024288040437E-04/
CD    DATA ZERO,ONE,THREE/0.0E0,1.0E0,3.0E0/,
CD   1   HALF,EIGHT,TWENTY,ALL9/0.5D0,8.0D0,20.0D0,-999.0D0/,
CD   2   XL2/6.9314718055994530942D-1/,
CD   3   ONE7,ONE6/-17.625D0,-16.875D0/,
CD   4   X0,X01,V0/374.0D0,256.0D0,-6.7240239024288040437D-04/
      DATA IOUT/6/
C--------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C--------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
CD    CONV(NDUM) = DBLE(NDUM)
C--------------------------------------------------------------------
C  Determine machine parameters and set constants
C--------------------------------------------------------------------
      CALL MACHAR(IBETA,IT,IRND,NGRD,MACHEP,NEGEP,IEXP,MINEXP,
     1            MAXEXP,EPS,EPSNEG,XMIN,XMAX)
      BETA = CONV(IBETA)
      ALBETA = LOG(BETA)
      AIT = CONV(IT)
      JT = 0
C--------------------------------------------------------------------
C     Random argument accuracy tests
C--------------------------------------------------------------------
      DO 300 J = 1, 4
         K1 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (J .EQ. 1) THEN
               A = ZERO
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               A = B + B
               B = EIGHT
            ELSE IF (J .EQ. 3) THEN
               A = B
               B = TWENTY
            ELSE
               A = ONE7
               B = ONE6
               N = 500
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C--------------------------------------------------------------------
C  Carefully purify arguments and evaluate identity
C--------------------------------------------------------------------
            XX = X * HALF
            XH = XX + HALF
            XX = XH - HALF
            X = XX + XX
            Z = PSI(X)
            ZH = PSI(XH)
            ZZ = PSI(XX)
            ZZ = (ZZ+ZH)*HALF + XL2
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
            W = (ZZ - Z) / ZZ
            IF (W .GT. ZERO) THEN
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN
                  K3 = K3 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
               R6 = W
               X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
C--------------------------------------------------------------------
C  Process and output statistics
C--------------------------------------------------------------------
         K2 = N - K3 - K1
         R7 = SQRT(R7/XN)
         IF (2*(J/2) .NE. J) WRITE (IOUT,1000)
         WRITE (IOUT,1001)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1021) R6,IBETA,W,X1
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         WRITE (IOUT,1023) R7,IBETA,W
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
C--------------------------------------------------------------------
C  Special tests
C--------------------------------------------------------------------
      WRITE (IOUT,1030)
         X = X0/X01
         Y = PSI(X)
         Z = (Y-V0)/V0
         IF (Z .NE. ZERO) THEN
               W = LOG(ABS(Z))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1031) X,Y,IBETA,W
      WRITE (IOUT,1033)
      IF (XMAX*XMIN .GE. ONE) THEN
            X = XMIN
         ELSE 
            X = ONE / XMAX
      END IF
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1035) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1037)
      X = ZERO
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      X = -THREE/EPS
      WRITE (IOUT,1034) X
      Y = PSI(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
C--------------------------------------------------------------------
 1000 FORMAT('1')
 1001 FORMAT(' Test of PSI(X) vs (PSI(X/2)+PSI(X/2+1/2))/2 + ln(2)'
     1 //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(PSI(X)) was larger',I6,' times', /
     1     21X,' agreed',I6,' times, and'/
     1   17X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near positive zero'//' PSI(',E14.7,') = ',
     1    E24.17/13X,'Loss of base',I3,' digits = ',F7.2/)
 1033 FORMAT(//' Test with extreme arguments'/)
 1034 FORMAT(' PSI will be called with the argument ',E17.10/
     1     ' This may stop execution.'/)
 1035 FORMAT(' PSI will be called with the argument ',E17.10/
     1     ' This should not stop execution.'/)
 1036 FORMAT(' PSI returned the value',E25.17//)
 1037 FORMAT(//' Test of error returns'//)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of PSI test program ----------
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
