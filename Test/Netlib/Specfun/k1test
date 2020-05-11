C--------------------------------------------------------------------
C  FORTRAN 77 program to test BESK1
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
C              be deleted provided the following three
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest positive power of BETA
C                          that overflows
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest non-vanishing normalized
C                          floating-point power of the radix, i.e.,
C                          XMIN = FLOAT(IBETA) ** MINEXP
C                 XMAX   - the largest finite floating-point number.
C                          In particular XMAX = (1.0-EPSNEG) * 
C                          FLOAT(IBETA) ** MAXEXP
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  User defined functions
C
C         BOT, TOP
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: May 30, 1989
C
C  Author - Laura Stoltz 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C--------------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
CD    DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,ATETEN,B,BESEK1,BESK0,BESK1,BETA,
     2   BOT,C,CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIFTEN,FIVE,FOUR8,
     3   HALF,HUND,ONE,ONENEG,ONE28,PI,REN,R6,R7,SUM,T,T1,THIRTY,THREE,
     4   TOP,TWENTY,TWO,U,W,X,XA,XB,XDEN,XL,XLAM,XLARGE,XLEAST,XMAX,
     5   XMB,XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
CS    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0E0,0.5E0,1.0E0,2.0E0,8.0E0/,
CS   1   XNINE,ATETEN,TWENTY,ONE28/9.0E0,18.0E0,20.0E0,128.0E0/,
CS   2   FIVE,ONENEG,XDEN,ALL9/5.0E0,-1.0E0,16.0E0,-999.0E0/,
CS   3   THREE,FIFTEN,THIRTY,FOUR8/3.0E0,15.0E0,30.0E0,48.0E0/,
CS   4   PI/3.141592653589793E0/,HUND/100.0E0/
CD    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
CD   1   XNINE,ATETEN,TWENTY,ONE28/9.0D0,18.0D0,20.0D0,128.0D0/,
CD   2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
CD   3   THREE,FIFTEN,THIRTY,FOUR8/3.0D0,15.0D0,30.0D0,48.0D0/,
CD   4   PI/3.141592653589793D0/,HUND/100.0D0/
C---------------------------------------------------------------------
C  Machine-dependent constant
C---------------------------------------------------------------------
CS    DATA XLEAST/1.18E-38/
CD    DATA XLEAST/2.23D-308/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE+(THREE/EIGHT-FIFTEN/
     1   ONE28/X)/X)
      BOT(X) = - ONE - HALF/X + ((- FOUR8*X + THIRTY) / 
     1   (((ONE28*X+FOUR8)*X-FIFTEN)*X))
C---------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C---------------------------------------------------------------------
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
      AMAXEXP = CONV(MAXEXP)
      JT = 0
      B = EPS
      XLAM = (XDEN - ONE) / XDEN
      CONST = HALF * LOG(PI) - LOG(XMIN)
C--------------------------------------------------------------------
C     Random argument accuracy tests
C--------------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = ONE
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
         END IF
         XN = CONV(N)
         DEL = (B - A) / XN
         XL = A
C---------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C---------------------------------------------------------------------
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
            Y = X / XLAM
            W = XDEN * Y
            Y = (W + Y) - W 
            X = Y * XLAM
            U(0) = BESK0(Y)
            U(1) = BESK1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
                  U(0) = U(0) * EPS
                  U(1) = U(1) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y 
            T = U(0) + C * U(1)
            T1 = EPS / HUND
            DO 110 II = 2, 60
               Z = U(II-1)
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                     T = T * T1
                     T1 = ONE
               END IF
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            MB = MB - 1
            DO 155 II = 1, MB
               XMB = XMB - ONE
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            ZZ = ZZ * XLAM
            Z = BESK1(X)
            Y = Z
            IF (U(0) .GT. Y) Y= U(0)
            W = (Z - ZZ) / Y
            IF (W .GT. ZERO) THEN 
                  K1 = K1 + 1
               ELSE IF (W .LT. ZERO) THEN 
                  K3 = K3 + 1
               ELSE
                  K2 = K2 + 1
            END IF
            W = ABS(W)
            IF (W .GT. R6) THEN
                  R6 = W
                  X1 = X
            END IF
            R7 = R7 + W * W
            XL = XL + DEL
  200    CONTINUE
         N = K1 + K2 + K3
         XN = CONV(N)
         R7 = SQRT(R7/XN)
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(R6)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(R7)/ALBETA
         IF (J .EQ. 3) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
  300 CONTINUE
C--------------------------------------------------------------------
C  Special tests
C--------------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK1(XLEAST)
      WRITE (IOUT,1032) Y
      Y = BESK1(XMIN)
      WRITE (IOUT,1036) Y
      Y = BESK1(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK1(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK1(XMAX)
      WRITE (IOUT,1035) Y
      XA = LOG(XMAX)
  330 XB = XA - (TOP(XA)+CONST) / BOT(XA)
      IF (ABS(XB-XA)/XB .LE. EPS) THEN 
            GO TO 350
         ELSE
            XA = XB
            GO TO 330
      END IF
  350 XLARGE = XB * XLAM 
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK1(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
C-------------------------------------------------------------------- 
C  Test of error returns 
C--------------------------------------------------------------------
      STOP
 1000 FORMAT('1Test of K1(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K1(X)) was larger',I6,' times,'/
     1    20X,' agreed',I6,' times, and'/
     1    16X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.'//)
 1021 FORMAT(' The maximum relative error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1    ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,' = ',I4,' **',
     1    F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    ' = ',I4,' **',F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(//' Test with extreme arguments'/)
 1032 FORMAT(' K1(XLEAST) = ',E24.17/)
 1033 FORMAT(' K1(',I1,') = ',E24.17/)
 1034 FORMAT(' K1(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**X * K1(XMAX) = ',E24.17/)
 1036 FORMAT(' K1(XMIN) = ',E24.17/)
C---------- Last line of BESK1 test program ----------
      END
      SUBROUTINE CALCK0(ARG,RESULT,JINT)
C--------------------------------------------------------------------
C
C This packet computes modified Bessel functions of the second kind
C   and order zero, K0(X) and EXP(X)*K0(X), for real
C   arguments X.  It contains two function type subprograms, BESK0
C   and BESEK0, and one subroutine type subprogram, CALCK0.
C   the calling statements for the primary entries are
C
C                   Y=BESK0(X)
C   and
C                   Y=BESEK0(X)
C
C   where the entry points correspond to the functions K0(X) and
C   EXP(X)*K0(X), respectively.  The routine CALCK0 is
C   intended for internal packet use only, all computations within
C   the packet being concentrated in this routine.  The function
C   subprograms invoke CALCK0 with the statement
C          CALL CALCK0(ARG,RESULT,JINT)
C   where the parameter usage is as follows
C
C      Function                     Parameters for CALCK0
C       Call              ARG                  RESULT          JINT
C
C     BESK0(ARG)   0 .LT. ARG .LE. XMAX       K0(ARG)           1
C     BESEK0(ARG)     0 .LT. ARG           EXP(ARG)*K0(ARG)     2
C
C   The main computation evaluates slightly modified forms of near 
C   minimax rational approximations generated by Russon and Blair, 
C   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461, 
C   1969.  This transportable program is patterned after the 
C   machine-dependent FUNPACK packet NATSK0, but cannot match that 
C   version for efficiency or accuracy.  This version uses rational
C   functions that theoretically approximate K-SUB-0(X) to at
C   least 18 significant decimal digits.  The accuracy achieved
C   depends on the arithmetic system, the compiler, the intrinsic
C   functions, and proper selection of the machine-dependent
C   constants.
C
C*******************************************************************
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   beta   = Radix for the floating-point system
C   minexp = Smallest representable power of beta
C   maxexp = Smallest power of beta that overflows
C   XSMALL = Argument below which BESK0 and BESEK0 may
C            each be represented by a constant and a log.
C            largest X such that  1.0 + X = 1.0  to machine
C            precision.
C   XINF   = Largest positive machine number; approximately
C            beta**maxexp
C   XMAX   = Largest argument acceptable to BESK0;  Solution to
C            equation: 
C               W(X) * (1-1/8X+9/128X**2) = beta**minexp
C            where  W(X) = EXP(-X)*SQRT(PI/2X)
C
C
C     Approximate values for some important machines are:
C
C
C                           beta       minexp       maxexp
C
C  CRAY-1        (S.P.)       2        -8193         8191
C  Cyber 180/185 
C    under NOS   (S.P.)       2         -975         1070
C  IEEE (IBM/XT,
C    SUN, etc.)  (S.P.)       2         -126          128
C  IEEE (IBM/XT,
C    SUN, etc.)  (D.P.)       2        -1022         1024
C  IBM 3033      (D.P.)      16          -65           63
C  VAX D-Format  (D.P.)       2         -128          127
C  VAX G-Format  (D.P.)       2        -1024         1023
C
C
C                          XSMALL       XINF         XMAX
C
C CRAY-1        (S.P.)    3.55E-15   5.45E+2465    5674.858
C Cyber 180/855
C   under NOS   (S.P.)    1.77E-15   1.26E+322      672.788
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)    5.95E-8    3.40E+38        85.337
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)    1.11D-16   1.79D+308      705.342
C IBM 3033      (D.P.)    1.11D-16   7.23D+75       177.852
C VAX D-Format  (D.P.)    6.95D-18   1.70D+38        86.715
C VAX G-Format  (D.P.)    5.55D-17   8.98D+307      706.728
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns the value XINF for ARG .LE. 0.0, and the
C  BESK0 entry returns the value 0.0 for ARG .GT. XMAX.
C
C
C  Intrinsic functions required are:
C
C     EXP, LOG, SQRT
C
C  Latest modification: March 19, 1990
C
C  Authors: W. J. Cody and Laura Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,JINT
CS    REAL  
CD    DOUBLE PRECISION
     1    ARG,F,G,ONE,P,PP,Q,QQ,RESULT,SUMF,SUMG,SUMP,SUMQ,TEMP,
     2    X,XINF,XMAX,XSMALL,XX,ZERO
      DIMENSION P(6),Q(2),PP(10),QQ(10),F(4),G(3)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
CS    DATA ONE/1.0E0/,ZERO/0.0E0/
CD    DATA ONE/1.0D0/,ZERO/0.0D0/
C--------------------------------------------------------------------
C  Machine-dependent constants
C--------------------------------------------------------------------
CS    DATA XSMALL/5.95E-8/,XINF/3.40E+38/,XMAX/ 85.337E0/
CD    DATA XSMALL/1.11D-16/,XINF/1.79D+308/,XMAX/705.342D0/
C--------------------------------------------------------------------
C
C     Coefficients for XSMALL .LE.  ARG  .LE. 1.0
C
C--------------------------------------------------------------------
CS    DATA   P/ 5.8599221412826100000E-04, 1.3166052564989571850E-01,  
CS   1          1.1999463724910714109E+01, 4.6850901201934832188E+02,
CS   2          5.9169059852270512312E+03, 2.4708152720399552679E+03/
CS    DATA   Q/-2.4994418972832303646E+02, 2.1312714303849120380E+04/ 
CS    DATA   F/-1.6414452837299064100E+00,-2.9601657892958843866E+02,
CS   1         -1.7733784684952985886E+04,-4.0320340761145482298E+05/
CS    DATA   G/-2.5064972445877992730E+02, 2.9865713163054025489E+04,
CS   1         -1.6128136304458193998E+06/
CD    DATA   P/ 5.8599221412826100000D-04, 1.3166052564989571850D-01,  
CD   1          1.1999463724910714109D+01, 4.6850901201934832188D+02,
CD   2          5.9169059852270512312D+03, 2.4708152720399552679D+03/
CD    DATA   Q/-2.4994418972832303646D+02, 2.1312714303849120380D+04/ 
CD    DATA   F/-1.6414452837299064100D+00,-2.9601657892958843866D+02,
CD   1         -1.7733784684952985886D+04,-4.0320340761145482298D+05/
CD    DATA   G/-2.5064972445877992730D+02, 2.9865713163054025489D+04,
CD   1         -1.6128136304458193998D+06/
C--------------------------------------------------------------------
C
C     Coefficients for  1.0 .LT. ARG
C
C--------------------------------------------------------------------
CS    DATA  PP/ 1.1394980557384778174E+02, 3.6832589957340267940E+03,
CS   1          3.1075408980684392399E+04, 1.0577068948034021957E+05,
CS   2          1.7398867902565686251E+05, 1.5097646353289914539E+05,
CS   3          7.1557062783764037541E+04, 1.8321525870183537725E+04,
CS   4          2.3444738764199315021E+03, 1.1600249425076035558E+02/
CS    DATA  QQ/ 2.0013443064949242491E+02, 4.4329628889746408858E+03,
CS   1          3.1474655750295278825E+04, 9.7418829762268075784E+04,
CS   2          1.5144644673520157801E+05, 1.2689839587977598727E+05,
CS   3          5.8824616785857027752E+04, 1.4847228371802360957E+04,
CS   4          1.8821890840982713696E+03, 9.2556599177304839811E+01/
CD    DATA  PP/ 1.1394980557384778174D+02, 3.6832589957340267940D+03,
CD   1          3.1075408980684392399D+04, 1.0577068948034021957D+05,
CD   2          1.7398867902565686251D+05, 1.5097646353289914539D+05,
CD   3          7.1557062783764037541D+04, 1.8321525870183537725D+04,
CD   4          2.3444738764199315021D+03, 1.1600249425076035558D+02/
CD    DATA  QQ/ 2.0013443064949242491D+02, 4.4329628889746408858D+03,
CD   1          3.1474655750295278825D+04, 9.7418829762268075784D+04,
CD   2          1.5144644673520157801D+05, 1.2689839587977598727D+05,
CD   3          5.8824616785857027752D+04, 1.4847228371802360957D+04,
CD   4          1.8821890840982713696D+03, 9.2556599177304839811D+01/
C--------------------------------------------------------------------
      X = ARG
      IF (X .GT. ZERO) THEN
            IF (X .LE. ONE) THEN
C--------------------------------------------------------------------
C     0.0 .LT.  ARG  .LE. 1.0
C--------------------------------------------------------------------
                  TEMP = LOG(X)
                  IF (X .LT. XSMALL) THEN
C--------------------------------------------------------------------
C     Return for small ARG
C--------------------------------------------------------------------
                        RESULT = P(6)/Q(2) - TEMP
                     ELSE
                        XX = X * X
                        SUMP = ((((P(1)*XX + P(2))*XX + P(3))*XX +
     1                         P(4))*XX + P(5))*XX + P(6)
                        SUMQ = (XX + Q(1))*XX + Q(2)
                        SUMF = ((F(1)*XX + F(2))*XX + F(3))*XX + F(4)
                        SUMG = ((XX + G(1))*XX + G(2))*XX + G(3)
                        RESULT = SUMP/SUMQ - XX*SUMF*TEMP/SUMG - TEMP
                        IF (JINT .EQ. 2) RESULT = RESULT * EXP(X)
                  END IF
               ELSE IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
C--------------------------------------------------------------------
C     Error return for ARG .GT. XMAX
C--------------------------------------------------------------------
                  RESULT = ZERO
               ELSE
C--------------------------------------------------------------------
C     1.0 .LT. ARG
C--------------------------------------------------------------------
                  XX = ONE / X
                  SUMP = PP(1)
                  DO 120 I = 2, 10
                     SUMP = SUMP*XX + PP(I)
  120             CONTINUE
                  SUMQ = XX
                  DO 140 I = 1, 9
                     SUMQ = (SUMQ + QQ(I))*XX
  140             CONTINUE
                  SUMQ = SUMQ + QQ(10)
                  RESULT = SUMP / SUMQ / SQRT(X)
                  IF (JINT .EQ. 1) RESULT = RESULT * EXP(-X)
            END IF
         ELSE
C--------------------------------------------------------------------
C     Error return for ARG .LE. 0.0
C--------------------------------------------------------------------
            RESULT = XINF
      END IF
C--------------------------------------------------------------------
C     Update error counts, etc.
C--------------------------------------------------------------------
      RETURN
C---------- Last line of CALCK0 ----------
      END
CS    REAL 
CD    DOUBLE PRECISION
     1    FUNCTION BESK0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order zero
C   for arguments 0.0 .LT. ARG .LE. XMAX (see comments heading
C   CALCK0).
C
C  Authors: W. J. Cody and Laura Stoltz
C
C  Latest Modification: January 19, 1988
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL 
CD    DOUBLE PRECISION
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 1
      CALL CALCK0(X,RESULT,JINT)
      BESK0 = RESULT
      RETURN
C---------- Last line of BESK0 ----------
      END
CS    REAL
CD    DOUBLE PRECISION 
     1    FUNCTION BESEK0(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order zero
C   multiplied by the Exponential function, for arguments
C   0.0 .LT. ARG.
C
C  Authors: W. J. Cody and Laura Stoltz
C
C  Latest Modification: January 19, 1988
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL 
CD    DOUBLE PRECISION 
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 2
      CALL CALCK0(X,RESULT,JINT)
      BESEK0 = RESULT
      RETURN
C---------- Last line of BESEK0 ----------
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
