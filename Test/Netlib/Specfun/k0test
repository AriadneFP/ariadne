C--------------------------------------------------------------------
C  FORTRAN 77 program to test BESK0
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
C  Author: Laura Stoltz 
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C-----------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
CD    DOUBLE PRECISION
     1   A,AIT,ALBETA,ALL9,AMAXEXP,ATETEN,B,BESEK0,BESK0,BESK1,BETA,
     2   BOT,C,CONST,CONV,DEL,EIGHT,EPS,EPSNEG,FIVE,HALF,ONE,ONENEG,
     3   ONE28,PI,REN,R6,R7,SUM,T,TOP,TWENTY,TWO,U,W,X,XA,XB,XDEN,
     4   XL,XLAM,XLARGE,XMAX,XMB,XMIN,XN,XNINE,X1,Y,Z,ZERO,ZZ
      DIMENSION U(0:559)
CS    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0E0,0.5E0,1.0E0,2.0E0,8.0E0/,
CS   1   XNINE,ATETEN,TWENTY,ONE28/9.0E0,18.0E0,20.0E0,128.0E0/,
CS   2   FIVE,ONENEG,XDEN,ALL9/5.0E0,-1.0E0,16.0E0,-999.0E0/,
CS   3   PI/3.141592653589793E0/
CD    DATA ZERO,HALF,ONE,TWO,EIGHT/0.0D0,0.5D0,1.0D0,2.0D0,8.0D0/,
CD   1   XNINE,ATETEN,TWENTY,ONE28/9.0D0,18.0D0,20.0D0,128.0D0/,
CD   2   FIVE,ONENEG,XDEN,ALL9/5.0D0,-1.0D0,16.0D0,-999.0D0/,
CD   3   PI/3.141592653589793D0/
      DATA IOUT/6/
      TOP(X) = -X - HALF*LOG(TWO*X) + LOG(ONE-(ONE/EIGHT-XNINE/
     1   ONE28/X)/X)
      BOT(X) = (XDEN*X-ATETEN) / (((ONE28*X-XDEN)*X+XNINE)*X)
     1   - ONE - HALF/X
C------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C------------------------------------------------------------------
CS    CONV(NDUM) = REAL(NDUM)
CD    CONV(NDUM) = DBLE(NDUM)
C-----------------------------------------------------------------
C  Determine machine parameters and set constants
C-----------------------------------------------------------------
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
C-----------------------------------------------------------------
C     Random argument accuracy tests
C-----------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = ((J .EQ. 1) .AND. (AMAXEXP/AIT .LE. FIVE))
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         IF (SFLAG) B = SQRT(EPS)
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
C------------------------------------------------------------------
C   Accuracy test is based on the multiplication theorem
C------------------------------------------------------------------
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
            T = U(0) * EPS
            W = (ONE-XLAM) * (ONE+XLAM)
            C = W *Y 
            DO 110 II = 2, 60
               T = XMB * T / C
               Z = U(II-1)
               IF (Z .LT. T) THEN
                     GO TO 120
                  ELSE IF (U(II-1) .GT. ONE) THEN
                     IF ((XMB/Y) .GT. (XMAX/U(II-1))) THEN
                           XL = XL + DEL
                           A = XL
                           GO TO  200
                     END IF
               END IF
               U(II) = XMB/Y * U(II-1) + U(II-2)
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
  120       SUM = U(MB)
            IND = MB
            DO 155 II = 1, MB
               IND = IND - 1
               SUM = SUM * W * Y / XMB + U(IND)
               XMB = XMB - ONE
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESK0(X)
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
C-----------------------------------------------------------------
C  Special tests
C-----------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031)
      Y = BESK0(XMIN)
      WRITE (IOUT,1032) Y
      Y = BESK0(ZERO)
      WRITE (IOUT,1033) 0,Y
      X = REN(JT) * ONENEG
      Y = BESK0(X)
      WRITE (IOUT,1034) X,Y
      Y = BESEK0(XMAX)
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
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      XLARGE = XB * (XNINE / EIGHT)
      Y = BESK0(XLARGE)
      WRITE (IOUT,1034) XLARGE,Y
      STOP
C----------------------------------------------------------------- 
 1000 FORMAT('1Test of K0(X) vs Multiplication Theorem'//)
 1010 FORMAT(I7,' random arguments were tested from the interval (',
     1    F5.1,',',F5.1,')'//)
 1011 FORMAT(' ABS(K0(X)) was larger',I6,' times,'/
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
 1032 FORMAT(' K0(XMIN) = ',E24.17/)
 1033 FORMAT(' K0(',I1,') = ',E24.17/)
 1034 FORMAT(' K0(',E24.17,' ) = ',E24.17/)
 1035 FORMAT(' E**X * K0(XMAX) = ',E24.17/)
C     ---------- Last line of BESK0 test program ----------
      END
      SUBROUTINE CALCK1(ARG,RESULT,JINT)
C--------------------------------------------------------------------
C
C This packet computes modified Bessel functions of the second kind
C   and order one,  K1(X)  and  EXP(X)*K1(X), for real arguments X.
C   It contains two function type subprograms, BESK1  and  BESEK1,
C   and one subroutine type subprogram, CALCK1.  The calling
C   statements for the primary entries are
C
C                   Y=BESK1(X)
C   and
C                   Y=BESEK1(X)
C
C   where the entry points correspond to the functions K1(X) and
C   EXP(X)*K1(X), respectively.  The routine CALCK1 is intended
C   for internal packet use only, all computations within the
C   packet being concentrated in this routine.  The function
C   subprograms invoke CALCK1 with the statement
C          CALL CALCK1(ARG,RESULT,JINT)
C   where the parameter usage is as follows
C
C      Function                      Parameters for CALCK1
C        Call             ARG                  RESULT          JINT
C
C     BESK1(ARG)  XLEAST .LT. ARG .LT. XMAX    K1(ARG)          1
C     BESEK1(ARG)     XLEAST .LT. ARG       EXP(ARG)*K1(ARG)    2
C
C   The main computation evaluates slightly modified forms of near 
C   minimax rational approximations generated by Russon and Blair, 
C   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461, 
C   1969.  This transportable program is patterned after the 
C   machine-dependent FUNPACK packet NATSK1, but cannot match that
C   version for efficiency or accuracy.  This version uses rational
C   functions that theoretically approximate K-SUB-1(X) to at
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
C   XLEAST = Smallest acceptable argument, i.e., smallest machine
C            number X such that 1/X is machine representable.
C   XSMALL = Argument below which BESK1(X) and BESEK1(X) may
C            each be represented by 1/X.  A safe value is the
C            largest X such that  1.0 + X = 1.0  to machine
C            precision.
C   XINF   = Largest positive machine number; approximately
C            beta**maxexp
C   XMAX   = Largest argument acceptable to BESK1;  Solution to
C            equation:  
C               W(X) * (1+3/8X-15/128X**2) = beta**minexp
C            where  W(X) = EXP(-X)*SQRT(PI/2X)
C
C
C     Approximate values for some important machines are:
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
C                         XLEAST     XSMALL      XINF       XMAX 
C
C CRAY-1                1.84E-2466  3.55E-15  5.45E+2465  5674.858 
C Cyber 180/855
C   under NOS   (S.P.)  3.14E-294   1.77E-15  1.26E+322    672.789
C IEEE (IBM/XT,
C   SUN, etc.)  (S.P.)  1.18E-38    5.95E-8   3.40E+38      85.343
C IEEE (IBM/XT,
C   SUN, etc.)  (D.P.)  2.23D-308   1.11D-16  1.79D+308    705.343
C IBM 3033      (D.P.)  1.39D-76    1.11D-16  7.23D+75     177.855
C VAX D-Format  (D.P.)  5.88D-39    6.95D-18  1.70D+38      86.721
C VAX G-Format  (D.P.)  1.12D-308   5.55D-17  8.98D+307    706.728
C
C*******************************************************************
C*******************************************************************
C
C Error returns
C
C  The program returns the value XINF for ARG .LE. 0.0 and the
C   BESK1 entry returns the value 0.0 for ARG .GT. XMAX.
C
C
C  Intrinsic functions required are:
C
C     LOG, SQRT, EXP
C
C
C  Authors: W. J. Cody and Laura Stoltz
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C  Latest modification: January 28, 1988
C
C--------------------------------------------------------------------
      INTEGER I,JINT
CS    REAL 
CD    DOUBLE PRECISION 
     1    ARG,F,G,ONE,P,PP,Q,QQ,RESULT,SUMF,SUMG,
     2    SUMP,SUMQ,X,XINF,XMAX,XLEAST,XSMALL,XX,ZERO
      DIMENSION P(5),Q(3),PP(11),QQ(9),F(5),G(3)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
CS    DATA ONE/1.0E0/,ZERO/0.0E0/
CD    DATA ONE/1.0D0/,ZERO/0.0D0/
C--------------------------------------------------------------------
C  Machine-dependent constants
C--------------------------------------------------------------------
CS    DATA XLEAST/1.18E-38/,XSMALL/5.95E-8/,XINF/3.40E+38/,
CS   1     XMAX/85.343E+0/
CD    DATA XLEAST/2.23D-308/,XSMALL/1.11D-16/,XINF/1.79D+308/,
CD   1     XMAX/705.343D+0/
C--------------------------------------------------------------------
C  Coefficients for  XLEAST .LE.  ARG  .LE. 1.0
C--------------------------------------------------------------------
CS    DATA   P/ 4.8127070456878442310E-1, 9.9991373567429309922E+1,
CS   1          7.1885382604084798576E+3, 1.7733324035147015630E+5,
CS   2          7.1938920065420586101E+5/
CS    DATA   Q/-2.8143915754538725829E+2, 3.7264298672067697862E+4,
CS   1         -2.2149374878243304548E+6/
CS    DATA   F/-2.2795590826955002390E-1,-5.3103913335180275253E+1,
CS   1         -4.5051623763436087023E+3,-1.4758069205414222471E+5,
CS   2         -1.3531161492785421328E+6/
CS    DATA   G/-3.0507151578787595807E+2, 4.3117653211351080007E+4,
CS   2         -2.7062322985570842656E+6/
CD    DATA   P/ 4.8127070456878442310D-1, 9.9991373567429309922D+1,
CD   1          7.1885382604084798576D+3, 1.7733324035147015630D+5,
CD   2          7.1938920065420586101D+5/
CD    DATA   Q/-2.8143915754538725829D+2, 3.7264298672067697862D+4,
CD   1         -2.2149374878243304548D+6/
CD    DATA   F/-2.2795590826955002390D-1,-5.3103913335180275253D+1,
CD   1         -4.5051623763436087023D+3,-1.4758069205414222471D+5,
CD   2         -1.3531161492785421328D+6/
CD    DATA   G/-3.0507151578787595807D+2, 4.3117653211351080007D+4,
CD   2         -2.7062322985570842656D+6/
C--------------------------------------------------------------------
C  Coefficients for  1.0 .LT.  ARG
C--------------------------------------------------------------------
CS    DATA  PP/ 6.4257745859173138767E-2, 7.5584584631176030810E+0,
CS   1          1.3182609918569941308E+2, 8.1094256146537402173E+2,
CS   2          2.3123742209168871550E+3, 3.4540675585544584407E+3,
CS   3          2.8590657697910288226E+3, 1.3319486433183221990E+3,
CS   4          3.4122953486801312910E+2, 4.4137176114230414036E+1,
CS   5          2.2196792496874548962E+0/
CS    DATA  QQ/ 3.6001069306861518855E+1, 3.3031020088765390854E+2,
CS   1          1.2082692316002348638E+3, 2.1181000487171943810E+3,
CS   2          1.9448440788918006154E+3, 9.6929165726802648634E+2,
CS   3          2.5951223655579051357E+2, 3.4552228452758912848E+1,
CS   4          1.7710478032601086579E+0/
CD    DATA  PP/ 6.4257745859173138767D-2, 7.5584584631176030810D+0,
CD   1          1.3182609918569941308D+2, 8.1094256146537402173D+2,
CD   2          2.3123742209168871550D+3, 3.4540675585544584407D+3,
CD   3          2.8590657697910288226D+3, 1.3319486433183221990D+3,
CD   4          3.4122953486801312910D+2, 4.4137176114230414036D+1,
CD   5          2.2196792496874548962D+0/
CD    DATA  QQ/ 3.6001069306861518855D+1, 3.3031020088765390854D+2,
CD   1          1.2082692316002348638D+3, 2.1181000487171943810D+3,
CD   2          1.9448440788918006154D+3, 9.6929165726802648634D+2,
CD   3          2.5951223655579051357D+2, 3.4552228452758912848D+1,
CD   4          1.7710478032601086579D+0/
C--------------------------------------------------------------------
      X = ARG
      IF (X .LT. XLEAST) THEN
C--------------------------------------------------------------------
C  Error return for  ARG  .LT. XLEAST
C--------------------------------------------------------------------
            RESULT = XINF
         ELSE IF (X .LE. ONE) THEN
C--------------------------------------------------------------------
C  XLEAST .LE.  ARG  .LE. 1.0
C--------------------------------------------------------------------
            IF (X .LT. XSMALL) THEN
C--------------------------------------------------------------------
C  Return for small ARG
C--------------------------------------------------------------------
                  RESULT = ONE / X
               ELSE
                  XX = X * X
                  SUMP = ((((P(1)*XX + P(2))*XX + P(3))*XX + P(4))*XX
     1                   + P(5))*XX + Q(3)
                  SUMQ = ((XX + Q(1))*XX + Q(2))*XX + Q(3)
                  SUMF = (((F(1)*XX + F(2))*XX + F(3))*XX + F(4))*XX
     1                   + F(5)
                  SUMG = ((XX + G(1))*XX + G(2))*XX + G(3)
                  RESULT = (XX * LOG(X) * SUMF/SUMG + SUMP/SUMQ) / X
                  IF (JINT .EQ. 2) RESULT = RESULT * EXP(X)
            END IF
         ELSE IF ((JINT .EQ. 1) .AND. (X .GT. XMAX)) THEN
C--------------------------------------------------------------------
C  Error return for  ARG  .GT. XMAX
C--------------------------------------------------------------------
            RESULT = ZERO
         ELSE
C--------------------------------------------------------------------
C  1.0 .LT.  ARG
C--------------------------------------------------------------------
            XX = ONE / X
            SUMP = PP(1)
            DO 120 I = 2, 11
               SUMP = SUMP * XX + PP(I)
  120       CONTINUE
            SUMQ = XX
            DO 140 I = 1, 8
               SUMQ = (SUMQ + QQ(I)) * XX
  140       CONTINUE
            SUMQ = SUMQ + QQ(9)
            RESULT = SUMP / SUMQ / SQRT(X)
            IF (JINT .EQ. 1) RESULT = RESULT * EXP(-X)
      END IF
      RETURN
C---------- Last line of CALCK1 ----------
      END
CS    REAL 
CD    DOUBLE PRECISION 
     1    FUNCTION BESK1(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order one
C   for arguments  XLEAST .LE. ARG .LE. XMAX.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL 
CD    DOUBLE PRECISION  
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 1
      CALL CALCK1(X,RESULT,JINT)
      BESK1 = RESULT
      RETURN
C---------- Last line of BESK1 ----------
      END
CS    REAL 
CD    DOUBLE PRECISION 
     1    FUNCTION BESEK1(X)
C--------------------------------------------------------------------
C
C This function program computes approximate values for the
C   modified Bessel function of the second kind of order one
C   multiplied by the exponential function, for arguments
C   XLEAST .LE. ARG .LE. XMAX.
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
CD    DOUBLE PRECISION  
     1    X, RESULT
C--------------------------------------------------------------------
      JINT = 2
      CALL CALCK1(X,RESULT,JINT)
      BESEK1 = RESULT
      RETURN
C---------- Last line of BESEK1 ----------
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
