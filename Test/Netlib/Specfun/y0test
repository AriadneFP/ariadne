C--------------------------------------------------------------------
C  Fortran 77 program to test BESY0
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
C  Intrinsic functions required are:
C
C         ABS, DBLE, LOG, MAX, REAL, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 27, 1990
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C--------------------------------------------------------------------
      LOGICAL SFLAG,TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
CD    DOUBLE PRECISION
     1   A,AIT,ALBETA,ALEPS,ALL9,B,BESY0,BESY1,BETA,C,
     2   CONV,DEL,EIGHT,EPS,EPSNEG,FIVE5,HALF,HUND,ONE,REN,
     3   R6,R7,SGN,SIXTEN,SUM,T,T1,THREE,TWENTY,TWO56,
     4   U,W,X,XI,XL,XLAM,XMAX,XMB,XMIN,X1,Y,YX,Z,ZERO,ZZ
      DIMENSION U(560),XI(3),YX(3)
CS    DATA ZERO,ONE,THREE,FIVE5/0.0E0,1.0E0,3.0E0,5.5E0/,
CS   1   EIGHT,TWENTY,ALL9,TWO56/8.0E0,20.0E0,-999.0E0,256.0E0/,
CS   2   HALF,SIXTEN,XLAM,HUND/0.5E0,16.0E0,0.9375E0,100.0D0/
CS    DATA XI/228.0E0,1013.0E0,1814.0E0/
CS    DATA YX(1)/-2.6003 14272 29334 87915 E-3/,
CS   1     YX(2)/ 2.6053 45491 14567 74983 E-4/,
CS   2     YX(3)/-3.4079 44871 47955 52077 E-5/
CD    DATA ZERO,ONE,THREE,FIVE5/0.0D0,1.0D0,3.0D0,5.5D0/,
CD   1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
CD   2   HALF,SIXTEN,XLAM,HUND/0.5D0,16.0D0,0.9375D0,100.0D0/
CD    DATA XI/228.0D0,1013.0D0,1814.0D0/
CD    DATA YX(1)/-2.6003 14272 29334 87915 D-3/,
CD   1     YX(2)/ 2.6053 45491 14567 74983 D-4/,
CD   2     YX(3)/-3.4079 44871 47955 52077 D-5/
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
      ALEPS = LOG(EPS)
      AIT = CONV(IT)
      JT = 0
      A = EPS
      B = THREE
      N = 2000
C--------------------------------------------------------------------
C  Random argument accuracy tests based on the multiplication theorem
C--------------------------------------------------------------------
      DO 300 J = 1, 4
         SFLAG = (J .EQ. 1) .AND. (MAXEXP/IT .LE. 5)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         SGN = ONE
         DEL = (B - A) / CONV(N)
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C--------------------------------------------------------------------
C  Carefully purify arguments
C--------------------------------------------------------------------
            Y = X/XLAM
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
C--------------------------------------------------------------------
C  Generate Bessel functions with forward recurrence
C--------------------------------------------------------------------
            U(1) = BESY0(Y)
            U(2) = BESY1(Y)
            TFLAG = SFLAG .AND. (Y .LT. HALF)
            IF (TFLAG) THEN
               U(1) = U(1) * EPS 
               U(2) = U(2) * EPS
            END IF
            MB = 1
            XMB = ONE
            Y = Y * HALF
            W = (ONE-XLAM)*(ONE+XLAM)
            C = W * Y
            T = ABS(U(1)+C*U(2))
            T1 = EPS/HUND
            DO 110 II = 3, 60
               Z = ABS(U(II-1))
               IF (Z/T1 .LT. T) GO TO 120
               IF (Y .LT. XMB) THEN
                  IF (Z .GT. XMAX*(Y/XMB)) THEN
                     A = X
                     XL = XL + DEL
                     GO TO  200
                  END IF
               END IF
               U(II) = XMB/Y * U(II-1) - U(II-2)
               IF (T1 .GT. ONE/EPS) THEN
                  T = T * T1
                  T1 = ONE
               END IF
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
  110       CONTINUE
C--------------------------------------------------------------------
C  Evaluate Bessel series expansion
C--------------------------------------------------------------------
  120       SUM = U(MB)
            IND = MB
            DO 155 II = 2, MB
               IND = IND - 1
               XMB = XMB - ONE
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM
            IF (TFLAG) ZZ = ZZ / EPS 
            Z = BESY0(X)
            Y = Z
            IF (ABS(U(1)) .GT. ABS(Y)) Y = U(1)
C--------------------------------------------------------------------
C  Accumulate results
C--------------------------------------------------------------------
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
C--------------------------------------------------------------------
C  Gather and print statistics
C--------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/CONV(N))
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         W = ALL9
         IF (R6 .NE. ZERO) W = LOG(ABS(R6))/ALBETA
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         W = ALL9
         IF (R7 .NE. ZERO) W = LOG(ABS(R7))/ALBETA
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1025) R7,IBETA,W
            ELSE
               WRITE (IOUT,1023) R7,IBETA,W
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
C--------------------------------------------------------------------
C  Initialize for next test
C--------------------------------------------------------------------
         A = B
         IF (J .EQ. 1) THEN
               B = FIVE5
               N = 2000
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
               N = 2000
            ELSE
               B = TWENTY
               N = 500
         END IF
  300 CONTINUE
C--------------------------------------------------------------------
C  Special tests
C--------------------------------------------------------------------
      WRITE (IOUT,1030)
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 3
         X = XI(I)/TWO56
         Y = BESY0(X)
         W = ALL9
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) W = LOG(ABS(T))/ALBETA
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1033)
      X = XMIN
      WRITE (IOUT,1035) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      X = ZERO
      WRITE (IOUT,1034) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESY0(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
 1000 FORMAT('1Test of Y0(X) VS Multiplication Theorem'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(Y0(X)) was larger',I6,' times', /
     1   15X,' agreed',I6,' times, and'/
     1   11X,'was smaller',I6,' times.'//)
 1020 FORMAT(' There are',I4,' base',I4,
     1    ' significant digits in a floating-point number.' //)
 1021 FORMAT(' The maximum relative error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1022 FORMAT(' The estimated loss of base',I4,
     1  ' significant digits is',F7.2//)
 1023 FORMAT(' The root mean square relative error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1024 FORMAT(' The maximum absolute error of',E15.4,3H = ,I4,3H **,
     1  F7.2/4X,'occurred for X =',E13.6)
 1025 FORMAT(' The root mean square absolute error was',E15.4,
     1    3H = ,I4,3H **,F7.2)
 1030 FORMAT('1Special Tests'//)
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESY0(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'/)
 1034 FORMAT(' Y0 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1035 FORMAT(' Y0 will be called with the argument ',E17.10/
     1     ' This should not stop execution.'//)
 1036 FORMAT(' Y0 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of BESY0 test program ----------
      END
      SUBROUTINE CALJY1(ARG,RESULT,JINT)
C---------------------------------------------------------------------
C
C This packet computes first-order Bessel functions of the first and
C   second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX
C   for Y1, and |X| <= XMAX for J1.  It contains two function-type
C   subprograms,  BESJ1  and  BESY1,  and one subroutine-type
C   subprogram,  CALJY1.  The calling statements for the primary
C   entries are:
C
C           Y = BESJ1(X)
C   and
C           Y = BESY1(X),
C
C   where the entry points correspond to the functions J1(X) and Y1(X),
C   respectively.  The routine  CALJY1  is intended for internal packet
C   use only, all computations within the packet being concentrated in
C   this one routine.  The function subprograms invoke  CALJY1  with
C   the statement
C           CALL CALJY1(ARG,RESULT,JINT),
C   where the parameter usage is as follows:
C
C      Function                  Parameters for CALJY1
C       call              ARG             RESULT          JINT
C
C     BESJ1(ARG)     |ARG| .LE. XMAX       J1(ARG)          0
C     BESY1(ARG)   0 .LT. ARG .LE. XMAX    Y1(ARG)          1
C
C   The main computation uses unpublished minimax rational
C   approximations for X .LE. 8.0, and an approximation from the 
C   book  Computer Approximations  by Hart, et. al., Wiley and Sons, 
C   New York, 1968, for arguments larger than 8.0   Part of this
C   transportable packet is patterned after the machine-dependent
C   FUNPACK program BESJ1(X), but cannot match that version for
C   efficiency or accuracy.  This version uses rational functions
C   that are theoretically accurate to at least 18 significant decimal
C   digits for X <= 8, and at least 18 decimal places for X > 8.  The
C   accuracy achieved depends on the arithmetic system, the compiler,
C   the intrinsic functions, and proper selection of the machine-
C   dependent constants.
C
C*******************************************************************
C
C Explanation of machine-dependent constants
C
C   XINF   = largest positive machine number
C   XMAX   = largest acceptable argument.  The functions AINT, SIN
C            and COS must perform properly for  ABS(X) .LE. XMAX.
C            We recommend that XMAX be a small integer multiple of
C            sqrt(1/eps), where eps is the smallest positive number
C            such that  1+eps > 1. 
C   XSMALL = positive argument such that  1.0-(1/2)(X/2)**2 = 1.0
C            to machine precision for all  ABS(X) .LE. XSMALL.
C            We recommend that  XSMALL < sqrt(eps)/beta, where beta
C            is the floating-point radix (usually 2 or 16).
C
C     Approximate values for some important machines are
C
C                          eps      XMAX     XSMALL      XINF  
C
C  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322
C  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465
C  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38
C  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308
C  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75
C  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307
C  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38
C
C*******************************************************************
C*******************************************************************
C
C Error Returns
C
C  The program returns the value zero for  X .GT. XMAX, and returns
C    -XINF when BESLY1 is called with a negative or zero argument.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, COS, LOG, SIN, SQRT
C
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C  Latest modification: November 10, 1987
C
C--------------------------------------------------------------------
      INTEGER I,JINT
      DIMENSION PJ0(7),PJ1(8),PLG(4),PY0(7),PY1(9),P0(6),P1(6),
     1          QJ0(5),QJ1(7),QLG(4),QY0(6),QY1(8),Q0(6),Q1(6)
CS    REAL
CD    DOUBLE PRECISION
     1   ARG,AX,DOWN,EIGHT,FOUR,HALF,PI2,PJ0,PJ1,PLG,PROD,PY0,
     2   PY1,P0,P1,P17,QJ0,QJ1,QLG,QY0,QY1,Q0,Q1,RESJ,RESULT,
     3   RTPI2,R0,R1,THROV8,TWOPI,TWOPI1,TWOPI2,TWO56,UP,W,WSQ,
     4   XDEN,XINF,XMAX,XNUM,XSMALL,XJ0,XJ1,XJ01,XJ02,XJ11,XJ12,
     5   XY,XY0,XY01,XY02,XY1,XY11,XY12,Z,ZERO,ZSQ
C-------------------------------------------------------------------
C  Mathematical constants
C-------------------------------------------------------------------
CS    DATA EIGHT/8.0E0/,
CS   1     FOUR/4.0E0/,HALF/0.5E0/,THROV8/0.375E0/,
CS   2     PI2/6.3661977236758134308E-1/,P17/1.716E-1/
CS   3     TWOPI/6.2831853071795864769E+0/,ZERO/0.0E0/,
CS   4     TWOPI1/6.28125E0/,TWOPI2/1.9353071795864769253E-03/
CS   5     TWO56/256.0E+0/,RTPI2/7.9788456080286535588E-1/
CD    DATA EIGHT/8.0D0/,
CD   1     FOUR/4.0D0/,HALF/0.5D0/,THROV8/0.375D0/,
CD   2     PI2/6.3661977236758134308D-1/,P17/1.716D-1/
CD   3     TWOPI/6.2831853071795864769D+0/,ZERO/0.0D0/,
CD   4     TWOPI1/6.28125D0/,TWOPI2/1.9353071795864769253D-03/
CD   5     TWO56/256.0D+0/,RTPI2/7.9788456080286535588D-1/
C-------------------------------------------------------------------
C  Machine-dependent constants
C-------------------------------------------------------------------
CS    DATA XMAX/8.19E+03/,XSMALL/1.22E-09/,XINF/1.7E+38/
CD    DATA XMAX/1.07D+09/,XSMALL/9.31D-10/,XINF/1.7D+38/
C-------------------------------------------------------------------
C  Zeroes of Bessel functions
C-------------------------------------------------------------------
CS    DATA XJ0/3.8317059702075123156E+0/,XJ1/7.0155866698156187535E+0/,
CS   1     XY0/2.1971413260310170351E+0/,XY1/5.4296810407941351328E+0/,
CS   2     XJ01/ 981.0E+0/, XJ02/-3.2527979248768438556E-04/,
CS   3     XJ11/1796.0E+0/, XJ12/-3.8330184381246462950E-05/,
CS   4     XY01/ 562.0E+0/, XY02/ 1.8288260310170351490E-03/,
CS   5     XY11/1390.0E+0/, XY12/-6.4592058648672279948E-06/
CD    DATA XJ0/3.8317059702075123156D+0/,XJ1/7.0155866698156187535D+0/,
CD   1     XY0/2.1971413260310170351D+0/,XY1/5.4296810407941351328D+0/,
CD   2     XJ01/ 981.0D+0/, XJ02/-3.2527979248768438556D-04/,
CD   3     XJ11/1796.0D+0/, XJ12/-3.8330184381246462950D-05/,
CD   4     XY01/ 562.0D+0/, XY02/ 1.8288260310170351490D-03/,
CD   5     XY11/1390.0D+0/, XY12/-6.4592058648672279948D-06/
C-------------------------------------------------------------------
C  Coefficients for rational approximation to ln(x/a)
C--------------------------------------------------------------------
CS    DATA PLG/-2.4562334077563243311E+01,2.3642701335621505212E+02,
CS   1         -5.4989956895857911039E+02,3.5687548468071500413E+02/
CS    DATA QLG/-3.5553900764052419184E+01,1.9400230218539473193E+02,
CS   1         -3.3442903192607538956E+02,1.7843774234035750207E+02/
CD    DATA PLG/-2.4562334077563243311D+01,2.3642701335621505212D+02,
CD   1         -5.4989956895857911039D+02,3.5687548468071500413D+02/
CD    DATA QLG/-3.5553900764052419184D+01,1.9400230218539473193D+02,
CD   1         -3.3442903192607538956D+02,1.7843774234035750207D+02/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C  J1(X) / (X * (X**2 - XJ0**2)),  XSMALL  <  |X|  <=  4.0
C--------------------------------------------------------------------
CS    DATA PJ0/9.8062904098958257677E+05,-1.1548696764841276794E+08,
CS   1       6.6781041261492395835E+09,-1.4258509801366645672E+11,
CS   2      -4.4615792982775076130E+03, 1.0650724020080236441E+01,
CS   3      -1.0767857011487300348E-02/
CS    DATA QJ0/5.9117614494174794095E+05, 2.0228375140097033958E+08,
CS   1       4.2091902282580133541E+10, 4.1868604460820175290E+12,
CS   2       1.0742272239517380498E+03/
CD    DATA PJ0/9.8062904098958257677D+05,-1.1548696764841276794D+08,
CD   1       6.6781041261492395835D+09,-1.4258509801366645672D+11,
CD   2      -4.4615792982775076130D+03, 1.0650724020080236441D+01,
CD   3      -1.0767857011487300348D-02/
CD    DATA QJ0/5.9117614494174794095D+05, 2.0228375140097033958D+08,
CD   1       4.2091902282580133541D+10, 4.1868604460820175290D+12,
CD   2       1.0742272239517380498D+03/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C  J1(X) / (X * (X**2 - XJ1**2)),  4.0  <  |X|  <=  8.0
C-------------------------------------------------------------------
CS    DATA PJ1/4.6179191852758252280E+00,-7.1329006872560947377E+03,
CS   1       4.5039658105749078904E+06,-1.4437717718363239107E+09,
CS   2       2.3569285397217157313E+11,-1.6324168293282543629E+13,
CS   3       1.1357022719979468624E+14, 1.0051899717115285432E+15/
CS    DATA QJ1/1.1267125065029138050E+06, 6.4872502899596389593E+08,
CS   1       2.7622777286244082666E+11, 8.4899346165481429307E+13,
CS   2       1.7128800897135812012E+16, 1.7253905888447681194E+18,
CS   3       1.3886978985861357615E+03/
CD    DATA PJ1/4.6179191852758252280D+00,-7.1329006872560947377D+03,
CD   1       4.5039658105749078904D+06,-1.4437717718363239107D+09,
CD   2       2.3569285397217157313D+11,-1.6324168293282543629D+13,
CD   3       1.1357022719979468624D+14, 1.0051899717115285432D+15/
CD    DATA QJ1/1.1267125065029138050D+06, 6.4872502899596389593D+08,
CD   1       2.7622777286244082666D+11, 8.4899346165481429307D+13,
CD   2       1.7128800897135812012D+16, 1.7253905888447681194D+18,
CD   3       1.3886978985861357615D+03/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y1(X) - 2 LN(X/XY0) J1(X)) / (X**2 - XY0**2),
C        XSMALL  <  |X|  <=  4.0
C--------------------------------------------------------------------
CS    DATA PY0/2.2157953222280260820E+05,-5.9157479997408395984E+07,
CS   1         7.2144548214502560419E+09,-3.7595974497819597599E+11,
CS   2         5.4708611716525426053E+12, 4.0535726612579544093E+13,
CS   3        -3.1714424660046133456E+02/
CS    DATA QY0/8.2079908168393867438E+02, 3.8136470753052572164E+05,
CS   1         1.2250435122182963220E+08, 2.7800352738690585613E+10,
CS   2         4.1272286200406461981E+12, 3.0737873921079286084E+14/
CD    DATA PY0/2.2157953222280260820D+05,-5.9157479997408395984D+07,
CD   1         7.2144548214502560419D+09,-3.7595974497819597599D+11,
CD   2         5.4708611716525426053D+12, 4.0535726612579544093D+13,
CD   3        -3.1714424660046133456D+02/
CD    DATA QY0/8.2079908168393867438D+02, 3.8136470753052572164D+05,
CD   1         1.2250435122182963220D+08, 2.7800352738690585613D+10,
CD   2         4.1272286200406461981D+12, 3.0737873921079286084D+14/
C--------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y1(X) - 2 LN(X/XY1) J1(X)) / (X**2 - XY1**2),
C        4.0  <  |X|  <=  8.0
C--------------------------------------------------------------------
CS    DATA PY1/ 1.9153806858264202986E+06,-1.1957961912070617006E+09,
CS   1          3.7453673962438488783E+11,-5.9530713129741981618E+13,
CS   2          4.0686275289804744814E+15,-2.3638408497043134724E+16,
CS   3         -5.6808094574724204577E+18, 1.1514276357909013326E+19,
CS   4         -1.2337180442012953128E+03/
CS    DATA QY1/ 1.2855164849321609336E+03, 1.0453748201934079734E+06,
CS   1          6.3550318087088919566E+08, 3.0221766852960403645E+11,
CS   2          1.1187010065856971027E+14, 3.0837179548112881950E+16,
CS   3          5.6968198822857178911E+18, 5.3321844313316185697E+20/
CD    DATA PY1/ 1.9153806858264202986D+06,-1.1957961912070617006D+09,
CD   1          3.7453673962438488783D+11,-5.9530713129741981618D+13,
CD   2          4.0686275289804744814D+15,-2.3638408497043134724D+16,
CD   3         -5.6808094574724204577D+18, 1.1514276357909013326D+19,
CD   4         -1.2337180442012953128D+03/
CD    DATA QY1/ 1.2855164849321609336D+03, 1.0453748201934079734D+06,
CD   1          6.3550318087088919566D+08, 3.0221766852960403645D+11,
CD   2          1.1187010065856971027D+14, 3.0837179548112881950D+16,
CD   3          5.6968198822857178911D+18, 5.3321844313316185697D+20/
C-------------------------------------------------------------------
C  Coefficients for Hart,s approximation,  |X| > 8.0
C-------------------------------------------------------------------
CS    DATA P0/-1.0982405543459346727E+05,-1.5235293511811373833E+06,
CS   1         -6.6033732483649391093E+06,-9.9422465050776411957E+06,
CS   2         -4.4357578167941278571E+06,-1.6116166443246101165E+03/
CS    DATA Q0/-1.0726385991103820119E+05,-1.5118095066341608816E+06,
CS   1         -6.5853394797230870728E+06,-9.9341243899345856590E+06,
CS   2         -4.4357578167941278568E+06,-1.4550094401904961825E+03/
CS    DATA P1/ 1.7063754290207680021E+03, 1.8494262873223866797E+04,
CS   1          6.6178836581270835179E+04, 8.5145160675335701966E+04,
CS   2          3.3220913409857223519E+04, 3.5265133846636032186E+01/
CS    DATA Q1/ 3.7890229745772202641E+04, 4.0029443582266975117E+05,
CS   1          1.4194606696037208929E+06, 1.8194580422439972989E+06,
CS   2          7.0871281941028743574E+05, 8.6383677696049909675E+02/
CD    DATA P0/-1.0982405543459346727D+05,-1.5235293511811373833D+06,
CD   1         -6.6033732483649391093D+06,-9.9422465050776411957D+06,
CD   2         -4.4357578167941278571D+06,-1.6116166443246101165D+03/
CD    DATA Q0/-1.0726385991103820119D+05,-1.5118095066341608816D+06,
CD   1         -6.5853394797230870728D+06,-9.9341243899345856590D+06,
CD   2         -4.4357578167941278568D+06,-1.4550094401904961825D+03/
CD    DATA P1/ 1.7063754290207680021D+03, 1.8494262873223866797D+04,
CD   1          6.6178836581270835179D+04, 8.5145160675335701966D+04,
CD   2          3.3220913409857223519D+04, 3.5265133846636032186D+01/
CD    DATA Q1/ 3.7890229745772202641D+04, 4.0029443582266975117D+05,
CD   1          1.4194606696037208929D+06, 1.8194580422439972989D+06,
CD   2          7.0871281941028743574D+05, 8.6383677696049909675D+02/
C-------------------------------------------------------------------
C  Check for error conditions
C-------------------------------------------------------------------
      AX = ABS(ARG)
      IF ((JINT .EQ. 1) .AND. ((ARG .LE. ZERO) .OR.
     1   ((ARG .LT. HALF) .AND. (AX*XINF .LT. PI2)))) THEN
            RESULT = -XINF
            GO TO 2000
         ELSE IF (AX .GT. XMAX) THEN
            RESULT = ZERO
            GO TO 2000
      END IF
      IF (AX .GT. EIGHT) THEN
            GO TO 800
         ELSE IF (AX .LE. XSMALL) THEN
            IF (JINT .EQ. 0) THEN
                  RESULT = ARG * HALF
               ELSE
                  RESULT = -PI2 / AX
            END IF
            GO TO 2000
      END IF
C-------------------------------------------------------------------
C  Calculate J1 for appropriate interval, preserving
C     accuracy near the zero of J1
C-------------------------------------------------------------------
      ZSQ = AX * AX
      IF (AX .LE. FOUR) THEN
            XNUM = (PJ0(7) * ZSQ + PJ0(6)) * ZSQ + PJ0(5)
            XDEN = ZSQ + QJ0(5)
            DO 50 I = 1, 4
               XNUM = XNUM * ZSQ + PJ0(I)
               XDEN = XDEN * ZSQ + QJ0(I)
   50       CONTINUE
            PROD = ARG * ((AX - XJ01/TWO56) - XJ02) * (AX + XJ0)
         ELSE
            XNUM = PJ1(1)
            XDEN = (ZSQ + QJ1(7)) * ZSQ + QJ1(1)
            DO 220 I = 2, 6
               XNUM = XNUM * ZSQ + PJ1(I)
               XDEN = XDEN * ZSQ + QJ1(I)
  220       CONTINUE
            XNUM = XNUM * (AX - EIGHT) * (AX + EIGHT) + PJ1(7)
            XNUM = XNUM * (AX - FOUR) * (AX + FOUR) + PJ1(8)
            PROD = ARG * ((AX - XJ11/TWO56) - XJ12) * (AX + XJ1)
      END IF
      RESULT = PROD * (XNUM / XDEN)
      IF (JINT .EQ. 0) GO TO 2000
C-------------------------------------------------------------------
C  Calculate Y1.  First find  RESJ = pi/2 ln(x/xn) J1(x),
C    where xn is a zero of Y1
C-------------------------------------------------------------------
      IF (AX .LE. FOUR) THEN
            UP = (AX-XY01/TWO56)-XY02
            XY = XY0
         ELSE
            UP = (AX-XY11/TWO56)-XY12
            XY = XY1
      END IF
      DOWN = AX + XY
      IF (ABS(UP) .LT. P17*DOWN) THEN
            W = UP/DOWN
            WSQ = W*W
            XNUM = PLG(1)
            XDEN = WSQ + QLG(1)
            DO 320 I = 2, 4
               XNUM = XNUM*WSQ + PLG(I)
               XDEN = XDEN*WSQ + QLG(I)
  320       CONTINUE
            RESJ = PI2 * RESULT * W * XNUM/XDEN
         ELSE
            RESJ = PI2 * RESULT * LOG(AX/XY)
      END IF
C-------------------------------------------------------------------
C  Now calculate Y1 for appropriate interval, preserving
C     accuracy near the zero of Y1
C-------------------------------------------------------------------
      IF (AX .LE. FOUR) THEN
            XNUM = PY0(7) * ZSQ + PY0(1)
            XDEN = ZSQ + QY0(1)
            DO 340 I = 2, 6
               XNUM = XNUM * ZSQ + PY0(I)
               XDEN = XDEN * ZSQ + QY0(I)
  340       CONTINUE
         ELSE
            XNUM = PY1(9) * ZSQ + PY1(1)
            XDEN = ZSQ + QY1(1)
            DO 360 I = 2, 8
               XNUM = XNUM * ZSQ + PY1(I)
               XDEN = XDEN * ZSQ + QY1(I)
  360       CONTINUE
      END IF
      RESULT = RESJ + (UP*DOWN/AX) * XNUM / XDEN
      GO TO 2000
C-------------------------------------------------------------------
C  Calculate J1 or Y1 for |ARG|  >  8.0
C-------------------------------------------------------------------
  800 Z = EIGHT / AX
      W = AINT(AX/TWOPI) + THROV8
      W = (AX - W * TWOPI1) - W * TWOPI2
      ZSQ = Z * Z
      XNUM = P0(6)
      XDEN = ZSQ + Q0(6)
      UP = P1(6)
      DOWN = ZSQ + Q1(6)
      DO 850 I = 1, 5
         XNUM = XNUM * ZSQ + P0(I)
         XDEN = XDEN * ZSQ + Q0(I)
         UP = UP * ZSQ + P1(I)
         DOWN = DOWN * ZSQ + Q1(I)
  850 CONTINUE
      R0 = XNUM / XDEN
      R1 = UP / DOWN
      IF (JINT .EQ. 0) THEN
            RESULT = (RTPI2/SQRT(AX)) * (R0*COS(W) - Z*R1*SIN(W))
         ELSE
            RESULT = (RTPI2/SQRT(AX)) * (R0*SIN(W) + Z*R1*COS(W))
      END IF
      IF ((JINT .EQ. 0) .AND. (ARG .LT. ZERO)) RESULT = -RESULT
 2000 RETURN
C---------- Last card of CALJY1 ----------
      END
      FUNCTION BESJ1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the first kind of order zero for arguments  |X| <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
CD    DOUBLE PRECISION
     1   BESJ1,RESULT,X
C--------------------------------------------------------------------
      JINT=0
      CALL CALJY1(X,RESULT,JINT)
      BESJ1 = RESULT
      RETURN
C---------- Last card of BESJ1 ----------
      END
      FUNCTION BESY1(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the second kind of order zero for arguments 0 < X <= XMAX
C   (see comments heading CALJY1).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL
CD    DOUBLE PRECISION
     1   BESY1,RESULT,X
C--------------------------------------------------------------------
      JINT=1
      CALL CALJY1(X,RESULT,JINT)
      BESY1 = RESULT
      RETURN
C---------- Last card of BESY1 ----------
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
