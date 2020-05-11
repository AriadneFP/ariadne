C--------------------------------------------------------------------
C  Fortran 77 program to test BESY1
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
C              be deleted provided the following six
C              parameters are assigned the values indicated
C
C                 IBETA  - the radix of the floating-point system
C                 IT     - the number of base-IBETA digits in the
C                          significand of a floating-point number
C                 MAXEXP - the smallest integer such that 
C                          FLOAT(IBETA)**MAXEXP causes overflow
C                 EPS    - the smallest positive floating-point
C                          number such that 1.0+EPS .NE. 1.0
C                 XMIN   - the smallest positive normalized
C                          floating-point power of the radix
C                 XMAX   - the largest finite floating-point
C                          number
C
C     REN(K) - a function subprogram returning random real
C              numbers uniformly distributed over (0,1)
C
C
C  Intrinsic functions required are:
C
C         ABS, DBLE, FLOAT, LOG, MAX, SIGN, SQRT
C
C  Reference: "Performance evaluation of programs for certain
C              Bessel functions", W. J. Cody and L. Stoltz,
C              ACM Trans. on Math. Software, Vol. 15, 1989,
C              pp 41-48.
C
C  Latest modification: March 27, 1990
C
C  Authors: George Zazi, W. J. Cody
C           Mathematics and Computer Science Division
C           Argonne National Laboratory
C           Argonne, IL 60439
C
C--------------------------------------------------------------------
      LOGICAL SFLAG, TFLAG
      INTEGER I,IBETA,IEXP,II,IND,IOUT,IRND,IT,J,JT,K1,K2,K3,
     1        MACHEP,MAXEXP,MB,MBM1,MBP1,MINEXP,N,NDUM,NEGEP,NGRD
CS    REAL
CD    DOUBLE PRECISION
     1   A,AIT,ALBETA,ALEPS,ALL9,B,BESY0,BESY1,BETA,C,CONV,
     2   DEL,EIGHT,EPS,EPSNEG,FOUR,HALF,HUND,ONE,REN,R6,R7,
     3   SGN,SIXTEN,SUM,T,TWENTY,TWO56,T1,U,W,X,XI,XL,
     4   XLAM,XMAX,XMB,XMIN,X1,Y,YX,Z,ZERO,ZZ
      DIMENSION U(560),XI(2),YX(2)
C--------------------------------------------------------------------
C  Mathematical constants
C--------------------------------------------------------------------
      DATA IOUT/6/
CS    DATA ZERO,ONE,FOUR/0.0E0,1.0E0,4.0E0/,
CS   1   EIGHT,TWENTY,ALL9,TWO56/8.0E0,20.0E0,-999.0E0,256.0E0/,
CS   2   HALF,SIXTEN,XLAM,HUND/0.5E0,16.0E0,0.9375E0,100.0E0/
CD    DATA ZERO,ONE,FOUR/0.0D0,1.0D0,4.0E0/,
CD   1   EIGHT,TWENTY,ALL9,TWO56/8.0D0,20.0D0,-999.0D0,256.0D0/,
CD   2   HALF,SIXTEN,XLAM,HUND/0.5D0,16.0D0,0.9375D0,100.0D0/
C--------------------------------------------------------------------
C  Zeroes of Y1
C--------------------------------------------------------------------
CS    DATA XI/562.0E0,1390.0E0/
CS    DATA YX(1)/-9.5282 39309 77227 03132 E-4/
CS   1     YX(2)/-2.1981 83008 05980 02741 E-6/
CD    DATA XI/562.0D0,1390.0D0/
CD    DATA YX(1)/-9.5282 39309 77227 03132 D-4/,
CD   1     YX(2)/-2.1981 83008 05980 02741 D-6/
C--------------------------------------------------------------------
C  Statement functions for conversion between integer and float
C--------------------------------------------------------------------
CS    CONV(NDUM) = FLOAT(NDUM)
CD    CONV(NDUM) = DBLE(FLOAT(NDUM))
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
      B = EPS
C--------------------------------------------------------------------
C  Random argument accuracy tests (based on the
C    multiplication theorem)
C--------------------------------------------------------------------
      DO 300 J = 1, 3
         SFLAG = (J .EQ. 1) .AND. (MAXEXP/IT .LE. 5)
         K1 = 0
         K2 = 0
         K3 = 0
         X1 = ZERO
         R6 = ZERO
         R7 = ZERO
         N = 2000
         A = B
         IF (J .EQ. 1) THEN
               B = FOUR
            ELSE IF (J .EQ. 2) THEN
               B = EIGHT
            ELSE
               B = TWENTY
               N = 500
         END IF
         SGN = ONE
         DEL = (B - A) / CONV(N)
         XL = A
         DO 200 I = 1, N
            X = DEL * REN(JT) + XL
C--------------------------------------------------------------------
C  Carefully purify arguments and evaluate identities
C--------------------------------------------------------------------
            Y = X/XLAM
            W = SIXTEN * Y
            Y = (W + Y) - W
            X = Y * XLAM
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
            T = ABS(XLAM*U(2))
            T1 = EPS/HUND
            DO 110 II = 3, 60
               U(II) = XMB/Y * U(II-1) - U(II-2)
               T1 = XMB * T1 / C
               XMB = XMB + ONE
               MB = MB + 1
               MBP1=MB + 1
               Z = ABS(U(II))
               IF (Z/T1 .LT. T) THEN
                     GO TO 120
                  ELSE IF (Y .LT. XMB) THEN
                     IF (Z .GT. XMAX*(Y/XMB)) THEN
                        A= X
                        XL=XL+DEL
                        GO TO  200
                     END IF
               END IF
               IF (T1 .GT. ONE/EPS) THEN
                  T = T * T1
                  T1 = ONE
               END IF
  110       CONTINUE
  120       SUM = U(MBP1)
            IND = MBP1
            MBM1 =MB-1
            DO 155 II = 1, MBM1
               IND = IND - 1
               XMB = XMB - ONE
               SUM = SUM * W * Y / XMB + U(IND)
  155       CONTINUE
            ZZ = SUM*XLAM
            IF (TFLAG) ZZ = ZZ / EPS
            Z = BESY1(X)
            Y = Z
            IF (ABS(U(2)) .GT. ABS(Y)) Y =  U(2)
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
C  Process and output statistics
C--------------------------------------------------------------------
         N = K1 + K2 + K3
         R7 = SQRT(R7/CONV(N))
         WRITE (IOUT,1000)
         WRITE (IOUT,1010) N,A,B
         WRITE (IOUT,1011) K1,K2,K3
         WRITE (IOUT,1020) IT,IBETA
         IF (R6 .NE. ZERO) THEN
               W = LOG(ABS(R6))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
               WRITE (IOUT,1024) R6,IBETA,W,X1
            ELSE
               WRITE (IOUT,1021) R6,IBETA,W,X1
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1022) IBETA,W
         IF (R7 .NE. ZERO) THEN
               W = LOG(ABS(R7))/ALBETA
            ELSE
               W = ALL9
         END IF
         IF (J .EQ. 4) THEN
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
      WRITE (IOUT,1031) IBETA
      DO 330 I = 1, 2  
         X = XI(I)/TWO56
         Y = BESY1(X)
         T = (Y-YX(I))/YX(I)
         IF (T .NE. ZERO) THEN
               W = LOG(ABS(T))/ALBETA
            ELSE
               W = ALL9
         END IF
         W = MAX(AIT+W,ZERO)
         WRITE (IOUT,1032) X,Y,W
  330 CONTINUE
C--------------------------------------------------------------------
C  Test of error returns
C--------------------------------------------------------------------
      WRITE (IOUT,1033)
      X = XMIN
      IF (XMIN*XMAX .LT. ONE) THEN
            WRITE (IOUT,1034) X
         ELSE
            WRITE (IOUT,1035) X
      END IF
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = -ONE
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      X = XMAX
      WRITE (IOUT,1034) X
      Y = BESY1(X)
      WRITE (IOUT,1036) Y
      WRITE (IOUT,1100)
      STOP
C--------------------------------------------------------------------
 1000 FORMAT('1Test of Y1(X) VS Multiplication Theorem'  //)
 1010 FORMAT(I7,' random arguments were tested from the interval ',
     1 1H(,F5.1,1H,,F5.1,1H)//)
 1011 FORMAT(' ABS(Y1(X)) was larger',I6,' times', /
     1     15X,' agreed',I6,' times, and'/
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
 1031 FORMAT(' Accuracy near zeros'//10X,'X',15X,'BESY1(X)',
     1    13X,'Loss of base',I3,' digits'/)
 1032 FORMAT(E20.10,E25.15,8X,F7.2/)
 1033 FORMAT(//' Test with extreme arguments'///)
 1034 FORMAT(' Y1 will be called with the argument ',E17.10/
     1     ' This may stop execution.'//)
 1035 FORMAT(' Y1 will be called with the argument ',E17.10/
     1     ' This should not stop execution.'//)
 1036 FORMAT(' Y1 returned the value',E25.17/)
 1100 FORMAT(' This concludes the tests.')
C---------- Last card of BESY1 test program ----------
      END
      SUBROUTINE CALJY0(ARG,RESULT,JINT)
C---------------------------------------------------------------------
C
C This packet computes zero-order Bessel functions of the first and
C   second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
C   for Y0, and |X| <= XMAX for J0.  It contains two function-type
C   subprograms,  BESJ0  and  BESY0,  and one subroutine-type
C   subprogram,  CALJY0.  The calling statements for the primary
C   entries are:
C
C           Y = BESJ0(X)
C   and
C           Y = BESY0(X),
C
C   where the entry points correspond to the functions J0(X) and Y0(X),
C   respectively.  The routine  CALJY0  is intended for internal packet
C   use only, all computations within the packet being concentrated in
C   this one routine.  The function subprograms invoke  CALJY0  with
C   the statement
C           CALL CALJY0(ARG,RESULT,JINT),
C   where the parameter usage is as follows:
C
C      Function                  Parameters for CALJY0
C       call              ARG             RESULT          JINT
C
C     BESJ0(ARG)     |ARG| .LE. XMAX       J0(ARG)          0
C     BESY0(ARG)   0 .LT. ARG .LE. XMAX    Y0(ARG)          1
C
C   The main computation uses unpublished minimax rational
C   approximations for X .LE. 8.0, and an approximation from the 
C   book  Computer Approximations  by Hart, et. al., Wiley and Sons, 
C   New York, 1968, for arguments larger than 8.0   Part of this
C   transportable packet is patterned after the machine-dependent
C   FUNPACK program BESJ0(X), but cannot match that version for
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
C   XSMALL = positive argument such that  1.0-(X/2)**2 = 1.0
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
C    -XINF when BESLY0 is called with a negative or zero argument.
C
C
C Intrinsic functions required are:
C
C     ABS, AINT, COS, LOG, SIN, SQRT
C
C
C  Latest modification: June 2, 1989
C
C  Author: W. J. Cody
C          Mathematics and Computer Science Division 
C          Argonne National Laboratory
C          Argonne, IL 60439
C
C--------------------------------------------------------------------
      INTEGER I,JINT
CS    REAL
CD    DOUBLE PRECISION
     1       ARG,AX,CONS,DOWN,EIGHT,FIVE5,FOUR,ONE,ONEOV8,PI2,PJ0,
     2       PJ1,PLG,PROD,PY0,PY1,PY2,P0,P1,P17,QJ0,QJ1,QLG,QY0,QY1,
     3       QY2,Q0,Q1,RESJ,RESULT,R0,R1,SIXTY4,THREE,TWOPI,TWOPI1,
     4       TWOPI2,TWO56,UP,W,WSQ,XDEN,XINF,XMAX,XNUM,XSMALL,XJ0,
     5       XJ1,XJ01,XJ02,XJ11,XJ12,XY,XY0,XY01,XY02,XY1,XY11,XY12,
     6       XY2,XY21,XY22,Z,ZERO,ZSQ
      DIMENSION PJ0(7),PJ1(8),PLG(4),PY0(6),PY1(7),PY2(8),P0(6),P1(6),
     1          QJ0(5),QJ1(7),QLG(4),QY0(5),QY1(6),QY2(7),Q0(5),Q1(5)
C-------------------------------------------------------------------
C  Mathematical constants
C    CONS = ln(.5) + Euler's gamma
C-------------------------------------------------------------------
CS    DATA ZERO,ONE,THREE,FOUR,EIGHT/0.0E0,1.0E0,3.0E0,4.0E0,8.0E0/,
CS   1     FIVE5,SIXTY4,ONEOV8,P17/5.5E0,64.0E0,0.125E0,1.716E-1/,
CS   2     TWO56,CONS/256.0E0,-1.1593151565841244881E-1/,
CS   3     PI2,TWOPI/6.3661977236758134308E-1,6.2831853071795864769E0/,
CS   4     TWOPI1,TWOPI2/6.28125E0,1.9353071795864769253E-3/
CD    DATA ZERO,ONE,THREE,FOUR,EIGHT/0.0D0,1.0D0,3.0D0,4.0D0,8.0D0/,
CD   1     FIVE5,SIXTY4,ONEOV8,P17/5.5D0,64.0D0,0.125D0,1.716D-1/,
CD   2     TWO56,CONS/256.0D0,-1.1593151565841244881D-1/,
CD   3     PI2,TWOPI/6.3661977236758134308D-1,6.2831853071795864769D0/,
CD   4     TWOPI1,TWOPI2/6.28125D0,1.9353071795864769253D-3/
C-------------------------------------------------------------------
C  Machine-dependent constants
C-------------------------------------------------------------------
CS    DATA XMAX/8.19E+03/,XSMALL/1.22E-09/,XINF/1.7E+38/
CD    DATA XMAX/1.07D+09/,XSMALL/9.31D-10/,XINF/1.7D+38/
C-------------------------------------------------------------------
C  Zeroes of Bessel functions
C-------------------------------------------------------------------
CS    DATA XJ0/2.4048255576957727686E+0/,XJ1/5.5200781102863106496E+0/,
CS   1     XY0/8.9357696627916752158E-1/,XY1/3.9576784193148578684E+0/,
CS   2     XY2/7.0860510603017726976E+0/,
CS   3     XJ01/ 616.0E+0/, XJ02/-1.4244423042272313784E-03/,
CS   4     XJ11/1413.0E+0/, XJ12/ 5.4686028631064959660E-04/,
CS   5     XY01/ 228.0E+0/, XY02/ 2.9519662791675215849E-03/,
CS   6     XY11/1013.0E+0/, XY12/ 6.4716931485786837568E-04/,
CS   7     XY21/1814.0E+0/, XY22/ 1.1356030177269762362E-04/
CD    DATA XJ0/2.4048255576957727686D+0/,XJ1/5.5200781102863106496D+0/,
CD   1     XY0/8.9357696627916752158D-1/,XY1/3.9576784193148578684D+0/,
CD   2     XY2/7.0860510603017726976D+0/,
CD   3     XJ01/ 616.0D+0/, XJ02/-1.4244423042272313784D-03/,
CD   4     XJ11/1413.0D+0/, XJ12/ 5.4686028631064959660D-04/,
CD   5     XY01/ 228.0D+0/, XY02/ 2.9519662791675215849D-03/,
CD   6     XY11/1013.0D+0/, XY12/ 6.4716931485786837568D-04/,
CD   7     XY21/1814.0D+0/, XY22/ 1.1356030177269762362D-04/
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
C  J0(X) / (X**2 - XJ0**2),  XSMALL  <  |X|  <=  4.0
C--------------------------------------------------------------------
CS    DATA PJ0/6.6302997904833794242E+06,-6.2140700423540120665E+08,
CS   1         2.7282507878605942706E+10,-4.1298668500990866786E+11,
CS   2        -1.2117036164593528341E-01, 1.0344222815443188943E+02,
CS   3        -3.6629814655107086448E+04/
CS    DATA QJ0/4.5612696224219938200E+05, 1.3985097372263433271E+08,
CS   1         2.6328198300859648632E+10, 2.3883787996332290397E+12,
CS   2         9.3614022392337710626E+02/
CD    DATA PJ0/6.6302997904833794242D+06,-6.2140700423540120665D+08,
CD   1         2.7282507878605942706D+10,-4.1298668500990866786D+11,
CD   2        -1.2117036164593528341D-01, 1.0344222815443188943D+02,
CD   3        -3.6629814655107086448D+04/
CD    DATA QJ0/4.5612696224219938200D+05, 1.3985097372263433271D+08,
CD   1         2.6328198300859648632D+10, 2.3883787996332290397D+12,
CD   2         9.3614022392337710626D+02/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C  J0(X) / (X**2 - XJ1**2),  4.0  <  |X|  <=  8.0
C-------------------------------------------------------------------
CS    DATA PJ1/4.4176707025325087628E+03, 1.1725046279757103576E+04,
CS   1         1.0341910641583726701E+04,-7.2879702464464618998E+03,
CS   2        -1.2254078161378989535E+04,-1.8319397969392084011E+03,
CS   3         4.8591703355916499363E+01, 7.4321196680624245801E+02/
CS    DATA QJ1/3.3307310774649071172E+02,-2.9458766545509337327E+03,
CS   1         1.8680990008359188352E+04,-8.4055062591169562211E+04,
CS   2         2.4599102262586308984E+05,-3.5783478026152301072E+05,
CS   3        -2.5258076240801555057E+01/
CD    DATA PJ1/4.4176707025325087628D+03, 1.1725046279757103576D+04,
CD   1         1.0341910641583726701D+04,-7.2879702464464618998D+03,
CD   2        -1.2254078161378989535D+04,-1.8319397969392084011D+03,
CD   3         4.8591703355916499363D+01, 7.4321196680624245801D+02/
CD    DATA QJ1/3.3307310774649071172D+02,-2.9458766545509337327D+03,
CD   1         1.8680990008359188352D+04,-8.4055062591169562211D+04,
CD   2         2.4599102262586308984D+05,-3.5783478026152301072D+05,
CD   3        -2.5258076240801555057D+01/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y0(X) - 2 LN(X/XY0) J0(X)) / (X**2 - XY0**2),
C        XSMALL  <  |X|  <=  3.0
C--------------------------------------------------------------------
CS    DATA PY0/1.0102532948020907590E+04,-2.1287548474401797963E+06,
CS   1         2.0422274357376619816E+08,-8.3716255451260504098E+09,
CS   2         1.0723538782003176831E+11,-1.8402381979244993524E+01/
CS    DATA QY0/6.6475986689240190091E+02, 2.3889393209447253406E+05,
CS   1         5.5662956624278251596E+07, 8.1617187777290363573E+09,
CS   2         5.8873865738997033405E+11/
CD    DATA PY0/1.0102532948020907590D+04,-2.1287548474401797963D+06,
CD   1         2.0422274357376619816D+08,-8.3716255451260504098D+09,
CD   2         1.0723538782003176831D+11,-1.8402381979244993524D+01/
CD    DATA QY0/6.6475986689240190091D+02, 2.3889393209447253406D+05,
CD   1         5.5662956624278251596D+07, 8.1617187777290363573D+09,
CD   2         5.8873865738997033405D+11/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y0(X) - 2 LN(X/XY1) J0(X)) / (X**2 - XY1**2),
C        3.0  <  |X|  <=  5.5
C--------------------------------------------------------------------
CS    DATA PY1/-1.4566865832663635920E+04, 4.6905288611678631510E+06,
CS   1         -6.9590439394619619534E+08, 4.3600098638603061642E+10,
CS   2         -5.5107435206722644429E+11,-2.2213976967566192242E+13,
CS   3          1.7427031242901594547E+01/
CS    DATA QY1/ 8.3030857612070288823E+02, 4.0669982352539552018E+05,
CS   1          1.3960202770986831075E+08, 3.4015103849971240096E+10,
CS   2          5.4266824419412347550E+12, 4.3386146580707264428E+14/
CD    DATA PY1/-1.4566865832663635920D+04, 4.6905288611678631510D+06,
CD   1         -6.9590439394619619534D+08, 4.3600098638603061642D+10,
CD   2         -5.5107435206722644429D+11,-2.2213976967566192242D+13,
CD   3          1.7427031242901594547D+01/
CD    DATA QY1/ 8.3030857612070288823D+02, 4.0669982352539552018D+05,
CD   1          1.3960202770986831075D+08, 3.4015103849971240096D+10,
CD   2          5.4266824419412347550D+12, 4.3386146580707264428D+14/
C-------------------------------------------------------------------
C  Coefficients for rational approximation of
C    (Y0(X) - 2 LN(X/XY2) J0(X)) / (X**2 - XY2**2),
C        5.5  <  |X|  <=  8.0
C--------------------------------------------------------------------
CS    DATA PY2/ 2.1363534169313901632E+04,-1.0085539923498211426E+07,
CS   1          2.1958827170518100757E+09,-1.9363051266772083678E+11,
CS   2         -1.2829912364088687306E+11, 6.7016641869173237784E+14,
CS   3         -8.0728726905150210443E+15,-1.7439661319197499338E+01/
CS    DATA QY2/ 8.7903362168128450017E+02, 5.3924739209768057030E+05,
CS   1          2.4727219475672302327E+08, 8.6926121104209825246E+10,
CS   2          2.2598377924042897629E+13, 3.9272425569640309819E+15,
CS   3          3.4563724628846457519E+17/
CD    DATA PY2/ 2.1363534169313901632D+04,-1.0085539923498211426D+07,
CD   1          2.1958827170518100757D+09,-1.9363051266772083678D+11,
CD   2         -1.2829912364088687306D+11, 6.7016641869173237784D+14,
CD   3         -8.0728726905150210443D+15,-1.7439661319197499338D+01/
CD    DATA QY2/ 8.7903362168128450017D+02, 5.3924739209768057030D+05,
CD   1          2.4727219475672302327D+08, 8.6926121104209825246D+10,
CD   2          2.2598377924042897629D+13, 3.9272425569640309819D+15,
CD   3          3.4563724628846457519D+17/
C-------------------------------------------------------------------
C  Coefficients for Hart,s approximation,  |X| > 8.0
C-------------------------------------------------------------------
CS    DATA P0/3.4806486443249270347E+03, 2.1170523380864944322E+04,
CS   1        4.1345386639580765797E+04, 2.2779090197304684302E+04,
CS   2        8.8961548424210455236E-01, 1.5376201909008354296E+02/
CS    DATA Q0/3.5028735138235608207E+03, 2.1215350561880115730E+04,
CS   1        4.1370412495510416640E+04, 2.2779090197304684318E+04,
CS   2        1.5711159858080893649E+02/
CS    DATA P1/-2.2300261666214198472E+01,-1.1183429920482737611E+02,
CS   1        -1.8591953644342993800E+02,-8.9226600200800094098E+01,
CS   2        -8.8033303048680751817E-03,-1.2441026745835638459E+00/
CS    DATA Q1/1.4887231232283756582E+03, 7.2642780169211018836E+03,
CS   1        1.1951131543434613647E+04, 5.7105024128512061905E+03,
CS   2        9.0593769594993125859E+01/
CD    DATA P0/3.4806486443249270347D+03, 2.1170523380864944322D+04,
CD   1        4.1345386639580765797D+04, 2.2779090197304684302D+04,
CD   2        8.8961548424210455236D-01, 1.5376201909008354296D+02/
CD    DATA Q0/3.5028735138235608207D+03, 2.1215350561880115730D+04,
CD   1        4.1370412495510416640D+04, 2.2779090197304684318D+04,
CD   2        1.5711159858080893649D+02/
CD    DATA P1/-2.2300261666214198472D+01,-1.1183429920482737611D+02,
CD   1        -1.8591953644342993800D+02,-8.9226600200800094098D+01,
CD   2        -8.8033303048680751817D-03,-1.2441026745835638459D+00/
CD    DATA Q1/1.4887231232283756582D+03, 7.2642780169211018836D+03,
CD   1        1.1951131543434613647D+04, 5.7105024128512061905D+03,
CD   2        9.0593769594993125859D+01/
C-------------------------------------------------------------------
C  Check for error conditions
C-------------------------------------------------------------------
      AX = ABS(ARG)
      IF ((JINT .EQ. 1) .AND. (ARG .LE. ZERO)) THEN
            RESULT = -XINF
            GO TO 2000
         ELSE IF (AX .GT. XMAX) THEN
            RESULT = ZERO
            GO TO 2000
      END IF
      IF (AX .GT. EIGHT) GO TO 800
      IF (AX .LE. XSMALL) THEN
         IF (JINT .EQ. 0) THEN
               RESULT = ONE
            ELSE
               RESULT = PI2 * (LOG(AX) + CONS)
         END IF
         GO TO 2000
      END IF
C-------------------------------------------------------------------
C  Calculate J0 for appropriate interval, preserving
C     accuracy near the zero of J0
C-------------------------------------------------------------------
      ZSQ = AX * AX
      IF (AX .LE. FOUR) THEN
            XNUM = (PJ0(5) * ZSQ + PJ0(6)) * ZSQ + PJ0(7)
            XDEN = ZSQ + QJ0(5)
            DO 50 I = 1, 4
               XNUM = XNUM * ZSQ + PJ0(I)
               XDEN = XDEN * ZSQ + QJ0(I)
   50       CONTINUE
            PROD = ((AX - XJ01/TWO56) - XJ02) * (AX + XJ0)
         ELSE
            WSQ = ONE - ZSQ / SIXTY4
            XNUM = PJ1(7) * WSQ + PJ1(8)
            XDEN = WSQ + QJ1(7)
            DO 220 I = 1, 6
               XNUM = XNUM * WSQ + PJ1(I)
               XDEN = XDEN * WSQ + QJ1(I)
  220       CONTINUE
            PROD = (AX + XJ1) * ((AX - XJ11/TWO56) - XJ12)
      END IF
      RESULT = PROD * XNUM / XDEN
      IF (JINT .EQ. 0) GO TO 2000
C-------------------------------------------------------------------
C  Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x),
C    where xn is a zero of Y0
C-------------------------------------------------------------------
      IF (AX .LE. THREE) THEN
            UP = (AX-XY01/TWO56)-XY02
            XY = XY0
         ELSE IF (AX .LE. FIVE5) THEN
            UP = (AX-XY11/TWO56)-XY12
            XY = XY1
         ELSE
            UP = (AX-XY21/TWO56)-XY22
            XY = XY2
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
C  Now calculate Y0 for appropriate interval, preserving
C     accuracy near the zero of Y0
C-------------------------------------------------------------------
      IF (AX .LE. THREE) THEN
            XNUM = PY0(6) * ZSQ + PY0(1)
            XDEN = ZSQ + QY0(1)
            DO 340 I = 2, 5
               XNUM = XNUM * ZSQ + PY0(I)
               XDEN = XDEN * ZSQ + QY0(I)
  340       CONTINUE
         ELSE IF (AX .LE. FIVE5) THEN
            XNUM = PY1(7) * ZSQ + PY1(1)
            XDEN = ZSQ + QY1(1)
            DO 360 I = 2, 6
               XNUM = XNUM * ZSQ + PY1(I)
               XDEN = XDEN * ZSQ + QY1(I)
  360       CONTINUE
         ELSE
            XNUM = PY2(8) * ZSQ + PY2(1)
            XDEN = ZSQ + QY2(1)
            DO 380 I = 2, 7
               XNUM = XNUM * ZSQ + PY2(I)
               XDEN = XDEN * ZSQ + QY2(I)
  380       CONTINUE
      END IF
      RESULT = RESJ + UP * DOWN * XNUM / XDEN
      GO TO 2000
C-------------------------------------------------------------------
C  Calculate J0 or Y0 for |ARG|  >  8.0
C-------------------------------------------------------------------
  800 Z = EIGHT / AX
      W = AX / TWOPI
      W = AINT(W) + ONEOV8
      W = (AX - W * TWOPI1) - W * TWOPI2
      ZSQ = Z * Z
      XNUM = P0(5) * ZSQ + P0(6)
      XDEN = ZSQ + Q0(5)
      UP = P1(5) * ZSQ + P1(6)
      DOWN = ZSQ + Q1(5)
      DO 850 I = 1, 4
         XNUM = XNUM * ZSQ + P0(I)
         XDEN = XDEN * ZSQ + Q0(I)
         UP = UP * ZSQ + P1(I)
         DOWN = DOWN * ZSQ + Q1(I)
  850 CONTINUE
      R0 = XNUM / XDEN
      R1 = UP / DOWN
      IF (JINT .EQ. 0) THEN
            RESULT = SQRT(PI2/AX) * (R0*COS(W) - Z*R1*SIN(W))
         ELSE
            RESULT = SQRT(PI2/AX) * (R0*SIN(W) + Z*R1*COS(W))
      END IF
 2000 RETURN
C---------- Last line of CALJY0 ----------
      END
CD    DOUBLE PRECISION FUNCTION BESJ0(X)
CS    REAL FUNCTION BESJ0(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the first kind of order zero for arguments  |X| <= XMAX
C   (see comments heading CALJY0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL  X, RESULT
CD    DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      JINT=0
      CALL CALJY0(X,RESULT,JINT)
      BESJ0 = RESULT
      RETURN
C---------- Last line of BESJ0 ----------
      END
CD    DOUBLE PRECISION FUNCTION BESY0(X)
CS    REAL FUNCTION BESY0(X)
C--------------------------------------------------------------------
C
C This subprogram computes approximate values for Bessel functions
C   of the second kind of order zero for arguments 0 < X <= XMAX
C   (see comments heading CALJY0).
C
C--------------------------------------------------------------------
      INTEGER JINT
CS    REAL  X, RESULT
CD    DOUBLE PRECISION  X, RESULT
C--------------------------------------------------------------------
      JINT=1
      CALL CALJY0(X,RESULT,JINT)
      BESY0 = RESULT
      RETURN
C---------- Last line of BESY0 ----------
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
