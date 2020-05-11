*DECK CHKPR4
      SUBROUTINE CHKPR4 (IORDER, A, B, M, MBDCND, C, D, N, NBDCND, COFX,
     +   IDMN, IERROR)
C***BEGIN PROLOGUE  CHKPR4
C***SUBSIDIARY
C***PURPOSE  Subsidiary to SEPX4
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (CHKPR4-S)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     This program checks the input parameters for errors.
C
C***SEE ALSO  SEPX4
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   801001  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900402  Added TYPE section.  (WRB)
C***END PROLOGUE  CHKPR4
      EXTERNAL COFX
C***FIRST EXECUTABLE STATEMENT  CHKPR4
      IERROR = 1
      IF (A.GE.B .OR. C.GE.D) RETURN
C
C     CHECK BOUNDARY SWITCHES
C
      IERROR = 2
      IF (MBDCND.LT.0 .OR. MBDCND.GT.4) RETURN
      IERROR = 3
      IF (NBDCND.LT.0 .OR. NBDCND.GT.4) RETURN
C
C     CHECK FIRST DIMENSION IN CALLING ROUTINE
C
      IERROR = 5
      IF (IDMN .LT. 7) RETURN
C
C     CHECK M
C
      IERROR = 6
      IF (M.GT.(IDMN-1) .OR. M.LT.6) RETURN
C
C     CHECK N
C
      IERROR = 7
      IF (N .LT. 5) RETURN
C
C     CHECK IORDER
C
      IERROR = 8
      IF (IORDER.NE.2 .AND. IORDER.NE.4) RETURN
C
C     CHECK THAT EQUATION IS ELLIPTIC
C
      DLX = (B-A)/M
      DO  30 I=2,M
         XI = A+(I-1)*DLX
         CALL COFX (XI,AI,BI,CI)
      IF (AI.GT.0.0) GO TO 10
      IERROR=10
      RETURN
   10 CONTINUE
   30 CONTINUE
C
C     NO ERROR FOUND
C
      IERROR = 0
      RETURN
      END
