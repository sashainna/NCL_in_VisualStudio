C*********************************************************************
C*    NAME         :  filmod.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*        filmod.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:10:03
C********************************************************************/

C******************************************************************
C*
C*      SUBROUTINE FILMOD (VEC,ANGL,SVEC,FVEC,IMOD)
C*
C*      PURPOSE:
C*            THIS ROUTINE SETS UP THE DIRECTION FOR THE CIRCLE
C*            TANGENT TO THE CURRENT LINES.  IT ALSO SETS UP START
C*            AND STOP POINTS FOR THE OUTPUT
C* 
C*       Except for the include files, this routine was imported from
C*       PREPST without any changes being made to the file.
C*
C*******************************************************************

      subroutine filmod (VEC,ANGL,SVEC,FVEC,IMOD)
C
      include 'com8a.com'
      include 'fillet.com'
      REAL*8 VEC(3,3),ANGL(2),SVEC(2),FVEC(2)
C
      INTEGER*4 QUAD(2)
      INTEGER*2 IMOD(2)
C
C... CLEAN UP VECTORS IF THEY ARE CLOSE TO BOUNRIES
C
C... DETERMIN XL,XS FOR CIRCLE CALCS
C... FIND QUADRANTS
C
      DO 425 J2=1,2,1
          IF (VEC(J2,1) .GT. 0. .AND. VEC(J2,2) .GE. 0.) THEN
             QUAD(J2) = 1
          ELSE IF (VEC(J2,1) .LE. 0. .AND. VEC(J2,2) .GT. 0.) THEN
             QUAD(J2) = 2
          ELSE IF (VEC(J2,1) .LT. 0. .AND. VEC(J2,2) .LE. 0.) THEN
              QUAD(J2) = 3
          ELSE IF (VEC(J2,1) .GE. 0. .AND. VEC(J2,2) .LT. 0.) THEN
              QUAD(J2) = 4
          ENDIF
  425 CONTINUE
C
C... SET UP XL,XS,YL,YS DEPENDING ON QUARANTS
C
  445 GO TO (500,600,700,800), QUAD(1)
C
C... QUAD - 1
C
  500 GO TO (510,520,530,540), QUAD(2)
C
  510 IF (ANGL(1) .GT. ANGL(2)) THEN
          IMOD(1) = 4
          IMOD(2) = 4
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))
      ELSE
          IMOD(1) = 2
          IMOD(2) = 2
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ENDIF
      GO TO 900
C
  520 IMOD(1) = 2
      IMOD(2) = 3
      SVEC(1) = DABS (VEC(1,2))
      SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
      FVEC(1) = DABS (VEC(2,2))
      FVEC(2) = DABS (VEC(2,1))
      GO TO 900
C
  530 IF (DABS(VEC(1,2)) .GT. DABS(VEC(2,2))) THEN
          IMOD(1) = 2
          IMOD(2) = 4
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))
      ELSE
          IMOD(1) = 4
          IMOD(2) = 2
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ENDIF
      GO TO 900
C
  540 IMOD(1) = 4
      IMOD(2) = 3
      SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
      SVEC(2) = DABS (VEC(1,1))
      FVEC(1) = DABS (VEC(2,2))
      FVEC(2) = DABS (VEC(2,1))
      GO TO 900
C
C... QUAD - 2
C
  600 GO TO (610,620,630,640), QUAD(2)
C
  610 IMOD(1) = 1
      IMOD(2) = 4
      SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
      SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
      FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
      FVEC(2) = DABS (VEC(2,1))
      GO TO 900
C
  620 IF (ANGL(1) .GT. ANGL(2)) THEN
          IMOD(1) = 1
          IMOD(2) = 1
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ELSE
          IMOD(1) = 3
          IMOD(2) = 3
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))
      ENDIF
      GO TO 900
C
  630 IMOD(1) = 3
      IMOD(2) = 4
      SVEC(1) = DABS (VEC(1,2))
      SVEC(2) = DABS (VEC(1,1))
      FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
      FVEC(2) = DABS (VEC(2,1))
      GO TO 900
C
  640 IF (DABS(VEC(1,2)) .GT. DABS(VEC(2,2))) THEN
          IMOD(1) = 1
          IMOD(2) = 3
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))
      ELSE
          IMOD(1) = 3
          IMOD(2) = 1
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ENDIF
      GO TO 900
C
C... QUAD - 3
C
  700 GO TO (710,720,730,740), QUAD(2)
C
  710 IF (DABS(VEC(1,2)) .GT. DABS(VEC(2,2))) THEN
          IMOD(1) = 4
          IMOD(2) = 2
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ELSE
          IMOD(1) = 2
          IMOD(2) = 4
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))
      ENDIF
      GO TO 900
C
  720 IMOD(1) = 2
      IMOD(2) = 1
      SVEC(1) = DABS (VEC(1,2))
      SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
      FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
      FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      GO TO 900
C
  730 IF (ANGL(1) .GT. ANGL(2)) THEN
          IMOD(1) = 2
          IMOD(2) = 2
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ELSE
          IMOD(1) = 4
          IMOD(2) = 4
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))
      ENDIF
      GO TO 900
C
  740 IMOD(1) = 4
      IMOD(2) = 1
      SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
      SVEC(2) = DABS (VEC(1,1))
      FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
      FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      GO TO 900
C
C... QUAD - 4
C
  800 GO TO (810,820,830,840), QUAD(2)
C
  810 IMOD(1) = 1
      IMOD(2) = 2
      SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
      SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
      FVEC(1) = DABS (VEC(2,2))
      FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      GO TO 900
C
  820 IF (DABS(VEC(1,2)) .GT. DABS(VEC(2,2))) THEN
          IMOD(1) = 3
          IMOD(2) = 1
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ELSE
          IMOD(1) = 1
          IMOD(2) = 3
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))
      ENDIF
      GO TO 900
C
  830 IMOD(1) = 3
      IMOD(2) = 2
      SVEC(1) = DABS (VEC(1,2))
      SVEC(2) = DABS (VEC(1,1))
      FVEC(1) = DABS (VEC(2,2))
      FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      GO TO 900
C
  840 IF (ANGL(1) .GT. ANGL(2)) THEN
          IMOD(1) = 3
          IMOD(2) = 3
          SVEC(1) = DABS (VEC(1,2))
          SVEC(2) = DABS (VEC(1,1))
          FVEC(1) = DABS (VEC(2,2))
          FVEC(2) = DABS (VEC(2,1))
      ELSE
          IMOD(1) = 1
          IMOD(2) = 1
          SVEC(1) = DABS (VEC(1,2))*(0.0-1.0)
          SVEC(2) = DABS (VEC(1,1))*(0.0-1.0)
          FVEC(1) = DABS (VEC(2,2))*(0.0-1.0)
          FVEC(2) = DABS (VEC(2,1))*(0.0-1.0)
      ENDIF
C
  900 RETURN
      END
