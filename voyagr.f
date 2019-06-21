C*********************************************************************
C*    NAME         :  voyagr.for
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       voyagr.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:36
C********************************************************************/
      subroutine voyagr (ikey,nkey)
C
      INTEGER*2 IKEY(2),NKEY
      INTEGER*4 IEURO
C
      DATA NATT /2/
      CALL SETEUR (IEURO)
      IF (IEURO .EQ. 0) GO TO 2000
 2000 DO 2100 I=1,NATT,1
          IKEY(I) = I
 2100 CONTINUE
      NKEY   = NATT
      RETURN
      END
C
      SUBROUTINE SETEUR (IEURO)
      INTEGER*4 IEURO
      IEURO = 0
      return
      end
