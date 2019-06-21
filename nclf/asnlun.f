C*********************************************************************
C*    NAME         : asnlun.for
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       asnlun.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:31
C********************************************************************/
      SUBROUTINE ASNLUN (UNIT,DEVICE,TERMNO,IERR)
 
C ************************************************************************
C ************************************************************************
C           THIS SUBROUTINE IS FOR USE ON THE VAX VERSION ONLY.  IT
C           REPLACES THE CALL TO THE FORTRAN RUN TIME LIBRARY ASNLUN
C           MODULE THAT RSX11M HAS BUT VAX DOESN'T HAVE
C ************************************************************************
C ************************************************************************
 
      INCLUDE 'com8a.com'
 
      INTEGER*2 TERMNO,IERR,UNIT
      CHARACTER*1 ADEVIC(6),ACTERM(3)
      CHARACTER*3 CTERMN
      CHARACTER*6 DEVICE,CDEVIC
      EQUIVALENCE (ADEVIC,CDEVIC),(ACTERM,CTERMN)
 
C          DUMMY SUBROUTINE TO REPLACE ASNLUN FORTRAN CALL IN RXS
C
C      THE ATTEMPT TO MAKE IT LIKE ASNLUN ON RSX WAS UNSUCCESSFUL.
C      IT IS ALWAYS CALLED WITH THE COMPLETE DEVICE NAME IN ARG DEVICE
C      AND 0 IN TERMNO. THE SAME EFFECT COULD PROBABLY BE ACHIEVED BY
C      DOING A CLOSE AND AN OPEN USING DEVICE IN THE FILE SPEC.
 
      IERR=0
99999 RETURN
      END
