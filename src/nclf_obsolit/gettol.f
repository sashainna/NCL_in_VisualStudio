C*********************************************************************
C*    NAME         :  gettol.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       gettol.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:08
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gettol
c       Return current value of toler (in inches).
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          tol            - Current value of toler
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine gettol(tol)

      include 'com8a.com'

      real*8 tol

      tol = sc(27)

      if (ifl(264).eq.1) tol = tol/25.4d0

99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine getsct
c       Return current value of toler.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          tol            - Current value of toler
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c
      subroutine getsct(tol)

      include 'com8a.com'

      real*8 tol

      tol = sc(27)

      return
      end
