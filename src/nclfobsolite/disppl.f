C*********************************************************************
C*    NAME         :  disppl.f
C*       CONTAINS:
C*     Displays a PLANE at a point in space
C* 
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       disppl.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:56
C********************************************************************/
C*
C***********************************************************************
C*   E_SUBROUTINE     : subroutine disppl
C*     Display plane at a point in space
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          plnasw : Associated word of PLANE to display
C*          ptasw  : Associated word of POINT to display PLANE at
C*          pltok  : Plane label
C*          plsub  : Plane subscript
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C***********************************************************************
 
      subroutine disppl (plnasw, ptasw, pltok, plsub)

      include 'com4a.com'
      include 'comdb.com'

      integer*4 plsub
      real*8 plnasw, ptasw
      character*64  pltok

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold

      integer*4 keypl, keypt
      integer*2 itpl, itpt
      logical trflg
      real*8 buf(7)

      token2 = pltok
      savid2 = token2
      ivxsub = plsub
      isvsub = ivxsub
      call vstchk
      keyold = keyhld
      istold = ist
      ifl(9) = ifl(11)
      ifl(10) = ifl(12)
      trflg = .true.
      call gtentt (plnasw, trflg, keypl, itpl, buf)
      call gtentt (ptasw, trflg, keypt, itpt, buf(5))
      call ptentt (itpl, buf, keypl, plnasw)
      rest = plnasw
      call vstore
c
c               Update the blank attribute in case the plane has
c               previously been erased.
      call blkgeo (keypl, 0)
      call dspent (keypl, ietype)
 
      return
      end
