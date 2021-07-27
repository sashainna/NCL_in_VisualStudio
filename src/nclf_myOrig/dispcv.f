C*********************************************************************
C*    NAME         :  dispcv.f
C*       CONTAINS:
C*     Displays a CIRCEL or CURVE
C* 
C*    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       dispcv.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:55
C********************************************************************/
C*
C***********************************************************************
C*   E_SUBROUTINE     : subroutine dispcv
C*     Display CIRCLE or CURVE
C*
C*    PARAMETERS   
C*       INPUT  : 
C*          cvasw  : Associated word of CIRCLE or CURVE to display
C*          sval   : Number of S points to draw
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C***********************************************************************
 
      subroutine dispcv (cvasw, sval)

      include 'com4a.com'
      include 'comdb.com'

      real*8 cvasw, sval
      integer*4 nclkey
      integer*2 ifl136, ietype, nwds

      call gtdesc (cvasw, nclkey, nwds, ietype)
      ifl136 = ifl(136)
      ifl(136) = sval
c           Update the blank attribute in case the curve has
c           previously been erased.
      call blkgeo (nclkey, 0)
      call dspent (nclkey, ietype)
      ifl(136) = ifl136
 
      return
      end
