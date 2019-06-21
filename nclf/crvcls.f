C*********************************************************************
C*    NAME         :  crvcls.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       crvcls.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:44
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine crvcls
c*          Set curve closed flag if curve is closed.
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - key of curve
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : Updates curve closed flag if curve is closed.
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine crvcls (nclkey)

      include 'com4a.com'

      integer*4 nclkey

      real*8 p0(3),p1(3),v0(3),v1(3),tolsq,sec0,sec1
      real*4 u
      integer*2 iclsd

      tolsq = sc(27)**2
c
c...  Get start and end points and vectors of curve
c
      u = 0.
      call crvevl (nclkey, u, p0, v0)
      u = 1.
      call crvevl (nclkey, u, p1, v1)
c
c...  If start and end points not coincident, exit
c
      if ((p0(1)-p1(1))**2+(p0(2)-p1(2))**2+(p0(3)-p1(3))**2.gt.tolsq)
     x     goto 999
c
c...  If start and end vectors not parallel, exit
c
      sec0 = dsqrt(v0(1)**2+v0(2)**2+v0(3)**2)
      if (sec0.lt.1.e-6) goto 999
      sec1 = dsqrt(v1(1)**2+v1(2)**2+v1(3)**2)
      if (sec1.lt.1.e-6) goto 999
      if ((v0(1)*v1(1)+v0(2)*v1(2)+v0(3)*v1(3))/sec0/sec1.lt..9998)
     x    goto 999
c
c...  Set closed flag
c
      iclsd = 1
      call ptclsd (nclkey, 0, iclsd)

999   return
      end
