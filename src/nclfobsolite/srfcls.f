C*********************************************************************
C*    NAME         :  srfcls.f
C*       CONTAINS:
C*    COPYRIGHT 1991 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       srfcls.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:43
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine srfcls
c*          Set surf closed flag if surf is closed.
c*                                                               
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey    - key of surf
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : Updates surf closed flag if surface is closed.
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine srfcls (nclkey)

      include 'com4a.com'

      integer*4 nclkey

      real*8 pvs00(12),pvs01(12),pvs10(12),pvs11(12)
      real*8 tolsq,sec0,sec1,asw
      real*4 u,v
      integer*2 iside, iclsd

      tolsq = 100.*sc(27)**2
c
c.....Need to initialize asw - ASF 2/10/14.
c
      asw = 0.
      call ptdesc(nclkey,9,asw)
c
c...  Get corner points and vectors of surface.
c
      u = 0.
      v = 0.
      call srfevl (asw, u, v, pvs00)
      u = 0.
      v = 1.
      call srfevl (asw, u, v, pvs01)
      u = 1.
      v = 0.
      call srfevl (asw, u, v, pvs10)
      u = 1.
      v = 1.
      call srfevl (asw, u, v, pvs11)
c
c...  Check closed in v
c...  If first corner points not coincident, go check for closed in u.
c
      if ((pvs00(1)-pvs01(1))**2+(pvs00(2)-pvs01(2))**2
     x   +(pvs00(3)-pvs01(3))**2.gt.tolsq) goto 100

      if ((pvs10(1)-pvs11(1))**2+(pvs10(2)-pvs11(2))**2
     x   +(pvs10(3)-pvs11(3))**2.gt.tolsq) goto 999
c
c...  If normal vectors not parallel, exit
c
      sec0 = dsqrt(pvs00(4)**2+pvs00(5)**2+pvs00(6)**2)
      if (sec0.lt.1.e-6) goto 999
      sec1 = dsqrt(pvs01(4)**2+pvs01(5)**2+pvs01(6)**2)
      if (sec1.lt.1.e-6) goto 999
      if ((pvs00(4)*pvs01(4)+pvs00(5)*pvs01(5)
     x    +pvs00(6)*pvs01(6))/sec0/sec1.lt..9998) goto 999

      sec0 = dsqrt(pvs10(4)**2+pvs10(5)**2+pvs10(6)**2)
      if (sec0.lt.1.e-6) goto 999
      sec1 = dsqrt(pvs11(4)**2+pvs11(5)**2+pvs11(6)**2)
      if (sec1.lt.1.e-6) goto 999
      if ((pvs10(4)*pvs11(4)+pvs10(5)*pvs11(5)
     x    +pvs10(6)*pvs11(6))/sec0/sec1.lt..9998) goto 999
c
c...  Set closed flag
c
      iside = 1
      iclsd = 1
      call ptclsd (nclkey, iside, iclsd)
      goto 999
c
c...  Check closed in u.
c...  If corner points not coincident, exit
c
100   if ((pvs00(1)-pvs10(1))**2+(pvs00(2)-pvs10(2))**2
     x   +(pvs00(3)-pvs10(3))**2.gt.tolsq) goto 999

      if ((pvs01(1)-pvs11(1))**2+(pvs01(2)-pvs11(2))**2
     x   +(pvs01(3)-pvs11(3))**2.gt.tolsq) goto 999
c
c...  If normal vectors not parallel, exit
c
      sec0 = dsqrt(pvs00(4)**2+pvs00(5)**2+pvs00(6)**2)
      if (sec0.lt.1.e-6) goto 999
      sec1 = dsqrt(pvs10(4)**2+pvs10(5)**2+pvs10(6)**2)
      if (sec1.lt.1.e-6) goto 999
      if ((pvs00(4)*pvs10(4)+pvs00(5)*pvs10(5)
     x    +pvs00(6)*pvs10(6))/sec0/sec1.lt..9998) goto 999

      sec0 = dsqrt(pvs01(4)**2+pvs01(5)**2+pvs01(6)**2)
      if (sec0.lt.1.e-6) goto 999
      sec1 = dsqrt(pvs11(4)**2+pvs11(5)**2+pvs11(6)**2)
      if (sec1.lt.1.e-6) goto 999
      if ((pvs01(4)*pvs11(4)+pvs01(5)*pvs11(5)
     x    +pvs01(6)*pvs11(6))/sec0/sec1.lt..9998) goto 999
c
c...  Set closed flag
c
      iside = 0
      iclsd = 1
      call ptclsd (nclkey, iside, iclsd)

999   return
      end
