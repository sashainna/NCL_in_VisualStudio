
C*********************************************************************
C*    NAME         :  angcal.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       angcal.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:37
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine angcal
C*       calc st and end angles this circle. sto in a(10)
C*       (stpt is in dum(79))
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine angcal

      include 'com8a.com'
      include 'const.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)
      common/lthdum/dum(80)

      real*8 e,f,a,b,dum
      real*4 ad(300),ang(2)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn)
      equivalence (a(10),ang)


c
c..... start angle
c
      ang(1)=0.
      dx=dum(79)-a(4)
      dy=dum(80)-a(5)
c
c..... round very small dy's to zero.  (2-plcs.)
c
      if (abs(dy).lt..0001) dy=0.
      if (dx**2+dy**2.lt.1.e-6) goto 20

      if (dabs(dx).lt..0001) then
        if (dy .gt. 0.) then
           ang(1) = HALF_PI
        else if (dy .lt. 0.) then
           ang(1) = - HALF_PI
        endif
        goto 20
      endif

      ang(1)=atan2(dy,dx)

20    if (ang(1).lt.0.) ang(1)=ang(1)+TWO_PI
c
c..... end angle
c
      ang(2)=0.
      dx=a(8)-a(4)
      dy=a(9)-a(5)

      if (abs(dy).lt..0001) dy=0.

      if (dx**2+dy**2.lt.1.e-6) goto 30

      if (dabs(dx).lt..0001) then
        if (dy .gt. 0.) then
           ang(2) = HALF_PI
        else if (dy .lt. 0.) then
           ang(2) = - HALF_PI
        endif
        goto 30
      endif

      ang(2)=atan2(dy,dx)

30    if (ang(2).lt.0.) ang(2)=ang(2)+TWO_PI
c
c..... angles must agree with clw direc in ka(2)
c
      asig=ka(2)
      delb=ang(2)-ang(1)
      if (asig*delb.ge.0.) goto 99
      if (asig.gt.0.) ang(2)=ang(2)+TWO_PI
      if (asig.lt.0.) ang(1)=ang(1)+TWO_PI

99    return
      end
