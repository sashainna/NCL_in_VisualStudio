
C*********************************************************************
C*    NAME         :  acibln.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       acibln.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:36
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine acibln
C*       intersect a-ci and b-ln.
C*       put xi,yi in a(8,9)  new angls in a(10)
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
      subroutine acibln
c
      include 'com8a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 f,a,b,asn
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn)

      real*8 r,rtool,x,y,vx,vy,del
c
      rtool=sc(28)/2.
      asig=ka(2)
      r=a(6)+rtool*asig

      vx = a(2)-a(4)
      vy = a(3)-a(5)

      if (asig .lt. 0.) then
        del = r/a(6)
        x = a(4) + del*vx
        y = a(5) + del*vy
      endif

      dis=b(6)-b(4)*a(4)-b(5)*a(5)
      h=0.
      hsq=r**2-dis**2
      if (hsq.gt.0.) h=dsqrt(hsq)
      hchk = -b(5)*vx + b(4)*vy
      if (hchk.lt.0.) h=-h
      a(8)=a(4)+b(4)*dis-b(5)*h
      a(9)=a(5)+b(5)*dis+b(4)*h

      if (asig .lt. 0. .and. a(9) .gt. (y + 1.d-6)) then
        ka(2) = -2
        a(8) = x
        a(9) = y
      endif

99    return
      end
