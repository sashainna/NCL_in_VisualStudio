
C*********************************************************************
C*    NAME         :  filename
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       acibci.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:36
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine acibci
C*       intersect 2 circles
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
      subroutine acibci
c
      include 'com8a.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 f,a,b,asn
cc      real*4 ad(300)
cc      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40),kf(1600)
cc      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn),(f,kf)
      integer*2 ka(40),kb(40)
      equivalence (a,ka),(b,kb)
c
      rtool=sc(28)/2.
      asig=ka(2)
      ra=a(6)+asig*rtool
      bsig=kb(2)
      rb=b(6)+bsig*rtool
      dx=b(4)-a(4)
      dy=b(5)-a(5)
      chd=sqrt(dx**2+dy**2)
      if (chd.gt.0.) goto 10
c               error. concentric circles.
      ifl(2)=168
      goto 99
10    fx=dx/chd
      fy=dy/chd
      rx=fy
      ry=-fx
      dis=(chd**2+ra**2-rb**2)/(2.*chd)
      h=0.
      hsq=ra**2-dis**2
      if (hsq.gt.0.) h=sqrt(hsq)
c               set h-direc per old endpt.
      hck=rx*(a(2)-a(4))+ry*(a(3)-a(5))
      if (hck.lt.0.) h=-h
      a(8)=a(4)+dis*fx+h*rx
      a(9)=a(5)+dis*fy+h*ry
99    return
      end
