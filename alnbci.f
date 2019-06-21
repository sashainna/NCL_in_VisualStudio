
C*********************************************************************
C*    NAME         :  alnbci.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       alnbci.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:36
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine alnbci
C*         called by shafix.
C*         intersect a-ln and b-ci.  put xi,yi in a(7,8)
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
      subroutine alnbci
c
      include 'com8a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 f,a,b,asn
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40)
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,ksn)
c
      rtool=sc(28)/2.
      bsig=kb(2)
      r=b(6)+rtool*bsig
      dis=a(6)-a(4)*b(4)-a(5)*b(5)
      h=0.
      hsq=r**2-dis**2
      if(hsq.gt.0.) h=sqrt(hsq)
c               look at ln endpt from cctr for h direction
      hchk=-a(5)*(a(2)-b(4))+a(4)*(a(3)-b(5))
      if(hchk.lt.0.) h=-h
      a(7)=b(4)+a(4)*dis-a(5)*h
      a(8)=b(5)+a(5)*dis+a(4)*h
99    return
      end
