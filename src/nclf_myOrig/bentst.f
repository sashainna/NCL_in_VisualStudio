
C*********************************************************************
C*    NAME         :  bentst.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       bentst.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine bentst
C*        put b-entity stpt (if any) in xnd,ynd
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine bentst
c
      include 'com8a.com'
      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
      common/pblok/p(400)
      integer*2 ksc(500),kw(576),ksn(4),kp(1600),ka(48),kb(48)
      equivalence (sc,ksc),(w,kw),(asn,ksn),(p,kp),(a,ka),(b,kb)
c
      if(kb(48).ne.5)goto 20
c                    b-entity is line
      xnd=b(1)+b(4)
      ynd=b(2)+b(5)
      goto 99
c                    b-entity is circle  (assume no limpl)
20    xnd=b(1)
      ynd=b(2)
      sec=dsqrt(b(8)**2+b(9)**2)
      if(sec.lt..001)goto 99
c                      limpl given.  calc fwd
      fx=-b(9)*b(6)
      fy=b(8)*b(6)
      dis=b(11)-b(8)*b(1)-b(9)*b(2)
      h=0.
      hsq=b(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
      xnd=b(1)+dis*b(8)-h*fx
      ynd=b(2)+dis*b(9)-h*fy
99    return
      end
