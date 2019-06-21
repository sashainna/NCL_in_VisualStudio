
C*********************************************************************
C*    NAME         :  berr.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       berr.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:38
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine berr
C*        calc errb from xnd,ynd  to b-entity
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
      subroutine berr
c
      include 'com8a.com'
      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
      common/pblok/p(400)
      integer*2 ksc(500),kw(576),ksn(4),kp(1600),ka(48),kb(48)
      equivalence (sc,ksc),(w,kw),(asn,ksn),(p,kp),(a,ka),(b,kb)
c
      if(kb(48).ne.5)goto 20
c                     b-entity is line
      sec=dsqrt(b(4)**2+b(5)**2)
      if(sec.gt.0.)goto 15
c                     error. line appears vertical
      ifl(2)=166
      goto 99
15    errb=(b(4)*(ynd-b(2))-b(5)*(xnd-b(1)))/sec
      goto 99
c
c                     b-entity is circle
20    dis=dsqrt((b(1)-xnd)**2+(b(2)-ynd)**2)
      errb=dabs(b(7)-dis)
99    return
      end
