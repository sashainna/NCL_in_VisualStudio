
C*********************************************************************
C*    NAME         :  ilbent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       ilbent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:12
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine ilbent
C*       intersect a-line with b-entity, 
C*       load result in xnd,ynd  or ifl(2)=+
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
      subroutine ilbent
c
      include 'com8a.com'
      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
      integer*2 ksc(500),kw(576),ksn(4),ka(48),kb(48)
      equivalence (sc,ksc),(w,kw),(asn,ksn),(a,ka),(b,kb)
c
      if(kb(48).ne.5)goto 20
c                b-entity is line.
      e=-a(5)
      f=a(4)
      g=e*a(1)+f*a(2)
      r=-b(5)
      s=b(4)
      t=r*b(1)+s*b(2)
      den=e*s-f*r
      if(dabs(den).gt.1.d-6) goto 10
c                 error.  parlel lines.
      ifl(2)=167
      goto 99
10    xnd=(g*s-f*t)/den
      ynd=(t*e-r*g)/den
      goto 99
c
c                b-entity is circle.  (if line endpt on circle, use it )
c
20    xnd=a(1)+a(4)
      ynd=a(2)+a(5)
      dis=dsqrt((b(1)-xnd)**2+(b(2)-ynd)**2)
      if(dabs(b(7)-dis).le..001)goto 99
c                intersect a-line, b-circle
      tl=dsqrt(a(4)**2+a(5)**2)
      co=a(4)/tl
      si=a(5)/tl
      al=-si
      bl=co
      dis=al*(b(1)-a(1))+bl*(b(2)-a(2))
      if(dis.ge.0.) goto 25
c                 flip a-nrm
      al=-al
      bl=-bl
      dis=-dis
25    if(dis.lt.b(7)+.0005)goto 30
c                 error. line does not intersect circle.
      ifl(2)=163
      goto 99
30    h=0.
      hsq=b(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
c                 use 1st inters pt ??????????
      xnd=b(1)-dis*al-co*h
      ynd=b(2)-dis*bl-si*h
99    return
      end
