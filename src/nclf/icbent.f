
C*********************************************************************
C*    NAME         :  icbent.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       icbent.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:11
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine icbent(kclwa)
C*       intersect a-circle with b-entity.  load result in xnd,ynd.
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
      subroutine icbent(kclwa)
c
      include 'com8a.com'
      common/wblok/w(144),a(12),b(12),xnd,ynd,errb
      common/pblok/p(400)
      integer*2 ksc(500),kw(576),ksn(4),kp(1600),ka(48),kb(48)
      equivalence (sc,ksc),(w,kw),(asn,ksn),(p,kp),(a,ka),(b,kb)
c
      if(kb(48).ne.5) goto 30
c
c                    b-entity is line. if bstpt on a-circle, use it.
      xnd=b(1)
      ynd=b(2)
      dis=dsqrt((a(1)-xnd)**2+(a(2)-ynd)**2)
      if(dabs(a(7)-dis).le..001)goto 99
c                    if a-limpl, calc a-endpt and see if on b-line.
      if((a(8)**2+a(9)**2).lt..001)goto 20
      fx=-a(6)*a(9)
      fy=a(8)*a(6)
      dis=a(11)-a(8)*a(1)-a(9)*a(2)
      h=0.
      hsq=a(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
      xnd=a(1)+a(8)*dis+fx*h
      ynd=a(2)+dis*a(9)+h*fy
      tl=dsqrt(b(4)**2+b(5)**2)
      dif=(-b(5)*(b(1)-xnd)+b(4)*(b(2)-ynd))/tl
      if(dabs(dif).lt..001)goto 99
c                    no endpt match found.  intersect acirc,bline
20    tl=dsqrt(b(4)**2+b(5)**2)
      al=b(4)/tl
      bl=b(5)/tl
      dis=bl*(b(1)-a(1))-al*(b(2)-a(2))
      if(dis.ge.0.)goto 25
      al=-al
      bl=-bl
      dis=-dis
25    h=0.
      hsq=a(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
      if(kclwa.eq.-1)h=-h
      xnd=a(1)+dis*bl+h*al
      ynd=a(2)-al*dis+h*bl
      goto 99
c                   a-circle, b-circle
30    if(a(8)**2+a(9)**2.lt..001)goto 35
      dis=a(11)-a(8)*a(1)-a(9)*a(2)
      h=0.
      hsq=a(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
      fx=-a(6)*a(9)
      fy=a(8)*a(6)
      xnd=a(1)+dis*a(8)+fx*h
      ynd=a(2)+dis*a(9)+fy*h
c                    dist between a-ndpt and bcircle.  if small, use it.
      dif=dsqrt((b(1)-xnd)**2+(b(2)-ynd)**2)-b(7)
      if(dabs(dif).lt..001)goto 99
35    if(b(8)**2+b(9)**2.lt..001)goto 40
      fx=-b(6)*b(9)
      fy=b(8)*b(6)
      dis=b(11)-b(8)*b(1)-b(9)*b(2)
      h=0.
      hsq=b(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
c                     b-stpt.  if on a-circle, use it.
      xnd=b(1)+b(8)*dis-fx*h
      ynd=b(2)+b(9)*dis-fy*h
      dif=dsqrt((a(1)-xnd)**2+(a(2)-ynd)**2)-a(7)
      if(dabs(dif).lt..001)goto 99
c                     general circ-circ intersection
40    dx=b(1)-a(1)
      dy=b(2)-a(2)
      tl=dsqrt(dx**2+dy**2)
      if(tl.gt..001)goto 45
c                     error.  concentric circles.
      ifl(2)=168
      goto 99
45    if(tl.lt.a(7)+b(7)+.001)goto 50
c                     error.  circles do not intersect.
      ifl(2)=163
      goto 99
50    co=(a(7)**2-b(7)**2+tl**2)/(2.*a(7)*tl)
      dis=co*a(7)
      h=0.
      hsq=a(7)**2-dis**2
      if(hsq.gt.0.)h=dsqrt(hsq)
      co=dx/tl
      si=dy/tl
      xnd=a(1)+co*dis-si*h
      ynd=a(2)+si*dis+co*h
99    return
      end
