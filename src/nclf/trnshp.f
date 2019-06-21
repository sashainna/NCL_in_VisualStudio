C*********************************************************************
C*    NAME         :  trnshp.f
C*       CONTAINS:
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       trnshp.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:49
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine trnshp
C*      Transform a shape through a matrix.
C*    PARAMETERS   
C*       INPUT  : 
C*          p         - Array holding shape.
C*          mx        - Transformation matrix.
C*       OUTPUT :  
C*          p         - Transformed shape.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine trnshp (p,mx)

      include 'com.com'
      include 'const.com'

      real*8 p(1000), mx(12)

      real*8 buf(3), pt(3), sec, xnd, ynd, delb, dx, dy
      integer*2 nwds,ix
      real*8 asn, pang
      real*4 ang(2)
      integer*2 ksn(4)
      equivalence (asn,ksn), (pang,ang)
      integer*2 i2v3, i2v4
      data i2v3 /3/, i2v4 /4/
 
      asn = p(1)
      nwds = ksn(3)
c
c... Transform start plane
c
      buf(1) = p(2)
      buf(2) = p(3)
      buf(3) = 0.0d0
      pt(1) = p(2)*p(4)
      pt(2) = p(3)*p(4)
      pt(3) = 0.0d0
      call conent (buf, mx, i2v4)
      call conent (pt, mx, i2v3)
      sec = dsqrt(buf(1)**2+buf(2)**2)
      if (sec.gt.0.0d0) then
        p(2) = buf(1)/sec
        p(3) = buf(2)/sec
        p(4) = p(2)*pt(1)+p(3)*pt(2)
      endif
c
c... Transform end plane
c
      buf(1) = p(5)
      buf(2) = p(6)
      buf(3) = 0.0d0
      pt(1) = p(5)*p(7)
      pt(2) = p(6)*p(7)
      pt(3) = 0.0d0
      call conent (buf, mx, i2v4)
      call conent (pt, mx, i2v3)
      sec = dsqrt(buf(1)**2+buf(2)**2)
      if (sec.gt.0.0d0) then
        p(5) = buf(1)/sec
        p(6) = buf(2)/sec
        p(7) = p(5)*pt(1)+p(6)*pt(2)
      endif

      ix = 8
10    asn = p(ix)
      if (ksn(4).eq.5) goto 20
      if (ksn(4).eq.7) goto 30
      goto 40
c
c... Transform point
c
20    buf(1) = p(ix+1)
      buf(2) = p(ix+2)
      buf(3) = 0.0d0
      call conent (buf, mx, i2v3)
      xnd = buf(1)
      ynd = buf(2)
      p(ix+1) = xnd
      p(ix+2) = ynd
      goto 40
c
c... Transform circle
c
30    buf(1) = p(ix+1)
      buf(2) = p(ix+2)
      buf(3) = 0.0d0
      call conent (buf, mx, i2v3)
      p(ix+1) = buf(1)
      p(ix+2) = buf(2)
      buf(1) = p(ix+3)
      buf(2) = p(ix+4)
      buf(3) = 0.0d0
      call conent (buf, mx, i2v3)
      p(ix+3) = buf(1)
      p(ix+4) = buf(2)
      dx = xnd-buf(1)
      dy = ynd-buf(2)
      p(ix+5) = dsqrt(dx**2+dy**2)
      if (dabs(dy).lt..0001) dy=0.
      ang(1)=0.
      if (dx**2+dy**2.gt.1.d-6) ang(1)=datan2(dy,dx)
      if (ang(1).lt.0.) ang(1)=ang(1)+TWO_PI
      xnd = p(ix+1)
      ynd = p(ix+2)
      dx = xnd-buf(1)
      dy = ynd-buf(2)
      if (dabs(dy).lt..0001) dy=0.
      ang(2)=0.
      if (dx**2+dy**2.gt.1.d-6) ang(2)=datan2(dy,dx)
      if (ang(2).lt.0.) ang(2)=ang(2)+TWO_PI
      p(ix+6) = pang
      delb=ang(2)-ang(1)
      if (ksn(2)*delb.lt.0.) then
        ksn(2) = -ksn(2)
        p(ix)  = asn
      endif

40    continue
      ix = ix+ksn(3)+1
      if (ix.lt.nwds) goto 10

999   return
      end
