
C*********************************************************************
C*    NAME         :  cirprg.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cirprg.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cirprg
C*        handles circle cases 3-6                
C*
C*          canon form is: x,y,z   ctr pt
C*                         a,b,c   'up'
C*                         radius
C*                         a,b,c,d  rgt pl
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
      subroutine cirprg

      include 'com8a.com'

      common/wblok/w(600)

      real*8 u(11),v(11)
      integer*2 ksn(4)
      integer*4 nclkey
      integer*2 ietype
      logical trflg
      equivalence (asn,ksn),(w(20),u),(w(40),v)

c          zero w(1-11)
      do 20 i=1,11
20    w(i)=0.
c          get subtype and branch
      trflg = .true.
      asn=sc(10)
      isub=ksn(2)
      if(isub-4)30,40,49
c******************************  circle/x,y,r
30    w(1)=sc(11)
      w(2)=sc(12)
      w(7)=sc(13)
      if(ksn(3).ne.4)goto 35
c                          ****  circle/x,y,z,r    3-may-83
      w(3)=sc(13)
      w(7)=sc(14)
35    w(6)=1.
c          radius must be positive
      if(w(7).le.0.)ifl(2)=165
      goto 99
c******************************  ci/xyls,ln1,xyls,ln2,radius,a
c          get ln1 in w(21), ln2 in w(31)
40    continue
      call gtentt(sc(12), trflg, nclkey, ietype, w(21))
      a1=-w(25)
      b1=w(24)
      c1=a1*w(21)+b1*w(22)
      sec=dsqrt(a1**2+b1**2)
c          sec must be real
      if(sec.gt.1.d-6)goto 42
c          error. line appears on end.
41    ifl(2)=166
      goto 99
42    imod=sc(11)
c          radius must be real
      if(sc(15).gt.0.)goto 43
425   ifl(2)=165
      goto 99
c          flip normal if indicated
43    if(imod.eq.638.and.a1.lt.0.)sec=-sec
      if(imod.eq.639.and.b1.lt.0.)sec=-sec
      if(imod.eq.641.and.a1.gt.0.)sec=-sec
      if(imod.eq.642.and.b1.gt.0.)sec=-sec
c give an error if x modifier relates to line parallel to x axis
c or y modifier relates to line parallel to y axis
      if (((imod.eq.638.or.imod.eq.641).and.a1.eq.0)
     1  .or.((imod.eq.639.or.imod.eq.642).and.b1.eq.0)) then
         ifl(2)=258
         goto 99
      endif
      a1=a1/sec
      b1=b1/sec
      c1=c1/sec+sc(15)
c          ditto ln2   load in w(31)
      call gtentt(sc(14), trflg, nclkey, ietype, w(31))
      a2=-w(35)
      b2=w(34)
      c2=a2*w(31)+b2*w(32)
      sec=dsqrt(a2**2+b2**2)
      if(sec.lt.1.d-6)goto 41
      imod=sc(13)
      if(imod.eq.638.and.a2.lt.0.)sec=-sec
      if(imod.eq.639.and.b2.lt.0.)sec=-sec
      if(imod.eq.641.and.a2.gt.0.)sec=-sec
      if(imod.eq.642.and.b2.gt.0.)sec=-sec
c give an error if x modifier relates to line parallel to x axis
c or y modifier relates to line parallel to y axis
      if (((imod.eq.638.or.imod.eq.641).and.a2.eq.0)
     1  .or.((imod.eq.639.or.imod.eq.642).and.b2.eq.0)) then
         ifl(2)=258
         goto 99
      endif
      a2=a2/sec
      b2=b2/sec
      c2=c2/sec+sc(15)
c          intersect 2 lns for circ center
      den=a1*b2-a2*b1
      if(dabs(den).gt.1.d-6)goto 44
c          error. parlel lines
      ifl(2)=167
      goto 99
44    xctr=(b2*c1-b1*c2)/den
      yctr=(a1*c2-a2*c1)/den
c          ok plane normal
      pa=-a1-a2
      pb=-b1-b2
      sec=dsqrt(pa**2+pb**2)
      pa=pa/sec
      pb=pb/sec
      cosa=a1*pa+b1*pb
      pd=pa*xctr+pb*yctr-cosa*sc(15)
c          load data in w(1,11)
      w(1)=xctr
      w(2)=yctr
      w(6)=1.
c                 w(6) may be logically -1.0
      up=pa*(b1-b2)-pb*(a1-a2)
      if(up.lt.0.)w(6)=-1.
      w(7)=sc(15)
      w(8)=pa
      w(9)=pb
      w(11)=pd
      goto 99
c          continue subtype branching
49    if(isub-6)50,60,90
c***************************  ci/xyls,inout,ci1,inout,ci2,radius,a
c          get 2 circles   into u and v
50    continue
      call gtentt(sc(13), trflg, nclkey, ietype, u(1))
      call gtentt(sc(15), trflg, nclkey, ietype, v(1))
      r1=u(7)
      r2=v(7)
      if(u(4)**2+u(5)**2.gt.1.d-4)goto 51
      if(v(4)**2+v(5)**2.lt.1.d-4)goto 52
c          error. tipped circle(s)
51    ifl(2)=161
      goto 99
c          correct r1,r2 by rval in/out    (rval must be real)
c         note.  'out' means this circle will be external to
c                ci1,ci2 resp.  'in' means overlaying case.
52    rval=sc(16)
      if(rval.le.0.)goto 425
      if(sc(12).eq.652.)rval=-rval
      u(7)=dabs(u(7)+rval)
      rval=sc(16)
      if(sc(14).eq.652.)rval=-rval
      v(7)=dabs(v(7)+rval)
c          chord ctr-to-ctr
      dx=v(1)-u(1)
      dy=v(2)-u(2)
      chsq=dx**2+dy**2
      chd=dsqrt(chsq)
      if(chd.gt.1.d-3)goto 53
c          error. ci1 and ci2 are concentric
      ifl(2)=168
      goto 99
53    dd=( u(7)**2+chsq-v(7)**2 )/(2.*chd)
      hsq=u(7)**2-dd**2
      if(dabs(hsq).gt.1.d-6)goto 54
      hsq=0.
      goto 55
54    if(hsq.gt.0.)goto 55
c          error. no solution
      ifl(2)=163
      goto 99
55    h=dsqrt(hsq)
c          choose between r/l for circ center
      rgt=1.
      dx=dx/chd
      dy=dy/chd
      rx=dy
      ry=-dx
      imod=sc(11)
      if(imod.eq.638.and.rx.lt.0.)rgt=-1.
      if(imod.eq.639.and.ry.lt.0.)rgt=-1.
      if(imod.eq.641.and.rx.gt.0.)rgt=-1.
      if(imod.eq.642.and.ry.gt.0.)rgt=-1.
      rx=rx*rgt
      ry=ry*rgt
c          stepout to ctr pt
      w(1)=u(1)+dx*dd+rx*h
      w(2)=u(2)+dy*dd+ry*h
      w(6)=1.
      w(7)=sc(16)
c          finup valid pl.  (u7,v7 still hold r1,r2)
      dx=w(1)-u(1)
      dy=w(2)-u(2)
      if(sc(12).eq.652..and.w(7).gt.r1)r1=-r1
      sec=dsqrt(dx**2+dy**2)
      if(sec.gt.0.)goto 58
c          error.  something phony.
57    ifl(2)=163
      goto 99
c               vector from ra ctr to ci1 pt
58    x1=u(1)+dx*r1/sec
      y1=u(2)+dy*r1/sec
      ax=(x1-w(1))/w(7)
      ay=(y1-w(2))/w(7)
c               ve from ra ctr to ci2 pt
      dx=w(1)-v(1)
      dy=w(2)-v(2)
      if(sc(14).eq.652..and.w(7).gt.r2)r2=-r2
      sec=dsqrt(dx**2+dy**2)
      if(sec.le.0.)goto 57
      x2=v(1)+dx*r2/sec
      y2=v(2)+dy*r2/sec
      bx=(x2-w(1))/w(7)
      by=(y2-w(2))/w(7)
c                ave. vector
      ax=ax+bx
      ay=ay+by
      sec=dsqrt(ax**2+ay**2)
      if(sec.le.0.)goto 57
      w(8)=ax/sec
      w(9)=ay/sec
      w(10)=0.
      w(11)=w(8)*x1+w(9)*y1
      goto 99
c********************************  ci/xyls,ln1,xyls,inout,ci1,radius,a
c              get ln1 in v, ci1 in u
60    continue
      call gtentt(sc(12), trflg, nclkey, ietype, v(1))
      call gtentt(sc(15), trflg, nclkey, ietype, u(1))
      rval=sc(16)
c          check for projected circle, line on end, neg radius
      if(u(4)**2+u(5)**2.gt.1.d-4) ifl(2)=161
      if(v(4)**2+v(5)**2.lt.1.d-4) ifl(2)=166
      if(rval.le.0.) ifl(2)=165
      if(ifl(2).gt.0)goto 90
c          do ln a,b,c
      a=v(5)
      b=-v(4)
      sec=dsqrt(a**2+b**2)
      imod=sc(11)
      if(imod.eq.638.and.a.lt.0.)sec=-sec
      if(imod.eq.639.and.b.lt.0.)sec=-sec
      if(imod.eq.641.and.a.gt.0.)sec=-sec
      if(imod.eq.642.and.b.gt.0.)sec=-sec
      a=a/sec
      b=b/sec
      c=a*v(1)+b*v(2)+rval
c          ci1 in/out by rval
      r1=u(7)+rval
      imod=sc(14)
      if(imod.eq.652) r1=u(7)-rval
      dd=c-a*u(1)-b*u(2)
      hsq=r1**2-dd**2
      if(dabs(hsq).gt.1.d-6)goto 62
c          hsq very small, let h=0.
      h=0.
      goto 64
c          if hsq neg, no solution
62    if(hsq.gt.0.)goto 63
      ifl(2)=163
      goto 90
63    h=dsqrt(hsq)
64    aa=-b
      bb=a
      sig=1.
      imod=sc(13)
      if(imod.eq.638.and.aa.lt.0.)sig=-1.
      if(imod.eq.639.and.bb.lt.0.)sig=-1.
      if(imod.eq.641.and.aa.gt.0.)sig=-1.
      if(imod.eq.642.and.bb.gt.0.)sig=-1.
      aa=aa*sig
      bb=bb*sig
      w(1)=u(1)+a*dd+aa*h
      w(2)=u(2)+b*dd+bb*h
      w(6)=1.
      w(7)=sc(16)
c              valid range.
      sig=1.
      c=a*v(1)+b*v(2)
      if(a*w(1)+b*w(2)-c.gt.0.)sig=-1.
      ax=a*sig
      ay=b*sig
      dx=w(1)-u(1)
      dy=w(2)-u(2)
      sec=dsqrt(dx**2+dy**2)
      if(sec.le.0.)goto 57
      xpt=u(1)+u(7)*dx/sec
      ypt=u(2)+u(7)*dy/sec
      bx=xpt-w(1)
      by=ypt-w(2)
      sec=dsqrt(bx**2+by**2)
      if(sec.le.0.)goto 57
      ax=ax+bx/sec
      ay=ay+by/sec
      sec=dsqrt(ax**2+ay**2)
      if(sec.le.0.)goto 57
      w(8)=ax/sec
      w(9)=ay/sec
      w(10)=0.
      w(11)=w(8)*xpt+w(9)*ypt
      goto 99
c          error exit
90    if(ifl(2).le.0)  ifl(2)=5
99    continue

999   return
      end
