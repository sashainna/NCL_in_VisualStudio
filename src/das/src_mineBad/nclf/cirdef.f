c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       cirdef.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:41
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: cirdef                                         **
c **                                                                  **
c **  purpose of subroutine: calc 3 pts for redef of ci1              **
c **              written 12-nov-86                                   **
c **********************************************************************
c **********************************************************************
c
c          input:       arg1   ci1  entity to be chgd
c                       arg2   entity used to redef ci1
c                       imod1  xyls for trim.
c                       imod2  clw or cclw   
 
      subroutine cirdef( arg1,arg2,arg3,imod1,imod2)
 
      include 'com8a.com'
 
      integer*2 isc(100),nw,pg,el,peq(4),ksn(4)
      real*8 w(12),pgelnw,u(12)
      equivalence (sc(10),isc),(peq(1),pgelnw)
      equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3)),(asn,ksn)
      integer*4 nclkey
      integer*2 ietype
c      integer*2 nwds
      logical trflg

      trflg=.true.
      ispec=0

c                  get ci1 in w-tbl
      pgelnw=arg1
cuni      call getent(w,nw,pg,el,7)
c      call gtgeom(arg1,w,nclkey,nwds,ietype)
      call gtentt(arg1, trflg, nclkey, ietype, w)
c                  ci1 must not be tipped
      rad=w(7)
      if(dabs(w(6)).gt..999)goto 20
      ifl(2)=161
      goto 998


c                  if partial circle, set ireal=1 and calc endpts
20    ireal=0
      if(w(8)**2+w(9)**2.lt..9) goto 25
      ireal=1
      dis=w(11)-w(8)*w(1)-w(9)*w(2)
      hsq=rad**2-dis**2
      h=0.
      if(hsq.gt.0.)h=dsqrt(hsq)
      xa=w(1)+w(8)*dis-w(9)*h
      ya=w(2)+w(9)*dis+w(8)*h
      xb=xa+2.*h*w(9)
      yb=ya-2.*h*w(8)
c                get  2nd entity in u-tbl
25    asn=arg2
      i2type=ksn(4)
cuni      call getent(u,ksn(3),ksn(1),ksn(2),ksn(4))
c      call gtgeom(asn,u,nclkey,nwds,ietype)
      call gtentt(asn, trflg, nclkey, ietype, u)
      if(i2type.ne.3)goto 40

c   ********************************       entity 2 is a point
      if(ireal.gt.0) goto 30
c                  360 degree circle is ng with pt ent2
      ifl(2)=323
      goto 998
c                  calc ipt per pt1
30    dx=u(1)-w(1)
      dy=u(2)-w(2)
      dis=dsqrt(dx**2+dy**2)
      if(dis.eq.0.)goto 998
      ro=w(7)/dis
      xi=w(1)+ro*dx
      yi=w(2)+ro*dy
31    itrim=0
      dtr=w(8)*xi+w(9)*yi-w(11)
c                  if dtr positive, ipt is in real part of ci1. task is trim
      if(dtr.ge.0.)itrim=1
c                  assume pta-ipt is resultant ci1.  check vs ptb +  itrim
c                   and switch to ptb-ipt if nec.

      
c                   dir indicated by imod2 clw or cclw
315   csig=1.
      if(imod2.eq.0)imod2=imod1
      if(imod2.ne.59)csig=-1.
      xcl=-dy*csig
      ycl=dx*csig
      dax=xi-xa
      day=yi-ya
      xai=-day
      yai=dax
      xo=xa
      yo=ya
c                   if this direc opposite xcl-ycl, flip it.
      if(xai*xcl+yai*ycl.gt.0.)goto 32
      xai=-xai
      yai=-yai
32    cai=xai*xi+yai*yi
      if(ispec.eq.1)goto 36

c                     check vs. ptb
      bdis=xai*xb+yai*yb-cai
c                  if bdis + and task is extend, all ok (I.E. pta is it)
      if(bdis.gt.0..and.itrim.eq.0)goto 36
c                  ditto if bdis - and task is trim
      if(bdis.lt.0..and.itrim.eq.1)goto 36
c                     bpt ipt is the correct combo.
      xo=xb
      yo=yb
      dax=xi-xb
      day=yi-yb
      xai=-day
      yai=dax
      if(xai*xcl+yai*ycl.gt.0.)goto 34
      xai=-xai
      yai=-yai
34    cai=xai*xi+yai*yi
c                      calc a midpt and load 3 pts in sc(11-19)
36    sec=dsqrt(xai**2+yai**2)
      if(sec.eq.0.)goto 998
      if(ifl(2).gt.0)goto 998
      sc(11)=xo
      sc(12)=yo
      sc(13)=w(3)
      sc(14)=xai*rad/sec+w(1)
      sc(15)=yai*rad/sec+w(2)
      sc(16)=w(3)
      sc(17)=xi
      sc(18)=yi
      sc(19)=w(3)
c                      fix sc(10) to   604-1-0-0
      asn=0.
      ksn(1)=604
      ksn(2)=1
c                      ksn(3)=9876 for geogn2
      ksn(3)=9876
      sc(10)=asn
      goto 999

40    if(i2type.ne.5)goto 60
c **********************************************     ent2 is line
c                        set direc h per mod1
      delx=u(4)
      dely=u(5)
      secln=dsqrt(delx**2+dely**2)
      if(secln.gt.1.d-6)goto 42
c                        error. line appears vertical
41    ifl(2)=44
      goto 998
42    dist=(delx*(u(2)-w(2))-dely*(u(1)-w(1)))/secln
      crwn=rad-dabs(dist)
c                        if crown very small, this is tancase
      if(dabs(crwn).gt..001)goto 45
c                         tancase.  if ent3 also line tangent to ci,
c                                   calc 3 pts here.
      asn=arg3
      if(ksn(4).ne.5)goto 43
c                              tanpt to ln1 from above info
c                        make dist=rad
      dist=rad*dist/dabs(dist)
      xi=w(1)-dely/secln*dist
      yi=w(2)+delx/secln*dist
c                              get ln2 in u-tbl
cuni      call getent(u,6,ksn(1),ksn(2),ksn(4))
c      call gtgeom(asn,u,nclkey,nwds,ietype)
      call gtentt(asn, trflg, nclkey, ietype, u)
      secln=dsqrt(u(4)**2+u(5)**2)
      if(secln.lt.1.d-6)goto 41
      dist=(u(4)*(u(2)-w(2))-u(5)*(u(1)-w(1)))/secln
      crwn=rad-dabs(dist)
      if(dabs(crwn).gt..001)goto 998
c                                 make dist=rad
      dist=rad*dist/dabs(dist)
      xo=w(1)-u(5)*dist/secln
      yo=w(2)+u(4)*dist/secln
c                            now do midpt per imod1 or lesser arc
      if(imod1.gt.0)goto 425
c                            direc not given. do lesser arc ai-plane
      xai=(xi+xo)/2.-w(1)
      yai=(yi+yo)/2.-w(2)
      goto 36
425   drx=0.
      dry=0.
      if(imod1.eq.638)drx=1.
      if(imod1.eq.641)drx=-1.
      if(imod1.eq.639)dry=1.
      if(imod1.eq.642)dry=-1.
      xai=yo-yi
      yai=xi-xo
      if(xai*drx+yai*dry.ge.0.)goto 36
      xai=-xai
      yai=-yai
      goto 36
c                          one line tancase. ci1 must be real. 
c                          set ipt = tanpt and use ent2 point logic.
43    if(ireal.ne.0)goto 44
      ifl(2)=323
      goto 998
44    xi=w(1)-dely/secln*dist
      yi=w(2)+delx/secln*dist
      dx=xi-w(1)
      dy=yi-w(2)
      goto 31
c                      if crown neg, no inters possible

45    if(crwn.gt.0.) goto 48
c                       error. no inters possible.
      ifl(2)=163
      goto 998
c                       2 inters pts.  set mod1 direction.
48    drx=0.
      dry=0.
      if(imod1.eq.638)drx=1.
      if(imod1.eq.641)drx=-1.
      if(imod1.eq.639)dry=1.
      if(imod1.eq.642)dry=-1.
      h=0.
      hsq=rad**2-dist**2
      if(hsq.gt.0.)h=dsqrt(hsq)
c                  check mod1 vs. ln1. flip h if indicated.
      coa=drx*delx+dry*dely
      if(dabs(coa).gt..001)goto 50
c                  error. mod2 is perpto ln1
      ifl(2)=163
      goto 998
50    if(coa.lt.0.)h=-h
      xi=w(1)+(-dely*dist+delx*h)/secln
      yi=w(2)+(delx*dist+dely*h)/secln
      dx=xi-w(1)
      dy=yi-w(2)
c                  if 360 circle, set pta = other inters pt
      if(ireal.eq.1)goto 31
      xa=xi-2.*delx*h/secln
      ya=yi-2.*dely*h/secln
      ispec=1
      itrim=1
      goto 315
c                  see if ent2 is circle
60    if(i2type.eq.7)goto 62
c                    error. ent2 must be pt,ln,ci
      ifl(2)=5
      goto 998
c****************************************ent2 is circle
c                                check for tipped
62    if(dabs(u(6)).gt..999)goto 64
c                    error. ci2 is tipped.
      ifl(2)=161
      goto 998
64    dcx=u(1)-w(1)
      dcy=u(2)-w(2)
      chdsq=dcx**2+dcy**2
      chd=dsqrt(chdsq)
c                     if chd = 0, circles are concentric
      if(chd.lt..001)ifl(2)=168
       if(ifl(2).gt.0)goto 998
      dcx=dcx/chd
      dcy=dcy/chd
      r2=u(7)
c                     dist from ci1 cept to intercept of 2 circles.
      dist=(rad**2+chdsq-r2**2)/(2.*chd)
      h=0.
      hsq=rad**2-dist**2
      if(hsq.gt.0.)h=dsqrt(hsq)
c                     set direc per imod1
      drx=0.
      dry=0.
      if(imod1.eq.638)drx=1.
      if(imod1.eq.641)drx=-1.
      if(imod1.eq.639)dry=1.
      if(imod1.eq.642)dry=-1.
c                      chk drx,dry vs. -dcy,dcx
      coa=-dcy*drx+dcx*dry
      if(dabs(coa).lt..001)ifl(2)=163
       if(ifl(2).gt.0)goto 998
      if(coa.lt.0.) h=-h
c                      calc ipt
      xi=w(1)+dcx*dist-dcy*h
      yi=w(2)+dcy*dist+dcx*h
      dx=xi-w(1)
      dy=yi-w(2)
      if(ireal.eq.1)goto 31
c                       ci1 is 360. tan case not allowed
      if(dabs(h).gt..002)goto 66
      ifl(2)=323
      goto 998
c                     special case.   pta is other inters pt
66    xa=xi+2.*h*dcy
      ya=yi-2.*h*dcx
      ispec=1
      itrim=1
      goto 315

c                  error exit
998   if(ifl(2).lt.1) ifl(2)=163
      err=.true.

999   return
      end
