c*****************************************************
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       lnrdef.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:10:14
c**
c*****************************************************
c**
c** copyright (c) 1981,1982 mills data systems corporation
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: lnrdef                                         **
c **                                                                  **
c **  purpose of subroutine: calc 2 pts for redefinition of ln1       **
c **              written 20-oct-86
c **********************************************************************
c **********************************************************************
c
c          input:       arg1   ln1  entity to be chgd
c                       arg2   entity used to redef ln1
c                       arg3   ent2    "    "   "    "  if given
c                       imod1  xyls of inters with ent2
c                       imod2  xyls of kept part of ent1
c
c                       note:  if imod2 not given, imod1 describes the kept
c                              part of ent1


      subroutine lnrdef( arg1,arg2,arg3,imod1,imod2)
 
      include 'com8a.com'
 
      integer*2 isc(100),nw,pg,el,peq(4),ksn(4)
      real*8 w(12),pgelnw,u(12)
      equivalence (sc(10),isc),(peq(1),pgelnw)
      equivalence (pg,peq(1)),(el,peq(2)),(nw,peq(3)),(asn,ksn)
      real*8 a,b,c,d,den,e,f,xa,ya,za,xyz(3)
      equivalence (xa,xyz(1)),(ya,xyz(2)),(za,xyz(3))
      integer*4 nclkey
      integer*2 ietype 
c      integer*2 nwds
      logical trflg
      
c                    get ln1 into w-tbl
      trflg = .true.
c      pgelnw=arg1
cuni      call getent(w,nw,pg,el,5)
c      call gtgeom(arg1,w,nclkey,nwds,ietype)
      call gtentt(arg1, trflg, nclkey, ietype, w)
c                    2nd entity
      asn=arg2
      if(ksn(4).ne.3)goto 20
c                    ent2 is pt.  read into u-tbl.  auxpt=pt2
cuni      call getent(u,3,ksn(1),ksn(2),3)
c      call gtgeom(asn,u,nclkey,nwds,ietype)
      call gtentt(asn, trflg, nclkey, ietype, u)
      xa=u(1)
      ya=u(2)
      za=u(3)
      goto 40
20    if(ksn(4).ne.5)goto 25
c                    ent2 is line. read into u-tbl, calc ipt
cuni      call getent(u,6,ksn(1),ksn(2),5)
c      call gtgeom(asn,u,nclkey,nwds,ietype)
      call gtentt(asn, trflg, nclkey, ietype, u)
c                    2d intof only
      a=-w(5)
      b=w(4)
      c=a*w(1)+b*w(2)
      d=-u(5)
      e=u(4)
      f=d*u(1)+e*u(2)
      den=a*e-b*d
      if(dabs(den).gt..00001)goto 21
c                        error. lines are parlel
      ifl(2)=167
      goto 998
21    xi=(c*e-b*f)/den
      yi=(a*f-c*d)/den
      xa=xi
      ya=yi
      i = 1
      if (w(4) .eq. 0.d0) i = 2 
      den = dsqrt(w(4)**2+w(5)**2+w(6)**2)
      a = (xyz(i) - w(i))*den/w(3+i)
      za = w(3) + a*w(6)/den
      goto 40

25    if(ksn(4).ne.7)goto 30
c                    ent2 is circle. 
c                     switch imod1 and imod2 if both given.
c                     (chg of mind about their usage)
c      if(imod1.eq.0.or.imod2.eq.0)goto 251
c      kmod1=imod1
c      imod1=imod2
c      imod2=kmod1
c251   continue
c                    get into u-tbl and calc dist cept to line
cuni      call getent(u,11,ksn(1),ksn(2),7)
c      call gtgeom(asn,u,nclkey,nwds,ietype)
      call gtentt(asn, trflg, nclkey, ietype, u)
      rad=u(7)
      a=-w(5)
      b=w(4)
      sec=dsqrt(a**2+b**2)
      if(dabs(sec).lt..001)goto 998
      dist=(a*(w(1)-u(1))+b*(w(2)-u(2)))/sec
      if(dabs(dist).lt.rad-.001)goto 26
c                    ln1 appears tanto or outside ci2.  auxpt=cept
      xa=u(1)
      ya=u(2)
      za=u(3)
      goto 40
c                     intersect ln1,ci2 and use imod2 for new endpt
26    if(imod2.eq.0)imod2=imod1
      h=dsqrt(rad**2-dist**2)
      drx=0.
      dry=0.
      if(imod2.eq.638)drx=1.
      if(imod2.eq.641)drx=-1.
      if(imod2.eq.639)dry=1.
      if(imod2.eq.642)dry=-1.
c                   assume plus drxy pt
      sig=1.
      co=drx*w(4)+dry*w(5)
c                    check for imod2 perpto ln1
      if(dabs(co).lt.1.d-6)goto 64
      if(co.lt.0.)sig=-1.
      a=a/sec
      b=b/sec
      w4=w(4)/sec
      w5=w(5)/sec
      xa=u(1)+a*dist+sig*h*w4
      ya=u(2)+b*dist+sig*h*w5
      za=0.
      goto 40
c                   ent2 must be curve 
30    if(ksn(4).eq.8)goto 31
      ifl(2)=21
      goto 998
c                   ent2 is curve.  prepare for geognd solution
31    kdst=idst
      idst=3
      asn=0.
      ksn(1)=601
      ksn(2)=7
      sc(10)=asn
      sc(11)=arg1
      sc(12)=arg2
      sc(13)=arg3
      call geognd
c
c... if error, go to end - raz
c
      if (ifl(2).ne.0) goto 998
c                    ipt is in sc(15-17)
      idst=kdst
      if(ifl(2).gt.0)goto 998
      xa=sc(15)
      ya=sc(16)
      za=sc(17)
c                   calc ipt per ln1 and auxpt
40    continue
      den=w(4)**2+w(5)**2+w(6)**2
      rho=(w(4)*(xa-w(1))+w(5)*(ya-w(2))+w(6)*(za-w(3)))/den
      xi=w(1)+rho*w(4)
      yi=w(2)+rho*w(5)
      zi=w(3)+rho*w(6)

c                   rho indicates trim or extend
      if(rho.gt..000001d0.and.rho.lt..999999d0)goto 60
c       ********  extend ln1    (kpt=farpt of ln1)
      xk=w(1)
      yk=w(2)
      zk=w(3)
      if(rho.gt..5)goto 80
c                 rho is neg.  kpt is p2 of ln1
      xk=xk+w(4)
      yk=yk+w(5)
      zk=zk+w(6)
      goto 80

c        *******   trim ln1   using imod1=xyls
60    if(imod1.ne.0)goto 61
c                         error. modifier not given
601   ifl(2)=18
      goto 998
61    drx=0.
      dry=0.
      if(imod1.eq.638)drx=1.
      if(imod1.eq.641)drx=-1.
      if(imod1.eq.639)dry=1.
      if(imod1.eq.642)dry=-1.
c                   assume p1 of ln1 is kpt
      xk=w(1)
      yk=w(2)
      zk=w(3)
      if(dabs(drx*w(4)+dry*w(5)).gt.1.d-6)goto 65
c                      error. direc indefinite to ln1
64    ifl(2)=213
      goto 998
65    if(drx*w(4)+dry*w(5).lt.0.)goto 80
c                    seems to be other pt of ln1
      xk=xk+w(4)
      yk=yk+w(5)
      zk=zk+w(6)

c                    finup       put kpt and ipt in sc(11-16)
80    sc(11)=xk
      sc(12)=yk
      sc(13)=zk
      sc(14)=xi
      sc(15)=yi
      sc(16)=zi
      goto 999

c                    error exit.
998   if(ifl(2).lt.1) ifl(2)=163
c
c... if ifl(2) .ne. 0 change error set in geognd() to:
c...   419 = FAILED TO FIND LINE-CURVE INTERSECTION
c
      if (ifl(2).gt.0) ifl(2)=419
      err=.true.
999   return
      end
