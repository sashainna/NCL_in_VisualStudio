C*********************************************************************
C*    NAME         :  pocket.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       pocket.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:25
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pocket
c*     process pocket command
c*     
c*     ******************************************************************
c*     *                                                               **
c*     *      add tipped tool feature      pem      1-nov-1984         **
c*     *                                                               **
c*     ******************************************************************
c*
c*             sc(10-18) holds asn + 8 params.
c*             pt addrs are in next unused ranfil pg.
c*
c*          command was pocket/re,c,f,  f1,f2,f3,  o,p, pt1,,,,ptn
c*
c*     1       re = effective radius.
c*     2       c=offset during pocketing (actual offset. not d/2 factor)
c*     3       f=finish cut amt.   (zero if user does not care)
c*     4,5,6   f1,f2,f3  fedrates for plunge,pocketing,finish.
c*     7       o = coverage test flag.   = 0, test        = 1, no test
c*     8       p=2  input points are toolend pts and ps plane prevails.
c*              =3    "     "     "  pocket corners  "    "       "
c*            ( p8=0,1  not included)
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
      subroutine pocket

      include 'com4a.com'
      include 'mocom.com'

      common/pokom/tdat,bu,x(20),y(20),a(20),b(20),c(20)
     1,hx(20),hy(20),rgt,amt,finamt,dist,salmin,psk,dnat
     2,npts,inpts,npold,ibux,stepno,first

      real*8 asn,tdat(3),bj(35)
      real*4 ad(300),bu(900),z(20)
      integer*2 ksn(4),kd(600),stepno
      equivalence (asn,bsn,ksn),(d,ad,kd)
      logical first
      integer*2 ietype
      logical trflg
      integer*4 nclkey
      integer*2 izro,iwlev
      data izro /0/
      data iwlev /0/

      if (isc10(2).eq.2 .or. isc10(2).eq.3) then
        call razpok (izro,iwlev)
c       if (ifl(2).lt.0 .and. sc(169).ge.9.15d0 .and.
        if (ifl(2).lt.0 .and. 
     -     ityp.eq.2 .and. ist.eq.1) ist = 22
        goto 999
      endif
c
c          pocket requires:
c             1.  ta/same.
c             2.  psis reasonable plane per ta

c              init.
      itim=0
      jtim=0
      ibux=0
      npold=0
c               icant flag.                           1-nov-1984
c                if cant case, matrix goes in d(71-79)
      kd(280)=0
      ad(300)=0.
c              do some initial chks
      if(ifl(23).ne.0.or.kd(1).ne.6) ifl(2)=207
c          direct pspl up and modify to contain te point.
      co=sc(4)*d(3)+sc(5)*d(4)+sc(6)*d(5)
      if(abs(co).lt..2) ifl(2)=208
      if(ifl(2).gt.0)goto 999
      if(co.gt.0.)goto 20
c          flip pspl
      co=-co
      do 15 i=3,6
15    d(i)=-d(i)
c          psk is pl const thru te per cutter geom and pthick
20    if(co.gt.1.)co=1.
      si=sqrt(1.-co**2)
      poff=sc(23)+tool(2)*(1.-co)+tool(6)*si
      psk=d(6)+poff
c          do some cursory checks on 8 scalars
      if(sc(12).lt..002.or.sc(18).lt.2.) goto 980
      finamt=sc(13)
c
c          get pt addresses from 'next' ranfil rec
      irec=ifl(4)+1
      call getran (bj,irec)
c
c          get pts into x,y tbl  (z-vals calc later in motion)
      asn=sc(10)
      inpts=ksn(3)
      npts=inpts
      do 30 i=1,npts
      asn=bj(i)
cuni      call getent(tdat,3,ksn(1),ksn(2),3)
      trflg = .true.
      call gtentt(asn, trflg, nclkey, ietype, tdat)
      x(i)=tdat(1)
      y(i)=tdat(2)
      z(i)=tdat(3)
30    continue
c

c               if canted case, wrt mx and conv ps plane and pts.  1-nov-84
      if(dabs(sc(6)).gt..99999) goto 309
      kd(280)=1
      d(74)=2.
      d(75)=3.
      d(76)=4.
301   d(77)=sc(4)
      d(78)=sc(5)
      d(79)=sc(6)
c                 xc-axis   (arbitrary)
      d(71)=d(75)*d(79)-d(76)*d(78)
      d(72)=d(76)*d(77)-d(74)*d(79)
      d(73)=d(74)*d(78)-d(75)*d(77)
      sec=dsqrt(d(71)**2+d(72)**2+d(73)**2)
      if(sec.gt.0.)goto 302
      d(76)=5.
      goto 301
c                 normalize xc
302   d(71)=d(71)/sec
      d(72)=d(72)/sec
      d(73)=d(73)/sec
c               now yc-axis
      d(74)=d(73)*d(78)-d(72)*d(79)
      d(75)=d(71)*d(79)-d(73)*d(77)
      d(76)=d(72)*d(77)-d(71)*d(78)
c               sto pspl in d(81,2,3) and rot d(3,4,5)
c                restore at exit to preserve ps plane
      d(81)=d(3)
      d(82)=d(4)
      d(83)=d(5)
      d(3)=d(71)*d(81)+d(72)*d(82)+d(73)*d(83)
      d(4)=d(74)*d(81)+d(75)*d(82)+d(76)*d(83)
      d(5)=d(77)*d(81)+d(78)*d(82)+d(79)*d(83)
c           rot pts to c-sys
      do 304 i=1,npts
      xi=d(71)*x(i)+d(72)*y(i)+d(73)*z(i)
      yi=d(74)*x(i)+d(75)*y(i)+d(76)*z(i)
      zi=d(77)*x(i)+d(78)*y(i)+d(79)*z(i)
      x(i)=xi
      y(i)=yi
304   z(i)=zi
309   continue

c              set rgt per pts 1,2,3
      rgt=1.
      aa=x(2)-x(1)
      bb=y(2)-y(1)
      drgt=bb*(x(3)-x(2))-aa*(y(3)-y(2))
      if(drgt)32,31,33
c              error.  first 3 pts in-line
31    ifl(2)=163
      goto 999

32    rgt=-1.
c                init the abc set
33    call pokabc
c
c              kolaps  zero or tooldia/2 to start the process
      dist=0.
      if(sc(18).eq.3.)dist=tool(1)/2.
      call kolaps(2)
c              if finish amt given, sto xy in hxy and collapse to f-ring
34    if(finamt.lt..001) goto 40
      do 35 i=1,npts
      hx(i)=x(i)
35    hy(i)=y(i)
      dist=finamt
      call kolaps(2)
c
c  ******  collapse successively while adding cl pts to bu-tbl
c             init misc.
40    effr=tool(1)/2.
      if(sc(11).gt.0.)effr=sc(11)
      off7=sc(12)*.7071
      if(effr.gt.off7)effr=off7
      if(effr.lt..001)effr=sc(12)/2.
c
c             add xy pts to b/u list (if room and no error)
50    if(ibux.gt.890.or.ifl(2).gt.0)goto 80
      do 52 i=1,npts
      bu(ibux+1)=x(i)
      bu(ibux+2)=y(i)
52    ibux=ibux+2
      ibux=ibux+1
      kd(600)=npts
      bu(ibux)=ad(300)
c               if npts .lt.3, all done
      if(npts.lt.3)goto 80

cxxxxx          temp safety exit
      itim=itim+1
      if(itim.gt.500)goto 80
cxxxxx
c              set dist this pass
54    dist=effr*(1.+salmin)
      if(dnat.lt.dist*1.5) goto 60
c              ok to do next cl set.  limit dist to offset
      if(dist.gt.sc(12)) dist=sc(12)
      call kolaps(1)
      goto 50
c              kolaps effr for island pts
60    dist=effr

      call kolaps(2)
c              if island not real, all done
      if(npts.lt.3)goto 80
c              check for 1 or 2 pt inner ring possibility (if npts small)
      if(npts.gt.8) goto 64
c
c                  find long side
      disq=0.
      do 602 i=1,npts
      j=i+1
      if(j.gt.npts)j=1
      dsq=(x(j)-x(i))**2+(y(j)-y(i))**2
      if(dsq.le.disq)goto 602
      disq=dsq
      l=i
602   continue
c                  now find hmax, xcmin, xcmax per same
      hmax=0.
      xcmin=1000.
      xcmax=-1000.
      fx=-b(l)*rgt
      fy=a(l)*rgt
      do 604 i=1,npts
      h=a(l)*x(i)+b(l)*y(i)-c(l)
      if(h.gt.hmax)hmax=h
      xc=fx*(x(i)-x(l))+fy*(y(i)-y(l))
      if(xc.gt.xcmax)xcmax=xc
      if(xc.lt.xcmin)xcmin=xc
604   continue
c                  if hmax.gt.2*effr, no simplification is possible
      if(hmax.gt.2.*effr)goto 64
c                  do midpt, chk for 1pt coverage
      xc=(xcmax+xcmin)/2.
      span=xcmax-xcmin
      h=hmax/2.
      xm=x(l)+fx*xc+a(l)*h
      ym=y(l)+fy*xc+b(l)*h
      effrsq=effr**2
      do 606 i=1,npts
      dx=xm-x(i)
      dy=ym-y(i)
      dsq=dx**2+dy**2
      if(dsq.gt.effrsq)goto 608
606   continue
c                   all pts within effr of midpt.
      x(1)=xm
      y(1)=ym
      npts=1
      goto 50
c                   do 2 pts for this island
c                    (first see where pt(1) is from midpt)
608   xc1=fx*(x(1)-xm)+fy*(y(1)-ym)
      if(xc1.ge.0.)goto 609
c                   flip fx,fy
      fx=-fx
      fy=-fy
609   del=sqrt(effr**2-h**2)
      dis=span/2.-del
      x(1)=xm+dis*fx
      x(2)=xm-dis*fx
      y(1)=ym+dis*fy
      y(2)=ym-dis*fy
      npts=2
      goto 50
c                   kolaps in until sum move = effr
64    rem=effr
      if(rem.gt.sc(12))rem=sc(12)
65    dist=rem*salmin
      jtim=jtim+1
      if(jtim.gt.500)goto 80
      if(dist.gt.dnat)dist=dnat
      if(dist.gt.sc(12))dist=sc(12)
      rem=rem-dist/salmin
      call kolaps(3)
      if(npts.lt.3)goto 50
      if(rem.gt..001)goto 65
      goto 50
c             all done.  do finup work
80    call pockmo
      goto 999
c                 error exit
980   if(ifl(2).lt.1) ifl(2)=177

999   continue
c                 if cant case, restore ps plane
      if(kd(280).eq.0)goto 9999
      d(3)=d(81)
      d(4)=d(82)
      d(5)=d(83)
9999  return
      end

C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtclpl(nam,sub,clpl)
c*     process pocket command
c*     
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
      subroutine gtclpl(nam,sub,clpl)

      include 'com.com'
 
      character*64 nam
      integer*4 sub
      real*8 clpl(4)
      
      integer*2 izro /0/

      token2 = nam
      ivxsub = sub
      call vstchk
c
c... Check if the geometry is a plane
c
      if (ityp.ne.2 .or. (geotyp.ne.plane .and. geotyp.ne.SURF)) then
           ifl(2) = 19
           call expnm2 (nam,sub, l, errcom)
        else
           call gtplt (tv, izro, clpl)
        endif

        return
        end
