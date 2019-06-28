C*********************************************************************
C*    NAME         :  rufpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       rufpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:38
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine rufpre
c        called by lthctl to prepare for lthruf execution.
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
      subroutine rufpre
c
      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 asn,e,f,a,b
      real*4 ad(300)
      integer*2 ksc(500),kd(600),ksn(4),ke(4000),kf(4000)
      equivalence (sc,ksc),(d,ad,kd),(asn,ksn),(e,ke),(f,kf)
      integer*4 nclkey
      integer*2 nwds,ietype
c
c
c              lathe/rough only (temp) and numwds=70 max
      nds=ksc(39)
      if(nds.gt.70) goto 98
      if(ksc(38).eq.0) goto 100
c              get lathe/rough list into d(1) from next ranfil rec
      irec=ifl(4)+1
      call getran(d,irec)
c              read again if nec.
      if(nds.lt.36) goto 12
      irec=irec+1
      call getran(d(36),irec)
12    continue
c              pickup cldist,xstk,ystk     (hold sh2 addr in f(400) )
      cldist=d(2)
      xstk=d(4)
      ystk=d(5)
      f(400)=d(3)
c              find 'depth' in d-list    (must be reasonable)
      do 14 i=6,nds
      k=4*i-3
      if(kd(k).eq.510) goto 16
14    continue
c              error.  depth and cutang reqd. in lth/ruf command.
15    ifl(2)=245
      goto 98
16    j=2*i
      depth=ad(j)
c
c       changed error number from 5 to    .kathy
c
      if(depth.lt..001.or.depth.gt.10.) then
          ifl(2)=363 
          goto 99
      endif
      idx=i
c              cont to find 'cutang'   (must be 180 temp.)
      do 18 i=7,nds
      k=4*i-3
      if(kd(k).eq.160)goto 20
18    continue
c              no find.
      goto 15
20    icx=i
      j=2*i
      if(ad(j).eq.180.) goto 22
c              error.  cutang must be 180
      ifl(2)=246
      goto 98
c              cont sch for 'retrct' & 'return', if not at d-list end.
22    ibx=0
      iax=0
      if(icx.ge.nds)goto 32
      do 24 i=icx,nds
      k=4*i-3
      if(kd(k).ne.7) goto 23
      ibx=i
      goto 24
23    if(kd(k).eq.322)goto 30
24    continue
      goto 32
30    iax=i
32    continue
c
c              if any cutang pp's, see if fedrat given
      if(ibx.eq.0)goto 34
      nc=ibx-icx
      goto 38
34    if(iax.eq.0)goto 36
      nc=iax-icx
      goto 38
36    nc=nds-icx
c              if numc lt.2, no pp's in zone-c
38    if(nc.lt.2)goto 44
c              see if any c-fedrat given.
      ist=icx+1
      ind=icx+nc-1
      do 40 i=ist,ind
      k=4*i-3
      if(kd(k).eq.2000.and.kd(k+1).eq.1009)goto 60
40    continue
c              no c-fedrats.  if any in zone-e, sto in f-tbl and then to c.
44    nf=0
      if(idx.lt.7) goto 60
      ind=idx-1
      do 48 i=6,ind
      k=4*i-3
      if(kd(k).ne.2000.or.kd(k+1).ne.1009)goto 48
c              fedrat found in zone-e.   copy to f-tbl
      num=kd(k+2)
      lst=i+num-1
      do 46 l=i,lst
      nf=nf+1
46    f(nf)=d(l)
48    continue
c              if any fedrats in f, dn-shift d-tbl after icx and copy from f
      if(nf.le.0)goto 60
      num=nds-icx
      nds=nds+nf
c              dn-shift num d's
      if(num.lt.1)goto 54
      j=nds+1
      do 50 i=1,num
      j=j-1
50    d(j)=d(j-nf)
c              now f to d-gap just formed
54    j=icx
      do 52 i=1,nf
      j=j+1
52    d(j)=f(i)
c              fix ibx,iax
      if(ibx.gt.0)ibx=ibx+nf
      if(iax.gt.0)iax=iax+nf
c              finup
60    continue
c            get sh1 into f-tbl and fix per rtool-cldist.  put in e-tbl
      asn=d(1)
c      call getent(f,ksn(3),ksn(1),ksn(2),0)
      call gtdesc(asn,nclkey,nwds,ietype)
      call gtshpt(f, nclkey)
      d(1) = f(1)
      call shafix(1)
      nf=kf(3)
      do 72 i=1,nf
72    e(i)=f(i)
c            same for sh2  (leave in f-tbl)
      asn=d(3)
c      call getent(f,ksn(3),ksn(1),ksn(2),0)
      call gtdesc(asn,nclkey,nwds,ietype)
      call gtshpt(f, nclkey)
      d(3) = f(1)
      call shafix(2)
c
c    ***********   finup lth/ruf  d(1-3) and exit  *******
c
c                   if any depth pp's, set idpp.  if any f/r, set idfr
      idfr=0
      idpp=0
      do 73 i=idx,icx
      k=4*i-3
      if(kd(k).ne.2000)goto 73
      idpp=idpp+1
      if(kd(k+1).eq.1009)idfr=1
73    continue
c                   if any cutang pp's, set icpp
      icpp=0
      last=nds
      if(ibx.eq.0)goto 74
      last=ibx
      goto 76
74    if(iax.gt.0)last=iax
76    ist=icx+1
      do 78 i=ist,last
      k=4*i-3
      if(kd(k).eq.2000)icpp=icpp+1
78    continue
c                   if any retrct pp's, set ibpp
      ibpp=0
      if(last.ge.nds)goto 82
      ist=last
      last=nds
      if(iax.gt.0)last=iax
      do 80 i=ist,last
      k=4*i-3
      if(kd(k).eq.2000) ibpp=ibpp+1
80    continue
82    iapp=0
      if(nds-last.gt.1)iapp=1
c                   now fix d(1-3)
      d(1)=0.
      kd(1)=idfr
      kd(3)=nds
      kd(5)=idx
      kd(6)=icx
      kd(7)=ibx
      kd(8)=iax
      kd(9)=idpp
      kd(10)=icpp
      kd(11)=ibpp
      kd(12)=iapp
c
c               scan e-tbl (matl shp) for xmax,ymax and rxmin,rymin
      i=10
      xmax=-1000.
      ymax=-1000.
      rxmin=1000.
      rymin=1000.
84    i=i+1
      k=4*i
      if(ke(k-1).ne.2.or.ke(k).ne.5) goto 86
c              line.
      x=e(i+1)
      y=e(i+2)
      i=i+2
      goto 88
86    if(ke(k-1).ne.6.or.ke(k).ne.7) goto 90
c              circle.
      x=e(i+1)
      y=e(i+2)
      i=i+6
c              set min/max as req.
88    if(x.gt.xmax) xmax=x
      if(y.gt.ymax) ymax=y
      if(x.lt.rxmin) rxmin=x
      if(y.lt.rymin) rymin=y
90    if(i+1.lt.ke(3))goto 84
      rxmin = rxmin-cldist
      rymin = rymin-cldist
c              scan f-tbl (part shp) for fxmin,fymin
      nf=kf(3)
      i=10
      fxmin=1000.
      fymin=1000.
92    i=i+1
      k=4*i
      if(kf(k-1).ne.2.or.kf(k).ne.5) goto 94
c              line.
      x=f(i+1)
      y=f(i+2)
      i=i+2
      goto 96
94    if(kf(k-1).ne.6.or.kf(k).ne.7) goto 97
c              circle.
      x=f(i+1)
      y=f(i+2)
      i=i+6
96    if(x.lt.fxmin) fxmin=x
      if(y.lt.fymin) fymin=y
97    if(i+1.lt.nf)goto 92
      if(rxmin.gt.fxmin) fxmin=rxmin
      if(rymin.gt.fymin) fymin=rymin
      ad(7)=fxmin
      ad(8)=fymin
      ad(9)=xmax
      ad(10)=ymax
      goto 99
c
c              lathe/finish   (future)
c
100   continue
c
c              error exit
98    if(ifl(2).le.0) ifl(2)=5
c
99    return
      end
