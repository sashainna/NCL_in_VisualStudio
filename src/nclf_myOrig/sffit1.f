C*********************************************************************
C*    NAME         :  sffit1.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sffit1.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sffit1
C*       description
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
      subroutine sffit1

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxpn = 20)
      parameter (maxcv = 50)
      parameter (maxev = 12)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/fitcom/scc(maxcv),r(maxcv*maxev*3),knv(maxcv),
     1  ncvs,lastk,nextk,nexk,itcrv
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q
      common/wblok/w(600)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxcv*maxev*3),
     2 aw(1200),r
      real*8 x(maxptx),y(maxptx),z(maxptx),p(400),v(450),asn,w,
     1       scc,hj(36)
      integer*2 inv(maxptx),ksn(4),kscc(80)
      equivalence (w,aw),(asn,ksn),(v,w(151)),(x,p),(jb,hj),(scc,kscc)
      integer*2 ient(4)
      equivalence (sc(53),ient)
      real*8 tbuf(3)
      integer*4 nclkey
      integer*2 ietype
      logical trflg

c                   240 pts (max) for srf fitting
c
c                pts 1-150 sto in v-tbl
c                next 90 sto in q-tbl
c                vecs sto in r-tbl
c
c   sf/fit uses cvs only - set sc(53) for crvpnt
      do 5 i=1,maxpt
5     inv(i)=0
      ient(1)=8
      asn=sc(10)
      ncvs=ksn(3)
      if(ncvs.le.maxcv.and.ncvs.gt.2)goto 12
      ifl(2)=93
      goto 99
c            sto sc list in scc-tbl
12    nmov=ncvs
      if(nmov.gt.12)nmov=12
      do 15 i=1,nmov
15    scc(i)=sc(10+i)
c            if any more cvs, get next ranfil record and finup scc-list
      if(ncvs.lt.13)goto 20
      ix=12
      nleft=ncvs-12
      irec=ifl(4)+1
16    call getran(hj,irec)
      nmov=nleft
      if (nmov.gt.35) nmov=35
      do 17 i=1,nmov
17    scc(i+ix)=hj(i)
      ix=ix+35
      irec=irec+1
      nleft=nleft-nmov
      if (nleft.gt.0) goto 16

c            get cvs, calc 12 pts each, sto 150 max in v-tbl, then in q
20    continue
      iqx=0

      do 30 i=1,ncvs
      asn=scc(i)
c                 note:  entity must be curve
      if(ksn(4).eq.8) goto 21
      ifl(2)=21
      goto 99
21    continue
cuni21    call getent(w,ksn(3),ksn(1),ksn(2),ksn(4))
      trflg = .true.
      call gtentt(asn, trflg, nclkey, ietype, w(1))
      call ncl_get_tpar (nclkey,tbuf)
      if (tbuf(1) .ne. 0. .or. tbuf(2) .ne. 1.d0)
     -       call rdf_curve (nclkey,tbuf,1)
      w(maxwd+20)=0.
      aw(maxwd*2+40)=aw(1)
      aw(1)=0.
      s=0.
      ds=maxev-1
      ds=1./ds
      do 25 j=1,maxev
      if(s.gt..99)s=1.
      call crvpnt(s,0,0,0)
c              pts sto in q
      q(iqx+1) = w(maxwd+12)
      q(iqx+2) = w(maxwd+13)
      q(iqx+3) = w(maxwd+14)
      iqx=iqx+3
25    s=s+ds
30    continue

c              slpset 12 times transverse direction
c               sto vecs in r-tbl
      do 40 i=1,maxev
c                   get pts into x,y,z
      iqx=3*(i-1)
      do 34 j=1,ncvs
      x(j)=q(iqx+1)
      y(j)=q(iqx+2)
      z(j)=q(iqx+3)
      iqx=iqx+maxev*3
34    continue
      call setslp(inv,ncvs,0,iret)
      if(iret.le.0)goto 35
      ifl(2)=iret+46
      goto 99
c                 sto vecs
35    irx=3*i-3
      do 37 j=1,ncvs
      r(irx+1)=a(j)
      r(irx+2)=b(j)
      r(irx+3)=c(j)
37    irx=irx+maxev*3
40    continue
c            toss-out unnec cvs
c              knv-tbl holds record of kept cvs
      do 50 i=1,ncvs
50    knv(i)=1
      lastk=1
52    nextk=ncvs
c              sch 12 tcvs for next kept cv
      do 54 j=1,maxev
      itcrv=j
      call echk
c              if error in echk, exit
      if(ifl(2).gt.0) goto 99
c              echk has retd next cv nec this tcrv
      if(nexk.lt.nextk)nextk=nexk
c              if nextk=lastk+1, stop searching
      if(nextk.le.lastk+1)goto 58
54    continue
c
c               zero-out any knv's between lastk and nextk
      jst=lastk+1
      jnd=nextk-1
      do 56 l=jst,jnd
56    knv(l)=0
c               update kount of last cv kept
58    lastk=nextk
c               if ncvs not reached, go again
      if(lastk+1.lt.ncvs)goto 52
c
c               knv-tbl now holds list of kept cvs.
99    return
      end
