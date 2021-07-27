
C*********************************************************************
C*    NAME         :  srffit.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       srffit.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:44
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine srffit
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
      subroutine srffit

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxpn = 20)
      parameter (maxcv = 50)
      parameter (maxev = 12)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))

      common/fitcom/scc(maxcv),r(maxcv*maxev*3),
     1  knv(maxcv),ncvs,lastk,nextk,nexk,itcrv
      real*8 scc
      real*4 r
      integer*2 ncvs,lastk,nextk,nexk,itcrv

      common/wblok/w(4*(maxwd+20))
      real*8 w

      common/hjblok/hj(2*maxpt)
      real*8 hj

      common/twblok/ktwist(2*maxpt)
      integer*2 ktwist

      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,inv
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxcv*maxev*3)
      integer*2 inv(maxptx)

      real*8 tbuf(3)
      real*8 asn
      real*4 aw(8*(maxwd+20)),ahj(4*maxpt),s,ds
      integer*4 nclkey
      integer*2 ient(4),ksn(4),kscc(80),ksc(500)
      integer*2 ietype,i,ild,j,jrec,nout
      logical trflg

      equivalence (sc(53),ient)
      equivalence (w,aw),(asn,ksn),(scc,kscc)
      equivalence (jb,ahj),(sc,ksc)

c
c..... The default value of ifl(346) is zero, it means reverse boundary
c..... curves to avoid twisting. The value ifl(346)=1 means do not reverse.
c

c
c..... set all inv(i)=0 for slpfar
c
      do 5 i=1,maxpt
5     inv(i)=0
c
c..... sf/fit uses cvs only - set sc(53) for crvpnt
c
      ient(1)=8

      asn=sc(10)
      ncvs=ksn(3)
      if(ncvs.le.maxcv .and. ncvs.gt.2) goto 12
      ifl(2)=93
      goto 99
c
c..... store cvs into the scc array
c
12    do 15 i=1,ncvs
15    scc(i)=hj(i)
c
c..... get cvs, calc 12 pts each, store in q-tbl
c
      iqx=0

      do 30 i=1,ncvs
      asn=scc(i)
c
c..... note:  entity must be curve
c
      if(ksn(4).eq.8) goto 21
      ifl(2)=21
      goto 99

21    trflg = .true.
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
         if(s.gt..99) s=1.
         if (ktwist(i).eq.0 .or. ifl(346).eq.1) then 
            call crvpnt(s,0,0,0)
         else
            call crvpnt(1.0-s,0,0,0)
         endif
c
c..... store pts in q
c
         q(iqx+1) = w(maxwd+12)
         q(iqx+2) = w(maxwd+13)
         q(iqx+3) = w(maxwd+14)
         iqx=iqx+3
         s=s+ds
25    continue
30    continue

c
c..... To determine slopes cut all curves in transverse direction
c..... at 12 points, then call slpfar. Store vecs in r-tbl
c
      do 40 i=1,maxev
c
c..... Get pts into x,y,z arrays; reversing some if necessary
c
      iqx=3*(i-1)

      do 34 j=1,ncvs
      x(j)=q(iqx+1)
      y(j)=q(iqx+2)
      z(j)=q(iqx+3)
      iqx=iqx+maxev*3
34    continue

      call slpfar(ncvs,0,iret)
      if(iret.eq.0) goto 35
      ifl(2)=iret+46
      goto 99
c
c..... store vecs
c
35    irx=3*i-3
      do 37 j=1,ncvs
      r(irx+1)=a(j)
      r(irx+2)=b(j)
      r(irx+3)=c(j)
37    irx=irx+maxev*3
40    continue
c
c..... toss out unnecessary cvs. knv-tbl holds record of kept cvs
c
      do 50 i=1,ncvs
50    knv(i)=1

      lastk=1
52    nextk=ncvs
c
c..... search for next kept cv
c
      do 54 j=1,maxev
      itcrv=j
      call echk
      if(ifl(2).gt.0) goto 99
      if(nexk.lt.nextk) nextk=nexk
c
c..... if nextk=lastk+1, stop searching
c
      if(nextk.le.lastk+1) goto 58
54    continue
c
c..... zero-out any knv's between lastk and nextk
c
      jst=lastk+1
      jnd=nextk-1
      do 56 l=jst,jnd
56    knv(l)=0
c
c..... update count of the kept curves
c
58    lastk=nextk
c
c..... if ncvs not reached, go again
c
      if(lastk+1.lt.ncvs) goto 52
c
c..... update ktwist array             
c
      if (ifl(346).eq.1) goto 100
      do 60 i=ncvs,1,-1
      ktwist(2*i-1)=ktwist(i)*knv(i)
      ktwist(2*i)=0
60    continue
c
c***** end of historic sffit1 subroutine
c***** next comes historic sffit2
c

c
c..... build a new scc-tbl of kept cvs
c
100   jrec=ifl(4)
      j=0
      do 130 i=1,ncvs
      if(knv(i).eq.0) goto 130
      j=j+1
      if(j.gt.maxpn*2) goto 132
      hj(j)=scc(i)
c
c..... get vecs into ahj-tbl and write into ranfil record
c
      jrec=jrec+1
      irx=3*maxev*(i-1)

      do 116 k=1,maxev*3
116   ahj(k)=r(k+irx)

      call putran(jb,jrec)
c
c..... also, record the corresponding (even) hj-entry
c
      asn=0.
      ksn(1)=jrec
      ksn(4)=23
      j=j+1
      if(j.gt.maxpn*2) goto 132
      hj(j)=asn
130   continue

      nout=j
      if(nout.le.maxpn*2) goto 140
c
c..... max output cvs = 20
c
132   ifl(2)=264
      goto 99
c
c..... update ranfil ptrs
c
140   ifl(4)=jrec+1
      ifl(7)=jrec+1
      ifl(8)=0
c
c..... update sc-tbl (12 max), and cut the hj-leftovers
c
      do 141 i=nout+1,2*maxpt
141   hj(i)=0.0      

      ild=min0(nout,12)
      do 142 k=1,ild
142   sc(10+k)=hj(k)
c
c..... fix sc(10) to show the final number of cvs
c
      asn=sc(10)
      ksn(3)=nout
      sc(10)=asn

99    return
      end
