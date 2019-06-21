C*********************************************************************
C*    NAME         :  fincut.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       fincut.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:03
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine fincut
C*       description
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
      subroutine fincut
c
      include 'com4a.com'
      include 'mocom.com'

cc      common/lthcom/f(400),a(10),b(10)
      common/lthcom/e(1000),f(1000),a(10),b(10)

      real*8 e,f,a,b,asn,dbuf(3),buf(120),abuf(6),rdat(13)
      real*8 bb
      real*4 ad(300),bsn(2)
      integer*2 ksc(500),kd(600),ksn(4),ka(40),kb(40),kf(4000)
      integer*2 ipu
      character*2 cpu
      integer*4 nclkey
      integer*2 ietype
      logical trflg
      equivalence (sc,ksc),(d,ad,kd),(a,ka),(b,kb),(asn,bsn,ksn),(f,kf)
      equivalence (buf,dbuf), (ipu,cpu)
c
      abuf(3) = 0.
      abuf(4) = sc(4)
      abuf(5) = sc(5)
      abuf(6) = sc(6)
      cpu = 'pu'
      trflg = .true.
      nd=ksc(39)
      nf=kf(3)
      iafed=0
      iefed=0
c                 hold input tool loc for possible return
      xin=sc(1)
      yin=sc(2)
c
c            init rdat for circ interp o/p
      do 21 i=2,5
21    rdat(i)=0.
      rdat(1)=5.
c
      iengx=0
      itrctx=0
      iturnx=0
c              set ptrs to engage,retrct,return in d-list (if any)
      if(nd.lt.6) goto 32
      do 30 i=6,nd
      asn=d(i)
      if(ksn(1).ne.324) goto 22
      iengx=i
      goto 30
22    if(ksn(1).ne.7) goto 24
      itrctx=i
      goto 30
24    if(ksn(1).ne.322) goto 30
      iturnx=i
      goto 32
30    continue
c              set zone-a end per engage,retrct,return, or nd
32    iand=iengx-1
      if(iand.lt.0) iand=itrctx-1
      if(iand.lt.0) iand=iturnx-1
      if(iand.lt.0) iand=nd
c
c              issue any zone-a pp commands
      if(iand.lt.6)goto 40
      do 38 i=6,iand
      asn=d(i)
      if(ksn(1).ne.2000)goto 38
c              if fedrat, turn iafed on.
      if(ksn(2).eq.1009) iafed=1
      call putcl(ksn(1),ksn(2),ksn(3),d(i+1))
38    continue
c       *******    engage   ******
c               if not given, issue f/r rapid and goto stpt
c          (if plotting and 'pu' not in effect, issue 'pd' to plotter)
40    continue
c     if(ifl(100).eq.1.and.ksc(431).ne.ipu) call pltout(2,'pd;',3)
      if(iengx.gt.5) goto 42
      dbuf(1)=f(12)
      dbuf(2)=f(13)
      dbuf(3)=0.
      call ppwrt(5)
      goto 50
c               engage given.  calc stpt per on,in,out  and prepnt.
c                ( for now, assume on.)
42    asn=d(iengx+1)
      alf=bsn(1)/57.29578
      dis=bsn(2)
      dbuf(1)=f(12)-dis*cos(alf)
      dbuf(2)=f(13)-dis*sin(alf)
      dbuf(3)=0.
      endx=dbuf(1)
      endy=dbuf(2)
c                goto prepnt at fr/rapid
      call ppwrt(5)
      abuf(1) = dbuf(1)
      abuf(2) = dbuf(2)
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,1,dbuf)
      else
        call putcl(5000,5,1,abuf)
      endif
cc      if(ifl(100).eq.1)call plotm(abuf,.false.)
c                issue any engage pp's
      iend=nd
      if(iturnx.gt.0) iend=iturnx
      if(itrctx.gt.0) iend=itrctx
      ist=iengx+2
      do 44 i=ist,iend
      asn=d(i)
      if(ksn(1).ne.2000) goto 44
c                if fedrat, turn iefed on
      if(ksn(2).eq.1009) iefed=1
      call putcl(ksn(1),ksn(2),ksn(3),d(i+1))
44    continue
c                now stpt
      dbuf(1)=f(12)
      dbuf(2)=f(13)
      dbuf(3)=0.
      endx=dbuf(1)
      endy=dbuf(2)
c               goto stpt
50    continue
      abuf(1) = dbuf(1)
      abuf(2) = dbuf(2)
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,1,dbuf)
      else
        call putcl(5000,5,1,abuf)
      endif
cc      if(ifl(100).eq.1)call plotm(abuf,.false.)
c               if a-fedrat was voided by e-fedrat, restore a
      if(iefed.eq.0.or.iafed.eq.0) goto 55
      do 54 i=6,iand
      asn=d(i)
      if(ksn(1).ne.2000.or.ksn(2).ne.1009) goto 54
      call putcl(2000,1009,ksn(3),d(i+1))
54    continue
c
c         ********   motion.  follow sh1 in f-tbl.  *******
c                      (pp's are temp disallowed as 1st or last)
c
55    ifx=13
56    ifx=ifx+1
      if(ifx.ge.nf)goto 70
      asn=f(ifx)
      if(ksn(4).ne.5.or.ksn(3).ne.2) goto 60
c           ****  line  ***
c              issue 5000,5,1 record
      dbuf(1)=f(ifx+1)
      dbuf(2)=f(ifx+2)
      dbuf(3)=0.
      endx=dbuf(1)
      endy=dbuf(2)
      abuf(1)=dbuf(1)
      abuf(2)=dbuf(2)
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,1,dbuf)
      else
        call putcl(5000,5,1,abuf)
      endif
c              if plotting, o/p now
cc      if(ifl(100).eq.1) call plotm(abuf,.false.)
      ifx=ifx+2
      goto 56
60    if(ksn(4).ne.7.or.ksn(3).ne.6)goto 67
c           ****  circle  ****
c             calc pts approx intol on this circle for nccs post.
      xc=f(ifx+3)
      yc=f(ifx+4)
      asn=f(ifx+6)
      bst=bsn(1)
      bnd=bsn(2)
      delb=bnd-bst
c             use tol*2.   (these are refpts only)
      toler=sc(27)*2.
      r=f(ifx+5)
      h=r-toler
      if(h.lt.r*.8)h=r*.8
      co=h/r
      db=acos(co)*2.
      anpts=abs(delb)/db
      npts=anpts+1.
c              npts=2 min, 40 max
      if(npts.lt.2)npts=2
      if(npts.gt.40)npts=40
      anpts=npts
      db=delb/anpts
      n=0
      bb=bst
c              calc x,y and add to buf
      do 65 n=1,npts
      i=3*n-2
      bb=bb+db
      buf(i)=xc+r*cos(bb)
      buf(i+1)=yc+r*sin(bb)
      buf(i+2)=0.
c             if plotting, do it now
cc      if(ifl(100).ne.1) goto 65
cc      abuf(1)=buf(i)
cc      abuf(2)=buf(i+1)
cc      call plotm(abuf,.false.)
65    continue
      endx=buf(i)
      endy=buf(i+1)
c             issue 3000-2 clfile rec (nccs format. not westinghouse)
      rdat(6)=f(ifx+3)
      rdat(7)=f(ifx+4)
      rdat(8)=0.
      rdat(9)=0.
      rdat(10)=0.
      rdat(11)=1.
      rdat(12)=r
      call putcl(3000,2,13,rdat)
c             issue 5000-5 rec
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,npts,buf)
      else
        it = 5
        do 66 n=1,npts
          i=3*n-2
          abuf(1)=buf(i)
          abuf(2)=buf(i+1)
          call putcl(5000,it,1,abuf)
          it = 6
66      continue
      endif
      goto 56
c
67    if(ksn(4).ne.2000)goto 56
c           ********  issue this pp command in shape list  *****
      num=ksn(3)+1
      call putcl(2000,ksn(1),num,f(ifx+1))
      goto 56
c
c      ***************** retrct ********************
70    if(itrctx.eq.0)goto 80
c             issue any pp-commands first
      iend=nd
      if(iturnx.gt.0)iend=iturnx-1
      num=iend-itrctx
      if(num.lt.2)goto 75
c              something is in retrct region. issue any pp's found
      ibeg=itrctx+2
      do 72 i=ibeg,iend
      asn=d(i)
      if(ksn(1).ne.2000)goto 72
      call putcl(2000,ksn(2),ksn(3),d(i+1))
72    continue
c              goto retrct pt
75    asn=d(itrctx+1)
      alf=bsn(1)/57.29578
      dis=bsn(2)
      dbuf(1)=endx+dis*cos(alf)
      dbuf(2)=endy+dis*sin(alf)
      dbuf(3)=0.
      endx=dbuf(1)
      endy=dbuf(2)
      abuf(1)=dbuf(1)
      abuf(2)=dbuf(2)
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,1,dbuf)
      else
        call putcl(5000,5,1,abuf)
      endif

cc      if(ifl(100).eq.1) call plotm(abuf,.false.)
c
c      ****************  return ********************
80    if(iturnx.eq.0)goto 95
c                 chk for pp commands.  issue if found
      if(nd.le.iturnx+1) goto 85
      ibeg=iturnx+2
      do 82 i=ibeg,nd
      asn=d(i)
      if(ksn(1).ne.2000)goto 82
      call putcl(2000,ksn(2),ksn(3),d(i+1))
82    continue
c                 do return motion
85    asn=d(iturnx+1)
c                  if return 'off'  exit now
      if(ksn(4).eq.72)goto 95
c                  if return (by itself) goto input loc
      if(ksn(4).ne.0) goto 86
      dbuf(1)=xin
      dbuf(2)=yin
      goto 90
c                  if return 'xaxis' , hold y-val
86    if(ksn(4).ne.84)goto 87
      dbuf(1)=xin
      dbuf(2)=endy
      goto 90
c                  if return 'yaxis', hold x-val
87    if(ksn(4).ne.85)goto 88
      dbuf(1)=endx
      dbuf(2)=yin
      goto 90
c                  must be 'point'.  get into dbuf
88    if(ksn(4).ne.3.or.ksn(3).ne.3) goto 95
      call gtentt(asn, trflg, nclkey, ietype, dbuf)
cuni      call getent(dbuf,3,ksn(1),ksn(2),3)
c                  issue motion
90    dbuf(3)=0.
      endx=dbuf(1)
      endy=dbuf(2)
      abuf(1) = dbuf(1)
      abuf(2) = dbuf(2)
      if (ifl(82) .eq. 0) then
        call putcl(5000,5,1,dbuf)
      else
        call putcl(5000,5,1,abuf)
      endif
cc      if(ifl(100).eq.1)call plotm(abuf,.false.)
c                  update sc( ) with last location
95    sc(1)=endx
      sc(2)=endy
99    return
      end
