C*********************************************************************
C*    NAME         :  sffit2.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       sffit2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine sffit2
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
      subroutine sffit2

      include 'com4a.com'

      parameter (maxpt = 50)
      parameter (maxptx = 1000)
      parameter (maxpn = 20)
      parameter (maxcv = 50)
      parameter (maxev = 12)

      common/fitcom/scc(maxcv),r(maxcv*maxev*3),knv(maxcv),
     1  ncvs,lastk,nextk,nexk,itcrv
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q
      common/wblok/w(600)
      real*4 a(maxptx),b(maxptx),c(maxptx),dx(maxptx),dy(maxptx)
      real*4 dz(maxptx),ch(maxptx),q(maxcv*maxev*3),
     2 aw(1200),r,ahj(72)
      real*8 x(maxptx),y(maxptx),z(maxptx),p(400),v(450),asn,w,
     1       scc,hj(36)
      integer*2 ksn(4),kscc(80),ksc(500)
      equivalence (w,aw),(asn,ksn),(v,w(151)),(x,p),(scc,kscc)
      equivalence (jb,ahj,hj),(sc,ksc)
      real*8 sss(maxpn*2)

c           build a new scc-tbl of kept cvs
      jrec=ifl(4)
      j=0
      do 30 i=1,ncvs
      if(knv(i).eq.0)goto 29
      j=j+1
      if(j.gt.maxpn*2) goto 32
      sss(j)=scc(i)
c           get vecs into ahj-tbl and wrt in ranfil excess record
      jrec=jrec+1
      irx=3*maxev*(i-1)
      do 16 k=1,maxev*3
16    ahj(k)=r(k+irx)
c            wrt this set of vecs
      krec=jrec
      call putran(jb,krec)
c             also sss entry
      asn=0.
      ksn(1)=jrec
      ksn(4)=23
      j=j+1
      if(j.gt.maxpn*2) goto 32
      sss(j)=asn
29    continue
30    continue
c              max output cvs = 10
      if(j.le.maxpn*2) goto 40
c              error.  too many o/p cvs
32    ifl(2)=264
      goto 99
c              update ranfil ptrs
40    ifl(4)=jrec+1
      ifl(7)=jrec+1
      ifl(8)=0
c              updat sc-tbl (12 max)
      nout=j
      if(j.gt.12)nout=12
      do 42 k=1,nout
42    sc(10+k)=sss(k)
c              if any more sss's, load in rec(ifl(4)+1)
      if (j.lt.13) goto 50
      ix=12
      num=j-ix
      jrec=ifl(4)+1
43    nout=num
      if (nout.gt.35)nout=35
      do 44 i=1,nout
44    hj(i)=sss(ix+i)
      call putran(jb,jrec)
      ix=ix+35
      jrec=jrec+1
      num=num-nout
      if (num.gt.0)goto 43
c              fix sc(10) to show final num cvs
50    asn=sc(10)
      ksn(3)=j
      sc(10)=asn
99    return
      end
