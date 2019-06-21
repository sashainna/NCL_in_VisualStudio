C*********************************************************************
C*    NAME         :  uvprmv.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       uvprmv.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:52
C*********************************************************************
C
c******************************************************************
c          is called by cvintx to prepare for basic move.         *
c            this file is a partial copy of the premov.for file.  *
c                                                                 *
c          1.  calc init fwd from prior fwd-up senses plus        *
c              --fwd,lft,rgt,back-- in go***/ command.            *
c                                                                 *
c          2.  load ds and cs in d-tbl (ps already there)         *
c                                                                 *
c          3.  set ps 'up' and ds 'rgt'                           *
c                                                                 *
c          4.  set calc depth levels for ps,ds,cs  ifl(56,57,58)  *
c                                                                 *
c          5.  prepare tcol(3) for mover entry.                   *
c                                                                 *
c******************************************************************
 
      subroutine uvprmv
 
      include 'com8a.com'
      include 'mocom.com'
      include 'drvcom.com'
 
c          motion common equivalences
      real*4 srf(10,3)
      equivalence (srf,s)
 
      real*4 ac(2),ad(300),asc(320),sbe,cbe,cutn
      equivalence (sc,asc),(asc(63),sbe),(asc(64),cbe),(asc(65),cutn)
      integer*2 ksn(4),kc(4),jd(600)
      integer*2 ia,ib,ic,isrf,kds
      equivalence(c,ac,kc),(d,ad,jd),(ifl(54),isrf)
      integer*4 ka(2),kb(2),nclkey
      equivalence(asn,ksn),(b,kb),(asn,ka)
c     real*4 fx,fy,fz,fl,u,v
      real*4 u,v
c     integer*4 locn
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
      equivalence(ifl(51),ia),(ifl(52),ib),(ifl(53),ic)

c          if psis quilt srf , set itfl=-3    3-oct-84
      if(jd(1).eq.25) ifl(49)=-3
c          point to sc word, index to d-tbl
30    ixx=1
32    ix=50*ixx
      kdx=ix*4
      isx=ixx*mxsfhd
      iscx=ixx*2+9
      asn=sc(iscx)
      sc(144+ixx)=asn
      nw=ksn(3)
      if(ksn(4).eq.6.or.ksn(4).eq.5)goto 40
      if(ksn(4).eq.7.or.ksn(4).eq.8)goto 44
      if(ksn(4).eq.9)goto 34
c          illegal entity type for ds, cs
      ifl(2)=118
      err=.true.
      goto 99
c
34    continue
c              assume full surf
      jd(kdx+1)=9
      goto 50

c40    if(ksn(1).ne.0) call getent(d(ix+3),nw,ksn(1),ksn(2),0)
40    call gtgeom(asn,d(ix+3),nclkey,nwds,itype)
c          add this asn to d(2) for ident purposes
      d(ix+2)=asn
c           add ityp to d(1)
      kc(1)=ksn(4)
      kc(2)=0
      d(ix+1)=c
c          also load cs pl in srf(_,3)
      if(iscx.eq.11.or.ksn(4).eq.5)goto 50
      do 42 m=1,4
      n=ix+2+m
42    srf(m,3)=d(n)
      goto 50
c
c... if not circle, must be curve
c
44    if(ksn(4).eq.CURVE) goto 48
c
c...circle. get it now.
c
      call gtgeom(asn,d(ix+3),nclkey,nwds,ietype)
      kc(1)=7
      kc(2)=0
c              go use cv finup logic.
      goto 49
c              curve.  set ityp,nmp and iseg=0 but do not get
c                       item now. segs will activate later.
48    kc(1)=ksn(4)
c
c... get nsegs for NCL curve.
c
      call gtdesc(asn,nclkey,nwds,ietype)
      call isitwf (nclkey, iwf)
      if (iwf.eq.0) call gtncsg(nclkey,kc(2))

49    kc(3)=0
      d(ix+1)=c
      d(ix+2)=asn

50    continue
c
c**************************  set direc's for ps and ds  (if surfs)
c          ps first
51    if(jd(1).ne.6)goto 60
c********  psis pl     ( direct it up )
      if(t(4,3)*d(3)+t(5,3)*d(4)+t(6,3)*d(5).gt.0.)goto 54
      do 52 i=3,6
52    d(i)=-d(i)
c          add this pl to srf(isrf)
54    do 55 i=1,4
55    srf(i,1)=d(i+2)
      goto 70
c********  ps is a surf
c          ( psis/ ln,cv,ci is now a syntax error. chg in future )
60    ad(2)=1.
      isrf=1
      ifl(51)=3
      t(4,3) = 0.0
      t(5,3) = 0.0
      t(6,3) = 0.0
c          use ps 1st-look pt
      srf(8,1)=t(1,3)
      srf(9,1)=t(2,3)
      srf(10,1)=t(3,3)
      u=u1
      v=v1
      call sfinit (sc(144), isrf, u, v)
      call surfpn (u,v,1)
      t(13,3)=u
      t(14,3)=v
      sfa=srf(1,1)
      sfb=srf(2,1)
      sfc=srf(3,1)
      t(4,3)=sfa
      t(5,3)=sfb
      t(6,3)=sfc

c          if psnorm not up, reverse direc(1) and ps tanpl.
c      if(t(4,3)*sfa+t(5,3)*sfb+t(6,3)*sfc.gt.0.)goto 67
c      ad(2)=-ad(2)
c      do 62 i=1,4
c62    srf(i,1)=-srf(i,1)
c67    continue
c********  ditto ds.   ( no action if ln,cv )
70    kds = jd(201)
      if (kds.eq.SURF .or. kds.eq.PLANE) goto 71

      if(kds.ne.CURVE .and. kds.ne.CIRCLE .and. kds.ne.LINE) then
        ifl(2) = 118
        goto 99
      endif

      ia = 3
      ic = 3
      isrf = 2
      t(21,3) = 0.
      call dsrel0
      if (ifl(2) .eq. 466) goto 99
      goto 80
c              do a rgt sense
71    rx=t(8,3)*t(6,3)-t(9,3)*t(5,3)
      ry=t(9,3)*t(4,3)-t(7,3)*t(6,3)
      rz=t(7,3)*t(5,3)-t(8,3)*t(4,3)
      if (jd(201) .ne. 6) goto 75
c          dsis pl
c      if(rx*d(53)+ry*d(54)+rz*d(55).gt.0.)goto 73
c          flip ds pl
c      do 72 i=53,56
c72    d(i)=-d(i)
c          also in srf tbl
73    do 74 i=1,4
74    srf(i,2)=d(i+52)
      goto 80
c          dsis sf
75    ad(102)=1.
      isrf=2
      ifl(51)=3
c          use ds 1st-look pt
      srf(8,2)=t(1,3)
      srf(9,2)=t(2,3)
      srf(10,2)=t(3,3)
      u=u2
      v=v2
      call sfinit (sc(145), isrf, u, v)
      call surfpn(u,v,1)
      t(19,3)=u
      t(20,3)=v
c      sfa=srf(1,2)
c      sfb=srf(2,2)
c      sfc=srf(3,2)
c      if(rx*sfa+ry*sfb+rz*sfc.gt.0.)goto 80
c          flip direc(2) and vec in srf( ,2))
c      ad(102)=-ad(102)
c      do 76 i=1,4
c76    srf(i,2)=-srf(i,2)
80    continue
c             set fwd
      t(7,3)=srf(2,1)*srf(3,2)-srf(3,1)*srf(2,2)
      t(8,3)=srf(3,1)*srf(1,2)-srf(1,1)*srf(3,2)
      t(9,3)=srf(1,1)*srf(2,2)-srf(2,1)*srf(1,2)
c          init zero dp
      t(10,3)=0.
      t(10,2)=0.
c          set ps,ds,cs calc depths    ifl(56,57,58)
c              ps. if ta/normal,ps or ball-end cutter, pslev=0
      ifl(56)=0
      t(15,3)=0.
c              ds. if tlon or cutter/0, dslev=0
82    ifl(57)=0
c           ifl(27)=0,1  for ds convex/concave to cutter.
c             convex assumed at pass start.
      ifl(27)=0
      t(21,3)=0.
c              set nearflg off           9-15-81
86    ifl(83)=0
99    if (ifl(2) .gt. 0) err = .true.
      return
      end
