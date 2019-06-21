C*********************************************************************
C*    NAME         :  mshpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       mshpre.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:33
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mshpre
c*          prepare a mesh surface from patch data
c*        in file supplied by md or northrop and
c*        store in ranfile.
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
      subroutine mshpre
 
      include 'com4a.com'
 
      common/pblok/p(400)
      common/wblok/w(600)
 
      real*8 p,w,p2(16,3),x,y,z
      real*4 aw(1200)
      integer*2 kw(12)
      character*200 ap1
      character*184 ap2
      logical leq1, leq2,leq3
c      equivalence (p,p1),(p,leq2),(leq2,ap1),(p2,p(50))
      equivalence (p,p2),(p,leq2),(leq2,ap1)
      equivalence (p(26),leq3),(leq3,ap2),(aw,kw,w)
      equivalence (ipst,isc10(3)),(ipnd,isc10(4))
      character*80 a
      character*8 a1
      character*6 sc11ch,snam
      logical lsvpg,lfilok
      equivalence (sc(11),leq1),(leq1,sc11ch)
      integer*2 ktv(4)
      equivalence (tv,ktv)
      logical nored
      integer*4  srfkey,m,n
      integer*2  ietype
c
c...Added for RS6000. Paul
c...11/19/91
c
      integer*4 lunsf
c
      lfilok=.false.
      lsvpg=.false.
      nored=.false.
      lunsf=4
      nwds=0
10    continue
c                                       find surface name specified in sc(11)
      read(lunsf,err=989,end=986)a
      a1=a(1:8)
      if (a1.ne.'SURFACE ') goto 10
      snam=a(9:14)
      read(lunsf,err=989)m,n
      lfilok=.true.
      if (sc11ch.eq.' ') goto 20
      if (snam(1:1).ne.' ') goto 16
      j=0
      do 12 i=1,6
        if (snam(i:i).eq.' ') goto 12
        j=j+1
        snam(j:j)=snam(i:i)
12    continue
      if (j.ne.6) snam(j+1:)=' '
 
16    if (snam.eq.sc11ch) goto 20
      do 18 i=1,m*n
        do 18 j=1,16
        read(lunsf,err=989,end=986)p2(j,1),p2(j,2),p2(j,3)
18    continue
      goto 10
c                                       get row & col sizes
20    continue
      lpat=m*n
      lm=m
      ln=n
      ipat=0
      jpat=0
      if (ipst.eq.0) goto 30
c                                 user requested patch selection
      if (ipst.lt.0.or.ipnd.gt.lpat) goto 988
      lm=ipnd-((ipnd-1)/m)*m - ipst+((ipst-1)/m)*m +1
      ln=(ipnd-1)/m-(ipst-1)/m+1
      if (lm.lt.1) goto 988
      lpat=ipnd
      jpat=ipst-1
      if (jpat.eq.0) goto 30
c                                 skip to first patch
      do 25 i=1,jpat
        do 25 j=1,16
        read(lunsf,err=989)p2(j,1),p2(j,2),p2(j,3)
25    continue
c                                  set up header word
30    kw(1)=26
      npats=lm*ln
      kw(2)=npats
      kw(3)=lm
      kw(4)=ln
c
c                      Added for U, and V to be saved in unibase
	  kw(9)=ifl(137)
	  kw(10)=ifl(138)
	  kw(11)=ifl(137)
	  kw(12)=ifl(138)
      w(2)=0.
      iwx=1
c
c   save surface header
c
       call ptmhed(w, srfkey)
c                            write out dummy last patch to allocate mem
       call ptmptt(srfkey, npats, w)
c...
C      write(cout,9005) snam,kw(1),kw(2),kw(3),kw(4),m,n
C9005  format(' ',a6,6i4)
C      call putmsg(cout, 80, 1, 0)
 
35    continue
      do 70 j=1,lm
        jpat=jpat+1
        do 40 i=1,16
40        read(lunsf,err=989) p2(i,1),p2(i,2),p2(i,3)
c                                                refsys
        if (ifl(72).eq.0) goto 55
        do 52 i=1,16
          x=p2(i,1)*sc(56)+p2(i,2)*sc(57)+p2(i,3)*sc(58)+sc(59)
          y=p2(i,1)*sc(60)+p2(i,2)*sc(61)+p2(i,3)*sc(62)+sc(63)
          z=p2(i,1)*sc(64)+p2(i,2)*sc(65)+p2(i,3)*sc(66)+sc(67)
          p2(i,1)=x
          p2(i,2)=y
          p2(i,3)=z
52      continue
 
55      jwx=2*iwx+7
        w(iwx+1)=p2(1,1)
        w(iwx+2)=p2(1,2)
        w(iwx+3)=p2(1,3)
c...
C        write(cout,9010) jpat,w(iwx+1),w(iwx+2),w(iwx+3)
C9010    format (i4,3f20.10)
C        call putmsg(cout, 80, 1, 0)
 
        do 60 i=2,16
          aw(jwx)=p2(i,1)-p2(1,1)
          aw(jwx+1)=p2(i,2)-p2(1,2)
          aw(jwx+2)=p2(i,3)-p2(1,3)
c...
C          write(cout,9020) aw(jwx),aw(jwx+1),aw(jwx+2)
C9020      format (3f20.10)
C          call putmsg(cout, 80, 1, 0)
 
          jwx=jwx+3
60      continue
c
c   save patch data
c
        ipat=ipat+1
        call ptmptt(srfkey, ipat, w(iwx+1))
 
        iwx=iwx+26
        if(iwx .ge. 575) then
           nwds=nwds+iwx
           iwx = 0
        endif
70    continue
 
      if (jpat.eq.lpat) goto 80
      if (lm.eq.m) goto 35
c                             skip patches if nec
      do 75 i=1,m-lm
        do 75 k=1,16
        read(lunsf,err=989)p2(k,1),p2(k,2),p2(k,3)
75    continue
      jpat=jpat+m-lm
      goto 35
 
80    continue
 
82    ktv(3)=nwds+iwx
      ktv(4)=9
      ietype = ktv(4)
      call ptdesc(srfkey, ietype, tv)
      rest = tv
      if (.not. ldspsf) call blkgeo (srfkey, 1)
      goto 999
 
986   if (lfilok) goto 987
c                           file is wrong format
      ifl(2)=267
      goto 990
c                           surf specified not found
987   ifl(2)=287
      goto 990
c                           inconsistent start and end patches
988   ifl(2)=288
      goto 990
c                           i/o error reading file
989   ifl(2)=289
 
990   err=.true.
      ist=0
 
999   close(lunsf)
      return
      end
