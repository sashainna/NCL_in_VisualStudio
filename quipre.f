C*********************************************************************
C*    NAME         :  quipre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       quipre.for , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:35
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine quipre
c*        prepares a quilt srf of rockwell patches in w-tbl.    18-sep-84
c*
c*        quipre now processes the sequential file supplied by rockwell
c*        directly.  declsf finds the file and opens it.  quipre verifies
c*        the format and builds the surf tables.    epm   1-12-85.
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
      subroutine quipre
 
      include 'com4a.com'
      common/pblok/p(400)
      common/wblok/w(600)
 
      real*8 p,w,asn,xnew,ynew,znew,xo,yo,zo,amap(12)
      real*8 x(25),y(25),z(25)
      real*4 xm(12),ym(12),zm(12),aw(1200),bsn(2)
      real*4 xe(48),ye(48),ze(48)
c
c...Changed for RS6000. Paul
c...11/19/91
c
c     integer*2 ival(67),ksn(4),kmap(48),ki(4),kj(4),roclun
c
      integer*2 ival(67),ksn(4),kmap(48),ki(4),kj(4)
      integer*4 roclun
c
      integer*2 ietype,ktv(4)
      equivalence (tv,ktv)
      character*200 ax,ay,az
      character*80 null
      logical leq1,leq2,leq3
 
      integer*4 npatch, nclkey
      character*5 title
 
c               use pblok for misc array storage
      equivalence (p,x),(p(26),y),(p(51),z),
     1 (x,leq1),(leq1,ax),(y,leq2),(leq2,ay),(z,leq3),(leq3,az),
     2 (asn,bsn,ksn),(w,aw),
     3 (p(76),amap,kmap),(p(100),xm),(p(106),ym),(p(112),zm),
     4 (p(118),xe),(p(142),ye),(p(166),ze),(p(190),ival),
     5 (p(210),ki),(p(211),kj)
 
c               ki,kj show pts used to calc epts
      ki(1)=8
      ki(2)=18
      ki(3)=12
      ki(4)=14
 
      kj(1)=3
      kj(2)=23
      kj(3)=11
      kj(4)=15
 
c               basic sf info is in sc(10),,,
 
      asn=sc(10)
      npats=ksn(3)
      if(npats.lt.13) goto 10
c             error.  numpats = 12 max
      ifl(2)=115
      goto 99
10    continue
 
 
      roclun=4
 
c        the patches to be extracted from the file are in sc table.
c
 
      jpat=0
c            read the patch record to ge the patch number
20    continue
      read (roclun,end=51) title, npatch
      if (debug) then
           write (cout,10101)npatch
10101      format (' patch number =',i6)
           call putmsg (cout,80,17,0)
      endif
 
      do 30,i=1,npats
c               check patch number against the ones in sc table
          if (sc(10+i).eq.npatch) then
c                  found this patch in sc table
              jpat=jpat+1
c                  zero that sc entry.  when all are zeroed, stop looking
              sc(10+i)=0
c                  skip the next record
              read (roclun)null
              read(roclun)ax
              read(roclun)ay
              read(roclun)az
 
c              do 25,j=1,25
c                  read (roclun,102)x(j),y(j),z(j)
c102               format (12x,3f20.12)
c                  if (debug) then
c                      write (cout,10102)x(j),y(j),z(j)
c10102                 format (' vals=',3(xf10.5))
c                      call putmsg (cout,80,18,0)
c                  endif
c25            continue
 
              go to 31
          endif
30    continue
c          if we get here, the patch number in the file was
c          not one of the patches specified in sc table.
c          skip it and try the next patch in the file
      read (roclun)null
      read(roclun)ax
      read(roclun)ay
      read(roclun)az
      go to 20
 
c             if refsys on, run xyz's thru refmat
31    if(ifl(72).eq.0) goto 35
      do 32 i=1,25
      xnew=x(i)*sc(56)+y(i)*sc(57)+z(i)*sc(58)+sc(59)
      ynew=x(i)*sc(60)+y(i)*sc(61)+z(i)*sc(62)+sc(63)
      znew=x(i)*sc(64)+y(i)*sc(65)+z(i)*sc(66)+sc(67)
      x(i)=xnew
      y(i)=ynew
32    z(i)=znew
 
c             add midpt to xyzmid tbl
35    xm(jpat)=x(13)
      ym(jpat)=y(13)
      zm(jpat)=z(13)
c             calc an ept each side, add to xe,ye,ze tbls.
      do 40 isid=1,4
      i=ki(isid)
      j=kj(isid)
      dx=x(j)-x(i)
      dy=y(j)-y(i)
      dz=z(j)-z(i)
      tl=sqrt(dx**2+dy**2+dz**2)
      ro=0.
      if(tl.gt.0.) ro=.01/tl
      iex=4*(jpat-1)+isid
      xe(iex)=x(j)+ro*dx
      ye(iex)=y(j)+ro*dy
      ze(iex)=z(j)+ro*dz
 
cc--              if this side length=0, spread pts small amt  17-apr-85
      k=kj(isid)
      inc=1
      if(isid.gt.2)inc=5
      i=k-2*inc
      j=k-inc
      l=k+inc
      m=l+inc
      tlsq=(x(m)-x(i))**2+(y(m)-y(i))**2+(z(m)-z(i))**2
      if(tlsq.gt.1.e-6)goto 40
c              side length is lt .001      spread it out.
      ii=ki(isid)-2
      if(isid.gt.2)ii=ii-8
      jj=ii+4
      if(isid.gt.2)jj=ii+20
      dx=x(jj)-x(ii)
      dy=y(jj)-y(ii)
      dz=z(jj)-z(ii)
      sec=sqrt(dx**2+dy**2+dz**2)
 
c              if this dist also very small , do not attempt spread
      if(sec.lt..01)goto 40
      delx=dx/sec*.001
      dely=dy/sec*.001
      delz=dz/sec*.001
      x(i)=x(k)-delx*2.
      y(i)=y(k)-dely*2.
      z(i)=z(k)-delz*2.
      x(j)=x(k)-delx
      y(j)=y(k)-dely
      z(j)=z(k)-delz
      x(l)=x(k)+delx
      y(l)=y(k)+dely
      z(l)=z(k)+delz
      x(m)=x(k)+delx*2.
      y(m)=y(k)+dely*2.
      z(m)=z(k)+delz*2.
 
cc--   end   17-apr-85
 
40    continue
c             chg rockwell data to bezier polygon form for convenience.
c              u-direction first
      do 47 i=1,5
      j=i+5
      k=i+10
      l=i+15
      m=i+20
      x(k)=(16.*x(k)-x(i)-4.*(x(j)+x(l))-x(m))/6.
      y(k)=(16.*y(k)-y(i)-4.*(y(j)+y(l))-y(m))/6.
47    z(k)=(16.*z(k)-z(i)-4.*(z(j)+z(l))-z(m))/6.
 
c              now v-direction
      do 49 i=1,21,5
      j=i+1
      k=i+2
      l=i+3
      m=i+4
      x(k)=(16.*x(k)-x(i)-4.*(x(j)+x(l))-x(m))/6.
      y(k)=(16.*y(k)-y(i)-4.*(y(j)+y(l))-y(m))/6.
49    z(k)=(16.*z(k)-z(i)-4.*(z(j)+z(l))-z(m))/6.
 
 
c               round-off pt1 and convert all to deltas from same.
      ix=x(1)
      iy=y(1)
      iz=z(1)
      xo=ix
      yo=iy
      zo=iz
 
      iwx=40*jpat-19
      jwx=2*iwx
      aw(jwx+1)=xo
      aw(jwx+2)=yo
      aw(jwx+3)=zo
      jwx=jwx+3
 
      do 50 i=1,25
      j=i+25
      k=i+50
      x(i)=x(i)-xo
      y(i)=y(i)-yo
      z(i)=z(i)-zo
      aw(jwx+i)=x(i)
      aw(jwx+j)=y(i)
50    aw(jwx+k)=z(i)
c
c                continue reading patches until npats satisfied.
      if(jpat.lt.npats) goto 20
      go to 53
c           this is the end of file branch.  check to see if all
c           the patches in the sc table were found.
51    do 52,i=1,npats
          if (sc(10+i).ne.0) then
              ifl(2)=269
              npatch=sc(10+i)
              write(errcom,1051) npatch
1051          format('=',i10)
              go to 99
          endif
52    continue
 
c        *************************
c                 build bdy map this set of patches
c
c                    check 4 sides each patch vs. all other patches for
c                    containment.  when found, enter other patnum in this
c                    map spot.  if that other patch side does not already
c                    point to a patch, point back to this one.
c
c                   zero the map tbl
53    do 55 i=1,12
55    amap(i)=0.
c                   if only one patch, no map reqd.
      if(npats.eq.1) goto 75
 
      jpat=0
60    jpat=jpat+1
      isid=0
62    isid=isid+1
      kmx=(jpat-1)*4+isid
      if(kmap(kmx).ne.0)goto 70
      do 65 i=1,npats
      if(i.eq.jpat)goto 65
c                   check ept(kmx) for containment in patch(i)
      kpat=i
      call contck(kmx,kpat,ksid)
 
      if(ksid.eq.0) goto 65
c                   a patch was found.  enter in map
      kmap(kmx)=i
c                   point that other patch to this one, if indicated.
      lmx=(i-1)*4+ksid
      if(kmap(lmx).eq.0) kmap(lmx)=jpat
65    continue
 
70    if(isid.lt.4) goto 62
      if(jpat.lt.npats) goto 60
 
c        ******* finup work ***********
 
c             w(1)
75    ksn(1)=20+40*npats
      ksn(2)=npats
      bsn(2)=1.
      w(1)=asn
c             w(2)=0.    ( later used to sto this srf address in d-tbl.)
      w(2)=0.
 
c                 add midpts to aw-tbl  and amap wd to each patch
      jwx=4
      do 80 i=1,npats
      aw(jwx+1)=xm(i)
      aw(jwx+2)=ym(i)
      aw(jwx+3)=zm(i)
      iwx=40*i-19
      w(iwx)=amap(i)
80    jwx=jwx+3
 
      call ptqhed(w,nclkey)
      iwx=21
      do 84 ipat=1,npats
      call ptqpat(nclkey, ipat, w(iwx))
      iwx=iwx+40
84    continue
 
      ietype=9
      call ptdesc(nclkey,ietype,tv)
      ktv(3)=20+40*npats
      if (.not. ldspsf) call blkgeo (nclkey, 1)
      rest=tv
 
99    close (unit=roclun)
      return
      end
