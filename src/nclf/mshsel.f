C*********************************************************************
C*    NAME         :  mshsel.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       mshsel.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:19
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine mshsel
c*          prepare a mesh surface by selecting patches from 
c*        given mesh surface
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
      subroutine mshsel

      include 'com4a.com'

      common/wblok/w(600)

      real*8 w
      integer*2 kw(12)
      equivalence (kw,w)
      equivalence (ipst,isc10(3)),(ipnd,isc10(4))
      integer*2 ktv(4)
      equivalence (tv,ktv)
      integer*4  srfkey, nclkey, m,n
      integer*2  nwds, ietype

      call gtgeom(sc(11), w, nclkey, nwds, ietype)
      m = kw(3)
      n = kw(4)
c                                       get row & col sizes
      lpat=m*n
      lm=m
      ln=n
      ipat=0
      jpat=0
      iwx=10
      if (ipst.eq.0) goto 30
c                                 user requested patch selection
      if (ipst.lt.0.or.ipnd.gt.lpat) goto 9288
      lm=ipnd-((ipnd-1)/m)*m - ipst+((ipst-1)/m)*m +1
      ln=(ipnd-1)/m-(ipst-1)/m+1
      if (lm.lt.1) goto 9288
      lpat=ipnd
      jpat=ipst-1
c                                  set up header word
30    kw(1)=26
      npats=lm*ln
      kw(2)=npats
      kw(3)=lm
      kw(4)=ln
C
C...Call ptmhdt() to convert offdist back to inches if we are using metric
C...ptmhdt() calls ptmhed() - RAZ
C
C     call ptmhed(w, srfkey)
c                            save surface header
      call ptmhdt(w, srfkey)
c                            write out dummy last patch to allocate mem
      call ptmpat(srfkey, npats, w(iwx))

      do 70 k=1,ln
      do 50 j=1,lm
        jpat=jpat+1
        call gtmpat (nclkey, jpat, w(iwx))
        ipat=ipat+1
        call ptmpat (srfkey, ipat, w(iwx))
50    continue
      jpat=jpat+m-lm
70    continue

      ktv(3)=npats
      call ptdesc(srfkey, ietype, tv)
      rest = tv
      if (.not. ldspsf) call blkgeo (srfkey, 1)
      goto 999
c                           inconsistent start and end patches
9288  ifl(2)=288
C     goto 990

990   err=.true.
      ist=0

999   return
      end
