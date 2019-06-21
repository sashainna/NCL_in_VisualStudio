C*********************************************************************
C*    NAME         :  geogn2.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       geogn2.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:05
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine geogn2
C*       prepare and store curve/circle and surf
C*                          canon lists.
C*
C*       the first 13 pieces of information about the geometry are
C*       contained in sc(10-22).  any additional information is in the
C*       next open ranfil page pointed to by ifl(4)+1.
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
      subroutine geogn2

      include 'com8a.com'

      integer*2 maxpt, maxptx, maxpn, maxwd, mxsfhd
      parameter (maxpt = 50)
      parameter (maxwd = (maxpt*6+(maxpt+1)/2))
      parameter (maxptx = 1000)
      parameter (maxpn = 20)
      parameter (mxsfhd = (2+(maxpn+1)/2))

      integer*4 keyold
      common/keycom/keyold
      real*8 x(maxptx),y(maxptx),z(maxptx)
      real*4 a(maxptx),b(maxptx),c(maxptx)
      real*4 dx(maxptx),dy(maxptx),dz(maxptx)
      real*4 ch(maxptx),q(maxpt*36)
      integer*2 invx(maxptx)
      common/pblok/x,y,z,a,b,c,dx,dy,dz,ch,q,invx
      real*8 w
      common/wblok/w(4*(maxwd+20))

      common/cviofl/cvion,iadvcs
      logical cvion
      integer*2 iadvcs

      real*8 hj
      common/hjblok/hj(2*maxpt)
      integer*2 ktwist
      common/twblok/ktwist(2*maxpt)

      integer*2 prtyp
      real*8 prdat(16)
      common/primbl/prdat,prtyp,icoor

      integer*2 ietype, ipg, isub, ixv, ipv, ivf, srfnum, ierr, nen
      integer*2 i2v1 /1/
      integer*2 srfhed(mxsfhd*4),isc(420),ipatwd(2)
      integer*2 inv(maxptx),kentj(4),ksn(4),ktv(4),iap(8)
      integer*4 ibnum,ienum,idir,key,nclkey,numpck,nup,nupt,nvp,nvpt
      integer*2 ifl72, itrans
C
C...Changes ncl_cvonsf to ncl_cvonsf_func to distinguish from
C...the array in neboundary.c NCL_cvonsf JLS 8/2/99
C
      integer*4 ikey(2), ncl_cvonsf_func
      real*8 asrfhd(3),p(400),t(6)
      real*8 asn, enti, entj, patwd, sctemp, tol
      real*4 aw(8*(maxwd+20))
      logical nored, trflg

      equivalence (aw,w), (x,p,iap)
      equivalence (asn,ksn),(tv,ktv),(asrfhd,srfhed)
      equivalence (sc,isc), (sctemp, ikey), (isub,ksn(4))
      equivalence (kentj,entj), (ipatwd,patwd)

      prtyp = 0
      icoor = 0
      nored=.false.
      icirc=0
      asn=sc(10)
      ksn2=ksn(2)
c                    if shape, go shapre route              17-aug-83
c      if (ksn(1).ne.600) goto 6
c      call shapre
c
c..... Janet 9/14/98
c
c***********************************************************
      if (ksn(1).eq.600) then
         call shapre
      else if (ksn(1).eq.615) then
         call shapre2
      else
         goto 6
      endif

      if (ifl(2) .lt.1) then
         goto 99
      else
         goto 981
      endif
c
c********** circle or curve ********************************
c
6     if (ksn(1).ne.604) goto 8
c..... assignments for circle
      icirc=1
      if (ksn(2).gt.2) goto 21
c
c..... if ci redef, in which case ksn(3)=9876, get xyz from sc(11-19)
c
      if (ksn(3).ne.9876) goto 7
      x(1)=sc(11)
      y(1)=sc(12)
      z(1)=sc(13)
      x(2)=sc(14)
      y(2)=sc(15)
      z(2)=sc(16)
      x(3)=sc(17)
      y(3)=sc(18)
      z(3)=sc(19)
      ksn(3)=3
      isc(39)=3
c                     also zero 3 inv's                    11-feb-87
      do 65 i = 1, 3
        inv(i)=0
        invx(i)=0
65    continue
      ivf=0
      nmp=3
      nen=3
      goto 205
7     ksn3 = ksn(3)
      if (ksn3 .eq. 0) ksn(3) = 3
      goto 9

8     if (ksn(1).ne.606) goto 40
c..... curve. existent subtypes are: 1-6,8-10,12-17,20       01-10-2000
c.....                      1  normal pt def
c.....                      2  curve fit
c
      if (ksn2.eq.3 .or. (ksn2.ge.15.and.ksn2.le.20)) then
        if (ksn2.eq.3 .or. ksn2.eq.16) then
c
c..... CV/ or SPLINE/cv1,<xval>,<yval>,<zval>
c
          call cvofst
        else if (ksn2.eq.15 .or. ksn2.eq.17) then
c
c..... CV/ or SPLINE/OFFSET,cv1,YLARGE,0.25
c
          call offcv(1)
        else if (ksn2.eq.18 .or. ksn2.eq.19) then
c
c..... CV/ or SPLINE/PROJCT,cv1,...
c
          call projcv(1)
        else if (ksn2.eq.20) then
          goto 99
        endif
        if (ifl(2).gt.0) goto 981
        goto 99
      endif

      if (ksn2.eq.4) then
c
c..... CV/EDGE
c
        call cvsfsd
        ranpts = .true.
        if (ifl(2).lt.1) goto 82
        goto 981
      endif
      if (ksn2.eq.5 .or. ksn2.eq.14) then
c
c..... CV or SPLINE / INTOF, SF1, SF2
c
        ranpts = .true.
        iadvcs = 0
        if (sc(169) .lt. 9.149d0) then
          call cvintx(1)
        else
          iadvcs = 1
          call cvio(1)
        endif
c
c...No intersection found
c
        if (isc10(3) .le. 1) ifl(2) = 163

        if (ifl(2).lt.1) then
          if (ksn2.eq.5) goto 82
c
c..... B-SPLINE case
c
          nclkey = keyold
          defwf = .true.
          if (lwrk) then
            itrans = 1
          else if (ifl(264) .eq. 1) then
            itrans = 2
          else
            itrans = 0
          endif
          if (iadvcs .ne. 2) then
            iadvcs = 0
            if (ifl(212) .le. 1) iadvcs = 1
          endif
          call ncl_bsp_intof(isc10(3), nclkey, itrans, iadvcs, sc(27))
          iadvcs = 0
          ifl(212) = 0

          if (nclkey .eq. 0) then
            ifl(2) = 51
            goto 981
          endif

          ietype = 8
          call ptdesc (nclkey, ietype, tv)
          rest = tv
          if (.not. dsplcv) call blkgeo (nclkey, i2v1)
          goto 36
        else
          if (ifl(2).ne.410.and.ifl(2).ne.385) ifl(2)=163
          goto 981
        endif
      endif
c
c..... cv/conic
c
      if (ksn2.eq.6) goto 9
c                                          COMPOSITE CURVE OR B-SPLINE
      if (ksn(2) .eq. 8 .or. ksn(2) .eq. 9 .or. ksn(2) .eq. 10 .or.
     -    ksn(2) .eq. 20) then

        nclkey = keyold
        if (ksn2.eq.8) then
c
c.....  (name =) CURVE/COMPOS,ln|ci|cv,...,ln|ci|cv
c
          call cmpdef (nclkey)
          if (nclkey .eq. 0) then
            ifl(2) = 409
            goto 981
          endif
        else
          j=0
          if (ksn2.eq.10) j=1
          if (ksn2.eq.20) j=2
          defwf = .true.
          call bspdef(nclkey, j)
          if (nclkey .eq. 0) then
            ifl(2) = 51
            goto 981
          endif
        endif
        ietype = 8
        call ptdesc (nclkey, ietype, tv)
        rest = tv
        if (.not. dsplcv) call blkgeo (nclkey, i2v1)
        goto 36
      endif

      if (ksn2 .eq. 12) then
c
c...jingrong 9/23/98
c...surface CURVE
c
        call gtdesc (sc(11),nclkey,nwds,ietype)
        numpck = sc(12)
        ibnum = sc(13)
        ienum = sc(14)
        idir = sc(15)
        defwf = .true.
	  if(numpck .ne. -1) then
          ierr = ncl_cvonsf_func (nclkey,numpck,key,ietype,ibnum,ienum,
     x                                                             idir)
	  else
c
c...curve on revsf
c
	    ierr = ncl_cvon_revsf (nclkey,key,ietype)
	  endif
        if (ierr.eq.0) then
          call ptdesc(key, ietype, tv)
          rest = tv
          goto 99
        else
          ifl(2) = 163
          if (ierr .eq. 472) ifl(2) = 472
          goto 981
        endif
      endif

      if (ksn2 .eq. 13) then
c
c..... SPLINE/SF,EDGE. ksn(3) is the type of edge (see cvsfsd for details),
c..... sc(12) is the offset value.
c
        nen = 1
        sctemp = sc(11)
        call gtdesc (sctemp,nclkey,nwds,ietype)
        call sftype (nclkey, isf)
c
c...quilt and net surfaces are not acceptable
c
        if (isf.eq.25.or.isf.eq.27) then
          ifl(2)=321
          goto 981
        endif

        nclkey = keyold
        defwf = .true.
        call gettol (tol)
        call ncl_cvsfsd(ikey(1),nclkey,ksn(3),ksn(4),isc10(13),sc(12),
     x                                                            tol)

        if (nclkey .eq. 0) then
          ifl(2) = 51
          goto 981
        endif

        ietype = 8
        call ptdesc (nclkey, ietype, tv)
        rest = tv
        if (.not. dsplcv) call blkgeo (nclkey, i2v1)
        goto 36
      endif

      if (ksn2.eq.21) then
c
c..... CV/ or SPLINE/PART[CONTUR],offset,[AT,]zlev,sf1,... - makes a spline anyway
c
        call cvcntr
        if (ifl(2).gt.0) goto 981
        goto 99
      else if (ksn2.eq.22) then
c
c..... CV/OUT,compos
c
        call cvouts
        if (ifl(2).gt.0) goto 981
        goto 99
      else if (ksn2.eq.23 .or. ksn2.eq.24 .or. ksn2.eq.25) then
c
c..... CV/ or SPLINE/INTOF,COMPOS,[AT,]zlev,sf1,... - makes a composite curve
c
        call sfsio
        if (ifl(2).gt.0) goto 981
        goto 99
      endif

      if (ksn2 .gt. 2) goto 98
c
c..... error, only subtypes 1,2 are left
c
      if (ksn2 .lt. 2 .or. ksn(3) .lt. 3) goto 9

82    call crvfit (inv,nmp)
c        turn off flag indicating points for curve fitting are in ranfil
c        this is only used for curve as edge of surface and curve as
c        intersection of a surf and another surf or a plane
      ranpts = .false.
      ifl(212) = 0
c              if no error, go normal crvpre route
      if (ifl(2).gt.0) then
        goto 981
      else
        goto 215
      endif

c*****************************************************  curve/circle
9     nen = ksn(3)
      ipv = 0
      if(ksn(2).eq.6) nen = ksn(4)
c
c..... get entities from sc-table/ranfil and build table
c
10    j=0
      ivf=0
       call ncl_get_asn (0,1,hj(1),nen)
12    do 20 i=1,nen
          asn=hj(i)
          ixv = 0
            trflg = .true.
            call gtentt(asn, trflg, nclkey, ietype, t(1))
c
c..... if point, bump j and put in pt-array. if vector, abc
c
          if (isub .eq. 4) goto 14
          if (isub .eq. 21 .and. ksn(3) .eq. 3) goto 13
          j=j+1
          x(j)=t(1)
          y(j)=t(2)
          z(j)=t(3)
c
c..... unset this vector flag. (will be set if next is vector)
c
          inv(j)=0
          invx(j)=0
          if (isub .eq. 3) goto 20
c
c..... PV as a point
c
          if (isub .eq. 21 .and. ksn(3) .eq. 0) goto 20

13        ixv = 3
c
c..... vector furnished. flag this in inv(i) and set ivf on
c
14        a(j) = t(ixv+1)
          b(j) = t(ixv+2)
          c(j) = t(ixv+3)
          inv(j)=1
          invx(j)=1
          ivf=1
20    continue

      if (icirc.eq.1 .and. nen .lt. 3) nen = 3
      ksn(2)=ksn2
      nmp=j
      if(ksn(2).ne.6) goto 205
      call conpre(inv,nmp)
      if(ifl(2).gt.1) goto 98
      nmp=5

205   iret=0
      if (nen.eq.2.and.inv(1).eq.0.and.inv(2).eq.0) then
        a(1)=x(2)-x(1)
        b(1)=y(2)-y(1)
        c(1)=z(2)-z(1)
        inv(1)=1
        invx(1)=1
      endif
c
c..... Eduard: I changed the next call to slpfar, making use of the common
c..... block array invx.  2/19/99.
c     call slpset (inv,nmp,ivf,iret)
c
      ifl72 = ifl(72)
      ifl(72) = 0
      call slpfar(nmp,ivf,iret)
      ifl(72) = ifl72
      if (iret.lt.1) goto 21
c
c..... error return from slpfar
c
      ifl(2)=iret+46
c
c..... void 15 slp passes error           2-may-85  pem
c
      if(ifl(2).eq.50) ifl(2)=51
      goto 981

c
c..... if crv, go call crvpre.  otherwise circle.
c
21    if (icirc.eq.1) then
         call cirpre (inv,nmp,icirc)
         if (ifl(2).gt.0) goto 981
         nw=11
         ietype = 7
         goto 32
      endif

c
c..... crvpre completes the curve def
c
215   call crvpre(inv,nmp,icirc)
c
c..... build canon form in w-tbl, call putent
c
c..... add s-values first
c
      do 22 i=1,nmp
22        aw(i)=dx(i)
c
c..... add segs one by one starting at iwx = (nmp+1)/2
c
      iwx=(nmp+1)/2
      do 30 iseg=1,nmp
          i=6*(iseg-1)+iwx
          w(i+1)=x(iseg)
          w(i+2)=y(iseg)
          w(i+3)=z(iseg)
          j=2*i+6
          aw(j+1)=a(iseg)
          aw(j+2)=b(iseg)
          aw(j+3)=c(iseg)
          aw(j+4)=dy(iseg)
          aw(j+5)=dz(iseg)
          aw(j+6)=ch(iseg)
30    continue

c
c..... call putent to store the canonical form in the ranfil
c
      nw=iwx+6*nmp
      ietype = 8

32    ktv(3) = nw
      call ptentt(ietype, w, nclkey, tv)
      rest = tv

36    if (ietype.eq.8) call crvcls (nclkey)
      goto 99

c
c********************************************************  surface
c
40    if (ksn(1).ne.607) goto 500

c
c...SF/OUT,compos
c
      if (ksn(2).eq.15) then
        call sfouts
        if (ifl(2).gt.0) goto 981
        goto 99
      endif

      if (ksn(2).eq.14) then
        rest = sc(11)
        goto 99
      endif
      
      if (ksn(2).eq.13) then
        call revpre
        if (ifl(2).gt.0) goto 981
        goto 99
      endif

      if (ksn(2).eq.12) then
        call entcpy
		  if (ifl(2).gt.0) goto 981
        goto 99
      endif

      if (ksn(2).eq.11) then
        call basesf
        goto 99
      endif

      if (ksn(2).eq.10) then
        call trimsf1
        goto 99
      endif

      if (ksn(2).eq.9) then
        call mshsel
        goto 99
      endif
c
c..... fillet surface
c
      if (ksn(2).eq.4.or.ksn(2).eq.8) then
c
c..... Add the U and V values to the surface header
c
        srfhed(1)=91
        call gtsfdp (nup,nupt,nvp,nvpt)
        srfhed(9) = nup
        srfhed(10) = nvp
        srfhed(11) = nupt
        srfhed(12) = nvpt
        srfnum=9
c
c..... get UNIBASE header record
c
        asrfhd(2) = 0.0
        call ptshed(asrfhd, nclkey)
        call srfpre
        if (ifl(2).gt.0) go to 97
        jwds=iap(1)
        iap(1)=iap(3)
        numpan = 1
        call ptspnt(nclkey, numpan, p)
        ietype = srfnum
        ktv(3) = jwds
        call ptdesc(nclkey, ietype, tv)
        rest = tv
        call srfcls (nclkey)
        goto 99
      endif
c
c..... offset surface
c
      if (ksn(2).eq.7) then
        call offpre
        goto 99
      endif
c
c..... mcdonnel-douglas/northrop mesh surface
c
      if (ksn(2).eq.6) then
        call mshpre
        goto 99
      endif
c
c..... q-srf  (rockwell patches)       18-sep-84
c..... quipre prepares storage form in w-tbl
c
      if (ksn(2).eq.5) then
        call quipre
        if(ifl(2).gt.0) goto 981
        goto 99
      endif
c
c..... net surface
c
      if (ksn(2).eq.3) then
        call smpre
        goto 99
      endif
c
c..... ksn(2)=1 here. It corresponds to surf/fit if ksn(4)=1,
c..... surf/cv1,cv2,... if ksn(4)=0
c
c
c..... The default value of ifl(346) is zero, it means "Reverse boundary
c..... and slope curves to avoid twisting". The value ifl(346)=1 means
c..... "Do not reverse curves".
c
c
c..... Add the U and V values to the surface header
c
      srfhed(1)=91
      call gtsfdp (nup,nupt,nvp,nvpt)
      srfhed(9) = nup
      srfhed(10) = nvp
      srfhed(11) = nupt
      srfhed(12) = nvpt
      srfnum=9
      ksn3=ksn(3)

      if (ksn3.lt.2 .and. ksn(2).eq.1) then
        ifl(2)=130
        goto 981
      endif

      ksn4=ksn(4)
      if (ksn4.eq.0) then
c
c..... surf/cv1,cv2,... case - must verify ksn(3) is even
c
         if ((ksn3/2)*2.ne.ksn3) then
           ifl(2)=277
           goto 981
         endif
         numpan=(ksn3/2)-1
      else
c
c...surf/fit,cv1,cv2,... case
C...For FIT there must be at least 3 curves, call an error if less than
C...that.   JLS 3/1/99
c
         if(ksn3.le.2) then
            ifl(2) = 21
            goto 981
         endif
         numpan=ksn3-1
      endif
c
c..... The initial, up to 12, surface-defining entities are stored in
c..... sc(11-22). If there are more, they are stored into the hj array.
c..... For convenience, all the entities are put in hj(1,2,...,ksn3)
c
      ild=min0(ksn3,12)
      do 86 i=1,ild
         hj(i)=sc(i+10)
86    continue

      if (ksn3.gt.2) goto 87
c
c..... ruled surface
c
      numpan=1
      goto 100

87    if (ksn3.ge.13) then
         ipg = ifl(4) + 1
         call getran(hj(13),ipg)
         if (ksn3 .ge. 48) then
             ipg = ipg + 1
             call getran(hj(48),ipg)
         endif
      endif
c
c..... Check successive pairs of curves (non-zero ones), and
c..... set flag ktwist(j)=1 on those which cause twisting.
c
      if (ifl(346).eq.1) goto 90
      enti=hj(1)
      i=1
      j=1
88    itwist=0
      j=j+1
      entj=hj(j)
c
c..... check if the entity has a direction, i.e., whether it is reversible.
c..... if not, j=j+1 again
c
      ityp=kentj(4)
      if (ityp.eq.0.or.ityp.eq.3.or.ityp.eq.4.or.ityp.eq.21) then
         if (j .lt. ksn3) then
            j=j+1
            entj=hj(j)
         else
            goto 90
         endif
      endif
      call chkent(enti, entj, itwist)
      if (ifl(2).gt.0) goto 981
      if (ktwist(i).eq.0) then
          ktwist(j)=itwist
      else
          ktwist(j)=1-itwist
      endif
      if (j .ge. ksn3) goto 90
      i=j
      enti=entj
      goto 88

C
C...If this is SURF/FIT we need to call srffit, which will change the values in
C...sc, so update asn,hj,and numpan. JLS 3/1/99
C
90    if(ksn(4).eq.1) then
         call srffit
         if(ifl(2).gt.0) goto 981
         asn=sc(10)
c         do 91 i=1,ksn(3)
c            hj(i)=sc(i+10)
c91       continue
         numpan=(ksn(3)-2)/2
         goto 100
      endif
c
c..... If any zeros, call crvbld to handle same.
c
      ivf=0
c     do 92 i=1,ksn3
      do 92 j=2,ksn3,2
         i=j/2
         patwd=hj(j)
         if (ipatwd(1).eq.0.and.ipatwd(2).eq.0) then
c
c..... zero curve + refsys is ng.      7-16-82
c
            invx(i)=0
         else
            ivf=ivf+1
            invx(i)=1
         endif
92    continue
c
c..... crvbld is called if there are zeros in hj(even)
c
      if (ivf.le.numpan) call crvbld(ivf)
      if (ifl(2).gt.0) goto 981
c
c..... get UNIBASE header record
c
100   asrfhd(2) = 0.0
      call ptshed(asrfhd, nclkey)

      do 400 i=1,numpan
          sc(11)=hj(2*i-1)
          sc(12)=hj(2*i)
          sc(13)=hj(2*i+1)
          sc(14)=hj(2*i+2)
          ktwist(1)=ktwist(2*i-1)
          ktwist(2)=ktwist(2*i)
          ktwist(3)=ktwist(2*i+1)
          ktwist(4)=ktwist(2*i+2)
          call srfpre
          if (ifl(2).gt.0) go to 97
          jwds=iap(1)
          iap(1)=iap(3)
          call ptspnt(nclkey, i, p)
400       continue
      ietype = srfnum
      ktv(3) = jwds
      call ptdesc(nclkey, ietype, tv)
      rest = tv
      call srfcls (nclkey)
      goto 99
c
c..... Error in srfpre.  Delete surface header
c
97    call dlgeom (nclkey)
      goto 98
c
c***********************************************************
c..... B-Spline surfaces
c
500   if (ksn(1).ne.614) goto 600
      if (ksn(2).eq.1) then
        call sdepre
      else if (ksn(2).eq.2) then
        call revpre
      else if (ksn(2).eq.3) then
        call bsfpre
      else
        goto 98
      endif
      if (ifl(2).gt.0) goto 98
      if (.not. ldspsf) then
        call gtdesc (rest, nclkey, i, ietype)
        call blkgeo (nclkey, i2v1)
      endif
      goto 99
c
c***********************************************************
c..... uv curve on surface
c
600   if (ksn(1).ne.619) goto 98
      call geogss
      goto 99

c
c..... error exit
c
98    if (ifl(2).lt.1) ifl(2) = 5
981   err=.true.
c
c..... turn off flag indicating points for curve fitting are in ranfil
c..... this is only used for curve as edge of surface and curve as
c..... intersection of a surf and another surf or a plane
c
      ranpts = .false.
c
c..... perform  primitive analysis automatically for newly created surfaces.
c....  jingrong 01/11/2000.
c
99    continue
      if (.not.err) then
         call gtdesc (rest, nclkey, i, ietype)
         if (ietype.eq.9) then
            if (prtyp .le. 0) then
              call ncl_get_sf_primtyp(nclkey,prtyp)
              if (prtyp.eq.0)
     1          call ncl_sf_prim_analyz(nclkey,prtyp,prdat)
            else if (icoor .eq. 1) then
              call ptprim(nclkey,prtyp,prdat)
            else
              call ncl_put_sf_primdat(nclkey,prtyp,prdat)
            endif
         endif
      endif
c
c...free memory used for points
c
      call ncl_free_uv
      return
      end
