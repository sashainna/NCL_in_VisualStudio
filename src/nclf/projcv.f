C*********************************************************************
C*    NAME         :  projcv.f
C*       CONTAINS:
C*    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       projcv.f , 25.1
C*    DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:28
C*********************************************************************
c
c **********************************************************************
c **********************************************************************
c **  subroutine name:  projcv (itsk)
c **                                                                  **
c **  purpose of subroutine: project a curve/spline/ln/ci,compcv  onto a
c **                         plane/surface (maybe with wrapping)
c **
c **    itsk = 1 for spline, 2 for ssplin
c **
c **      SPLINE/PROJCT,cv(,ve),sf(WRAP or REVOLV)(,ATTACHPT)(,NEARPT)
c **    or
c **      SPLINE/PROJCT,cv(ve or sang(,eang)),sf(,NEARPT)
c **    Same format for SSPLIN
c **
c **    Note: the result is always a SPLINE (SSPLIN)
c **
c **********************************************************************
c **********************************************************************

      subroutine projcv(itsk)

      include 'com8a.com'
      include 'const.com'


      integer*2 ksn(4),ktv(4),krest(4)
      real*4 bsn(2)
      real*8 asn,angs(2),uv(2),uva(2)
      equivalence (asn,bsn,ksn),(tv,ktv),(rest,krest)
      integer*4 nclkey,cvkey,sfkey,sfka,vckey,atkey,npkey
      integer*2 nwds,ietype,isub
      equivalence (isub,ksn(4))

      real*8 tol
      integer*2 itsk,ksn2,iatach,iwrap,iproj,inrpt,atangl
      parameter (atangl = 1)
      integer*2 i2v1 /1/


      asn = sc(10)
      ksn2 = ksn(2)
      iwrap = ksn(3)
      iatach = ksn(4)

      asn = sc(19)
      uv(1) = bsn(1)
      uv(2) = bsn(2) 

      asn=sc(11)
      call gtdesc(asn,cvkey,nwds,ietype)
c
c..... do direction vector indicated by direction modifier
c
      iproj = 0
      asn = sc(12)
      vckey = 0
      if (ksn(1) .eq. atangl) then
        if (iwrap .ge. 1) goto 990
        iproj = 2
        asn = sc(20)
        angs(1) = bsn(1)/RADIAN
        angs(2) = bsn(2)/RADIAN
        sfka = 0
        uva(1) = 0
        uva(2) = 0
        if (sc(16) .ne. 0) then
          call gtdesc(sc(16),nclkey,nwds,ietype)
          if (ietype.eq.plane .or. ietype.eq.surf) then
            sfka = nclkey
            asn = sc(17)
            uva(1) = bsn(1)
            uva(2) = bsn(2) 
          endif
        endif
      else if (isub .gt. 0) then
        iproj = 1
        call gtdesc(asn,vckey,nwds,ietype)
      endif

      asn = sc(13)
      call gtdesc (asn,sfkey,nwds,ietype)
      if (ietype.ne.plane .and. ietype.ne.surf) goto 990

      inrpt = 0
      asn = sc(14)
      npkey = 0
      if (isub .gt. 0) call gtdesc(asn,npkey,nwds,ietype)

      atkey = 0
      if (iatach .eq. 5) call gtdesc(sc(15),atkey,nwds,ietype)

      tol = sc(27)
      if (ifl(264).eq.1) tol = tol/25.4d0
c
c.... call C-routine that evolves curves and projects or wraps them onto
c.... surfaces
c
      defwf = .true.

      call nclf_cv_project_sf(itsk,cvkey,sfkey,uv,tol,iwrap,iatach,
     1    iproj,angs,vckey,atkey,sfka,uv1,npkey,nclkey,ifl(2))

      if (nclkey.le.0 .or. ifl(2).gt.0) goto 990

      if (itsk .eq. 2) defwf = .false.
      call ptdesc (nclkey,8,tv)
      rest = tv
      if (.not. dsplcv) call blkgeo (nclkey, i2v1)
      
      if (itsk .eq. 1) call crvcls (nclkey)

      goto 999
c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2) = 5
c
c..... equivalence (lfl(29),err) in com.com
c
      err=.true.

999   return
      end

