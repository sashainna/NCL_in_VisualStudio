C*********************************************************************
C*    NAME         :  cvofst.f
C*       CONTAINS:  offcv
C*    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
c**
c**    MODULE NAME AND RELEASE LEVEL
c*       offcv.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:10:23
C*********************************************************************
c
c **********************************************************************
c **********************************************************************
c **  subroutine name:  offcv  
c **                                                                  **
c **  purpose of subroutine: create a curve offset a given distance
c **                         from an existing curve per command :
c **
c **                           cv/offset,cv1,xyzls,dist
c **
c **                                    or
c **
c **                           sp/offset,cv1,xyzls,dist
c **
c **    itsk = 1 for spline, 2 for ssplin
c **   
c **                           ssplin/offset,cv1,xyzls,dist             
c **    
c **********************************************************************
c **********************************************************************

      subroutine offcv(itsk)

      include 'com8a.com'
      include 'wrksys.com'

      common/pblok/p(400)
      common/wblok/w(600)

      integer*2 ksn(4),ksc(100),ktv(4),krest(4)
      real*8 p,w,asn,asw
      equivalence (asn,ksn),(tv,ktv),(sc,ksc),(rest,krest)
      integer*4 nclkey,ncevof,scomp,ecomp
      integer*2 nwds,ietype,mod,offall
      real*8 dis,sc10,dtol
      real*8 dm(3)
      integer*2 isf,maxpts,npts,itsk,errflg,labfl,typefl
      integer*2 ifl4x, ifl7, ifl8, ksn2, i
      integer*2 i2v4 /4/
      parameter (maxpts=600)
      real*8 pts(maxpts*3), vs(maxpts*3)
      real*8 invmx(12)
      logical*2 offcmp

      sc10=sc(10)
      asn = sc(10)
      ksn2 = ksn(2)
c
c..... get cv1 header into w-tbl
c
      asn=sc(11)

      call gtdesc(asn,nclkey,nwds,ietype)
      call isitwf (nclkey, iwf)
c
c..... do direction vector indicated by direction modifier
c
      dm(1)=0.
      dm(2)=0.
      dm(3)=0.
      mod=ksc(48)
c
c.....Added option to use offset vector - ASF 11/19/13.
c
      if (mod.eq.604) then
        dm(1) = sc(16)
        dm(2) = sc(17)
        dm(3) = sc(18)
      else
        if (mod.eq.638) dm(1)=1.
        if (mod.eq.639) dm(2)=1.
        if (mod.eq.640) dm(3)=1.
        if (mod.eq.641) dm(1)=-1.
        if (mod.eq.642) dm(2)=-1.
        if (mod.eq.643) dm(3)=-1.
        if (ifl(72).eq.1) call conent(dm,sc(56),i2v4)
      endif
      if (lwrk) call conent(dm,wrkmx,i2v4)
c
c..... dis is the offset distance
c
      dis = sc(13)
      if (ifl(264).eq.1) dis = dis/25.4d0
      ifl4x = ifl(4)
      ifl7 = ifl(7)
      ifl8 = ifl(8)
c
c.....Get component number range for offset if given
c
      scomp = sc(14)
      ecomp = sc(15)
      offcmp = .false.
      offall = 0
      errflg = 0
      if (ifl(395).gt.0) offcmp = .true.
      if (ifl(395).eq.2) offall = 1
      typefl = ifl(396)
      ifl(396) = 0
      if (offcmp.and.scomp.le.0.and.ecomp.le.0.and.offall.ne.1) then
        err = .true.
        ifl(2) = 445
        goto 999
      endif
      if (offcmp .eq. .true.) goto 60

      isf = 3
      call evstup (nclkey, isf)
  
      dtol = sc(27)
      if (ifl(264).eq.1) dtol = dtol/25.4d0
 
      if (ksn2 .eq. 17) goto 50
c
c.... call C-routine that evolves curve and offsets evolved pts
c
      npts = ncevof(isf, dtol, maxpts, dm, dis, pts, vs)

      if (npts.eq.0) then
        ifl(2) = 163
      else if (npts.eq.-1) then
        ifl(2) = 476
      else if (npts.gt.maxpts) then
c
c..... test allocation error: if greater than allocated space
c
        ifl(2) = 156
      endif

      if (ifl(2) .gt. 0) goto 990

      call crvgn1 (npts, pts, vs, asw)

      if (ifl(2).gt.0) goto 990
c
c.... transform if refsys or modsys in effect
c
      sc(10)=sc10
      ietype = CURVE
      if (lwrk) then
        do i = 1,12
          invmx(i) = invwrk(i)
        enddo
        if (ifl(264).eq.1) then
          invmx(4) = invwrk(4)*25.4
          invmx(8) = invwrk(8)*25.4
          invmx(12) = invwrk(12)*25.4
        endif
        call transf(w,invmx,nwds,ietype)
      endif
      if (ifl(72).eq.1) call transf (w, sc(68), nwds, ietype)

      iwx = (npts+1)/2
      nw = iwx + 6*npts
      ktv(3) = nw

      ktv(4)=8
      ietype = ktv(4)
      call ptentt(ietype, w, nclkey, tv)
      goto 900

50    continue
      defwf = .true.

      call ncofcv (itsk,nclkey,dm,dis,dtol,ifl(2))

      if (nclkey.eq.0) goto 990

      call ptdesc (nclkey,ietype,tv)

      goto 900
c
c.....Call C routine to offset components
c
60    dtol = sc(27)
      ifl(395) = 0
      if (ifl(264).eq.1) dtol = dtol/25.4d0
      labfl = 1
      call umf_offset_compcrv_comps(nclkey,dm,offall,scomp,ecomp,dis,
     x                        errflg,savid2,isvsub,labfl,dtol,typefl)
      if (errflg.ne.0) then
        err = .true.
        ifl(2) = errflg
        goto 999
      endif
      call ptdesc (nclkey,ietype,tv)
      
900   continue
      rest = tv

      call crvcls (nclkey)

      ifl(4) = ifl4x
      ifl(7) = ifl7
      ifl(8) = ifl8

      goto 999

c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2)=5
c
c..... equivalence (lfl(29),err) in com.com
c
      err=.true.

999   return
      end

