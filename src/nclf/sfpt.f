C*********************************************************************
C*    NAME         :  sfpt.f
C*       CONTAINS:
C*
C*      SUBROUTINE SFPT (ASW, PTE, ISF, DIR, U, V, SS)
C*      SUBROUTINE SFPT1 (ASW, PTE, ISF, DIR, U, V, SS)
C*      subroutine sfpt2(nclkey, pe, u8, v8, unflg, dir8, psf, vsf, ierr)
C*      SUBROUTINE SFPTVC (ASW,PTE,prvec,ISF,ucur,vcur,ierr)
C*
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       sfpt.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       06/01/15 , 07:45:22
C*********************************************************************
C
C **********************************************************************
C **  SUBROUTINE NAME: SFPT
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: PROJECT POINT TO SURFACE
C **  INPUT -
C **    ASW      - ASSOCIATED WORD OF SURFACE
C **    PTE      - POINT TO PROJECT
C **    ISF      - INDEX TO SURFACE TABLE
C **    DIR      - SIDE OF SURFACE
C **    U        - STARTING U VALUE
C **    V        - STARTING V VALUE
C **  OUTPUT -
C **    U        - ENDING U VALUE
C **    V        - ENDING V VALUE
C **    SS       - SURFACE TABLE
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE SFPT (ASW, PTE, ISF, DIR, U, V, SS)

      INCLUDE 'com4a.com'
      include 'mocom.com'

      REAL*8 ASW, PTE(3)
      REAL*4 U, V, DIR, SS(9)
      INTEGER*2 ISF
C                      MOTION COMMON EQUIVALENCES
      real*4 srf(10,4)
      equivalence (srf,s)

      INTEGER*2 ISRF
      EQUIVALENCE (IFL(54), ISRF)
      REAL*8 PL1(4),ASN
      INTEGER*2 KSN(4)
      EQUIVALENCE (ASN,KSN)
      REAL*4 D1
      INTEGER*2 I
      integer*4 nclkey
      integer*2 nwds, ietype

      ASN = ASW
      IF (KSN(4).EQ.9) GOTO 40
C                                            --- PLANE
c      IF (DIR.NE.0.) GOTO 20
c.... moved the 'if' above a few lines down - otherwise the dir reset in mover 
c.... did not work
c
      call gtgeom (asn, pl1, nclkey, nwds, ietype)
      DO 10 I=1,4
10    SRF(I,ISF) = PL1(I)
      IF (DIR.NE.0.) GOTO 20
      DIR = 1.
      IF ((PTE(1)-PL1(1)*PL1(4))*PL1(1)+(PTE(2)-PL1(2)*PL1(4))*PL1(2)
     X      +(PTE(3)-PL1(3)*PL1(4))*PL1(3).LT.0.) DIR = -1.
20    D1 = SRF(1,ISF)*PTE(1)+SRF(2,ISF)*PTE(2)+SRF(3,ISF)*PTE(3)
     X     -SRF(4,ISF)
      SRF(5,ISF) = PTE(1)-SRF(1,ISF)*D1
      SRF(6,ISF) = PTE(2)-SRF(2,ISF)*D1
      SRF(7,ISF) = PTE(3)-SRF(3,ISF)*D1
      GOTO 60
C                                            --- SURFACE
40    if (dir.eq.0.) then
        call sfinit (asw, isf, u, v)
        ifl(330+isf) = 0
      endif
      DO 50 I=1,3
50    SRF(I+7,ISF) = PTE(I)
      ISRF = ISF
      sc(isf+143) = asw
      CALL SURFPN (U, V, 1)
      IF (DIR.NE.0.) GOTO 60
      DIR = 1.
      IF ((PTE(1)-SRF(5,ISF))*SRF(1,ISF)+(PTE(2)-SRF(6,ISF))*SRF(2,ISF)
     X     +(PTE(3)-SRF(7,ISF))*SRF(3,ISF).LT.0.) DIR=-1.

60    DO 70 I=1,4
70    SS(I) = SRF(I,ISF)*DIR
 
      DO 80 I=5,7
80    SS(I) = SRF(I,ISF)

999   RETURN
      END

C **********************************************************************
C **  SUBROUTINE NAME: SFPT1
C **
C **  LAST REVISION:
C **  PURPOSE OF SUBROUTINE: PROJECT POINT TO SURFACE
C **  Unlike sfpt, the surface normal is set along the tool axis vector
C **  INPUT -
C **    ASW      - ASSOCIATED WORD OF SURFACE
C **    PTE      - POINT TO PROJECT
C **    ISF      - INDEX TO SURFACE TABLE
C **    DIR      - SIDE OF SURFACE
C **    U        - STARTING U VALUE
C **    V        - STARTING V VALUE
C **  OUTPUT -
C **    U        - ENDING U VALUE
C **    V        - ENDING V VALUE
C **    SS       - SURFACE TABLE
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE SFPT1 (ASW, PTE, ISF, DIR, U, V, SS)

      INCLUDE 'com4a.com'
      include 'mocom.com'

      REAL*8 ASW, PTE(3)
      REAL*4 U, V, DIR, SS(9)
      INTEGER*2 ISF
C                      MOTION COMMON EQUIVALENCES
      real*4 srf(10,4)
      equivalence (srf,s)

      INTEGER*2 ISRF
      EQUIVALENCE (IFL(54), ISRF)
      INTEGER*2 I

      if (dir.eq.0.) then
        call sfinit (asw, isf, u, v)
        ifl(330+isf) = 0
      endif
      DO 50 I=1,3
50    SRF(I+7,ISF) = PTE(I)
      ISRF = ISF
      sc(isf+143) = asw
      CALL SURFPN (U, V, 1)
      IF (DIR.NE.0.) GOTO 60
      DIR = 1.
      IF (sc(4)*SRF(1,ISF)+sc(5)*SRF(2,ISF)+sc(6)*SRF(3,ISF).LT.0.)
     X      DIR=-1.

60    DO 70 I=1,4
70    SS(I) = SRF(I,ISF)*DIR
 
      DO 80 I=5,7
80    SS(I) = SRF(I,ISF)

999   RETURN
      END

C*********************************************************************
C*    E_SUBROUTINE     : subroutine sfpt2(nclkey,pe,u8,v8,unflg,dir8,psf,vsf)
C*      Project a point onto a surf.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey       - Key of surf.
C*          pe           - Point to project.
C*          u8           - Starting u value
C*          v8           - Starting v value
C*          unflg        - 0 return point in current units and model space
C*                         1 return point in inches and world space.
C*          dir8         - 0 for first call, returned value for subsequent
C*                         calls.
C*       OUTPUT :
C*          dir8         - 1 or -1 to indicate side of sf.
C*          psf          - Point on surf.
C*          vsf          - Surface normal vector.
C*          ierr         - 0 = no error, < 0 = warning, > 0 error.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
      subroutine sfpt2(nclkey, pe, u8, v8, unflg, dir8, psf, vsf, ierr)

      include 'com.com'
      include 'mocom.com'
 
      integer*4 nclkey, unflg, ierr
      real*8 u8, v8, dir8, pe(3), psf(3), vsf(3)
 
      real*8 asw
      real*4 ss(7), dir
      integer*2 isf, nwds, ietype, ifl264, ifl2
      logical lhld

      isf = 3
      u = u8
      v = v8
      nwds = 1
      ietype  = 9
      call ptdsc3(nclkey,nwds,ietype,asw)
      dir = dir8
      if (unflg.eq.1) then
        ifl264 = ifl(264)
        ifl(264) = 0
        lhld = lwrk
        lwrk = .false.
      endif

      ifl2 = ifl(2)
      ifl(2) = 0
      call sfpt (asw, pe, isf, dir, u, v, ss)
      ierr = ifl(2)
      if (ierr .eq. 0 .and. ifl(385).lt.0) ierr = 128
      ifl(2) = ifl2

      if (unflg.eq.1) then
        ifl(264) = ifl264
        lwrk = lhld
      endif
      if (ierr.lt.1) then
        psf(1) = ss(5)
        psf(2) = ss(6)
        psf(3) = ss(7)
        vsf(1) = ss(1)
        vsf(2) = ss(2)
        vsf(3) = ss(3)
        u8 = u
        v8 = v
        dir8   = dir
      endif
 
      return
      end

C **********************************************************************
C **  SUBROUTINE NAME: SFPTVC
C **
C **  PURPOSE OF SUBROUTINE: PROJECT POINT TO SURFACE along a vector.
C **  Fortran version of ncl_pt_proj_sf
C **  INPUT -
C **    ASW      - ASSOCIATED WORD OF SURFACE
C **    PTE      - POINT TO PROJECT
C **    ISF      - INDEX TO SURFACE TABLE
C **    PRVEC    - projection vector
C **    UCUR     - STARTING U VALUE
C **    VCUR     - STARTING V VALUE
C **  OUTPUT -
C **    SRF      - SURFACE TABLE
C **    ierr     - > 0 iff error
C **
C **********************************************************************
C **********************************************************************
 
      SUBROUTINE SFPTVC (ASW,PTE,prvec,ISF,ucur,vcur,ierr)

      INCLUDE 'com.com'
      include 'mocom.com'

      REAL*8 ASW,PTE(3),prvec(3)
      REAL*4 ucur,vcur
      INTEGER*2 ISF,ierr
C                      MOTION COMMON EQUIVALENCES
      real*4 srf(10,4)
      equivalence (srf,s)

      INTEGER*2 ISRF
      EQUIVALENCE (IFL(54), ISRF)

      real*8 umin,vmin,umax,vmax,u,du,v,dv,u0,v0,tol,del,co,co1
      real*8 sv(9),pti(3),spt(3),vc(3),snorm(3),f_dot,f_dist
      REAL*4 uu,vv
      integer*2 nu,nv,i,j,isrfp

      isrfp = isrf
      ISRF = ISF
      tol = sc(27)
c
c.....  find surface point near pte
c
      if (ifl(330+isrf).eq.1) then
        call gtmmuv(isrf,umin,vmin,umax,vmax)
      else
        umin = 0
        vmin = 0
        umax = 1
        vmax = 1
      endif

      u0 = ucur
      v0 = vcur
      do i = 1,3
        spt(i) = srf(4+i,isrf)
        pti(i) = pte(i)
      enddo
      d0 = f_dist (spt,pti)

         nu = 10
         nv = 10

         du = (umax-umin)/(nu-1)
         dv = (vmax-vmin)/(nv-1)

         do i = 1,nu
            u = umin + (i-1)*du
            u = dmin1(umax,dmax1(umin,u))
            do j = 1,nv
               v = vmin + (j-1)*dv
               v = dmin1(vmax,dmax1(vmin,v))
               call uevsft(u,v,isrf,sv,ierr)
               if (ierr .eq. 466) goto 999
	         del = f_dist (sv,pti)
               if (del .lt. d0) then
                  d0 = del
                  u0 = u
                  v0 = v
                  spt(1) = sv(1)
                  spt(2) = sv(2)
                  spt(3) = sv(3)
               endif
            enddo
         enddo

      DO I=1,3
        SRF(I+4,ISF) = spt(I)
        SRF(I+7,ISF) = pti(I)
      enddo

      if (d0 .lt. tol) goto 999

      uu = u0
      vv = v0

      CALL SURFPN (uu,vv,1)
      if (ifl(2) .gt. 0) goto 99

      do i = 1,3
        spt(i) = srf(i+4,isf)
        vc(i) = spt(i) - pti(i)
        snorm(i) = srf(i,isf)
      enddo

      del = dabs(f_dot (vc,snorm))
      if (del .lt. tol) goto 999
c
c..... move the point along the projection vector until it is on surface
c
      k = 1
10    k = k+1

      co = dabs(f_dot(prvec,snorm))
      if (co .lt. 1.d-6) goto 99
      co1 = f_dot (prvec,vc)
      if (co1 .lt. 0) co = -co
      co1 = del/co
      
      do i = 1,3
        pti(i) = pti(i) + co1*prvec(i)
        srf(i+7,isrf) = pti(i) 
      enddo

      CALL SURFPN (uu,vv,1)
      if (ifl(2) .gt. 0) goto 99
      do i = 1,3
        spt(i) = srf(i+4,isf)
        vc(i) = spt(i) - pti(i)
        snorm(i) = srf(i,isf)
      enddo

      del = dabs(f_dot (vc,snorm))

      if (del .lt. tol) goto 999

      if (k .lt. 20) goto 10

99    ierr = 1
      if (ifl(2) .gt. 0) ifl(2) = 0

999   if (ierr.eq.0 .and. isrf.eq.4) then
        ucur = uu
        vcur = vv
      endif
      isrf = isrfp
      return
      end
