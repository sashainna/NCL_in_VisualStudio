c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       gtprimt.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:10:10
c**
c*****************************************************
c**
c** COPYRIGHT (c) 2000 Numerical Control Computer Sciences
c**
c **********************************************************************

C*********************************************************************
C*    E_SUBROUTINE     : subroutine gtprimt(nclkey,iref,primtyp,primdat)
C*       Get the primitive type and data for a surface, transformed for
C*       modsys and units and optionally refsys.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey   - Key of surface.
C*          iref     - =1 transform through refsys
C*       OUTPUT :
C*          primtyp  - Primitive type.
C*          primdat  - Primitive data.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine gtprimt(nclkey,iref,primtyp,primdat)

      include 'com.com'
      include 'wrksys.com'

      integer*4 nclkey
      integer*2 iref,primtyp
      real*8 primdat(16)

      real*8 f_dot, f_mag
      real*8 fct, ufct /25.4d0/
      integer*2 i2v3 /3/, i2v4 /4/

      call ncl_get_sf_primdat(nclkey,primtyp,primdat)
      if (primtyp.eq.0) call ncl_sf_prim_analyz(nclkey,primtyp,primdat)
      if (primtyp.le.2) goto 9999
      if (.not.lwrk .and. ifl(264).eq.0 .and. iref.eq.0) goto 9999
c
c...Plane
c
      fct = 1.0d0
      if (primtyp.eq.3) then
        if (lwrk) then
          call conent (primdat, invwrk, i2v4)
          call conent (primdat(5), invwrk, i2v3)
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat(5), primdat(5), ufct)
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v4)
          call conent (primdat(5), sc(68), i2v3)
        endif
        call unitvc (primdat,primdat)
        primdat(4) = f_dot(primdat,primdat(5))
c
c...Sphere
c
      else if (primtyp.eq.4) then
        if (lwrk) then
          call conent (primdat, invwrk, i2v3)
          fct = 1.0d0/wrkscl
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v3)
          fct = fct*f_mag(sc(68))
        endif
        primdat(4) = primdat(4)*fct
c
c...Cylinder
c
      else if (primtyp.eq.5) then
        if (lwrk) then
          call conent (primdat, invwrk, i2v3)
          call conent (primdat(4), invwrk, i2v4)
          fct = 1.0d0/wrkscl
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v3)
          call conent (primdat(4), sc(68), i2v4)
          fct = fct*f_mag(sc(68))
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(7) = primdat(7)*fct
        primdat(8) = primdat(8)*fct
c
c...Cone
c
      else if (primtyp.eq.6) then
        if (lwrk) then
          call conent (primdat, invwrk, i2v3)
          call conent (primdat(4), invwrk, i2v4)
          fct = 1.0d0/wrkscl
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v3)
          call conent (primdat(4), sc(68), i2v4)
          fct = fct*f_mag(sc(68))
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(8) = primdat(8)*fct
        primdat(9) = primdat(9)*fct
c
c...Torus
c
      else if (primtyp.eq.8) then
        if (lwrk) then
          call conent (primdat, invwrk, i2v3)
          call conent (primdat(4), invwrk, i2v4)
          fct = 1.0d0/wrkscl
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v3)
          call conent (primdat(4), sc(68), i2v4)
          fct = fct*f_mag(sc(68))
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(7) = primdat(7)*fct
        primdat(8) = primdat(8)*fct
      endif

9999  return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine primpln(nclkey,iref,pln)
C*       Get the primitive plane data for a surface, transformed for
C*       modsys and units and optionally refsys.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey   - Key of surface.
C*          iref     - transform through refsys iff 1
C*       OUTPUT :
C*          pln      - Primitive data.
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine primpln(nclkey,iref,pln)

      include 'com.com'
      include 'wrksys.com'

      integer*4 nclkey
      integer*2 iref
      real*8 pln(4)

      real*8 f_dot
      real*8 ufct /25.4d0/
      integer*2 i2v3 /3/, i2v4 /4/
      integer*2 primtyp
      real*8 primdat(16)

      call ncl_get_sf_primdat(nclkey,primtyp,primdat)
      if (primtyp.eq.0) call ncl_sf_prim_analyz(nclkey,primtyp,primdat)
      if (primtyp.ne.3) goto 9999
      if (.not.lwrk .and. ifl(264).eq.0 .and. iref.eq.0) goto 10

        if (lwrk) then
          call conent (primdat, invwrk, i2v4)
          call conent (primdat(5), invwrk, i2v3)
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat(5), primdat(5), ufct)
        endif
        if (iref.eq.1) then
          call conent (primdat, sc(68), i2v4)
          call conent (primdat(5), sc(68), i2v3)
        endif

10      call unitvc (primdat,pln)
        pln(4) = f_dot(primdat,primdat(5))

9999  return
      end
