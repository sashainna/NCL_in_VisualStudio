c*****************************************************
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       sfprim.f , 25.1
c**    DATE AND TIME OF LAST  MODIFICATION
c**       04/29/15 , 15:10:41
c**
c*****************************************************
c**
c** COPYRIGHT (c) 2000 Numerical Control Computer Sciences
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name: sfprim
c **
c **  purpose of subroutine: to analyze and show surface(s) primitive
c **  type and parameters
c **********************************************************************
c **********************************************************************
 
      subroutine sfprim
 
      include 'com4a.com'
 
      PARAMETER (MAXPT=50)
      PARAMETER (MAXPN=20)
      PARAMETER (MXSFHD=(2+(MAXPN+1)/2))
      PARAMETER (MXPNHD=(2+(MAXPT+1)/2))

      integer*2 ksn(4),kw(4),i4w(1200)
      integer*4 jsn(2),isfkey,i4sc10(20),ncl_sf_prim_analyz,ierr
      real*8 w(400),asn,panel(200),data(16)
      equivalence (aw,kw,i4w,w),(asn,jsn,ksn),(w,panel,apan,ipanel)
      equivalence (tv,ktv),(sc(10),i4sc10)
c
      ifl(44) = 9
      idtype = 8
      call parsit
      if (ityp.ne.2 .or.ist.ne.9) then
         call error(186)
         goto 99999
      end if

   10 continue
            call gtdesc(tv,isfkey,nwds,ietype)
            call sftype(isfkey,ietype)
            if (ietype.ne.91 .and. ietype.ne.29 .and. 
     x          ietype.ne.99 .and. ietype.ne.100) then
               ifl(2)=321
               goto 99999
            end if 
            ierr = ncl_sf_prim_analyz(isfkey,ietype,data)
            if (ierr.ne.0) then
              call error (477)
              goto 99999
            end if
            call ushow
            if (nextyp.eq.11) then
               goto 99999
            end if
            idtype = 9
            call parsit
            if (ityp.ne.2 .or.ist.ne.9) then
               call error(186)
               goto 99999
            end if
      go to 10

99999 return
      end

C **********************************************************************
C **  SUBROUTINE NAME: NSRFTYP
C **
C **  PURPOSE OF SUBROUTINE: to obtain the surface primitive type
C **
C **********************************************************************

      subroutine nsrftyp

      include 'com8a.com'

      integer*2 ietype, nwds
      integer*4 isfkey, ncl_get_sf_primtyp, ierr

      call parser
      if (ityp.ne.5.or.ist.ne.6) goto 9309
      call parser
      if (ityp .eq. 2 .and. ist .eq. 14 .and. nextyp .eq. 6) call exprs2
      if (ifl(2).ne.0) goto 999
      if (ityp.ne.2.or.ist.ne.SURF) goto 9186
      if (nextyp.ne.7) goto 9310
      call gtdesc(tv,isfkey,nwds,ietype)
      call sftype(isfkey,ietype)
      if (ietype.ne.91 .and. ietype.ne.29 .and. 
     x    ietype.ne.99 .and. ietype.ne.100) goto 9477
      ierr = ncl_get_sf_primtyp(isfkey,ietype)
      if (ierr.ne.0) goto 9477
      call parser
      tv = ietype
      goto 999
c                   expression too long
9059  ifl(2)=59
      goto 999
c                   Surface expected
9186  ifl(2)=186
      goto 999
c                   left paren expected
9309  ifl(2)=309
      isvinx=inx
      goto 999
c                   right paren expected
9310  ifl(2)=310
      goto 999
c                   surf analysis failed
9477  ifl(2)=477
      goto 999

999   return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine ptprim(nclkey,primtyp,primdat)
C*       Put the primitive type and data into a surface structure,
C*       transformed for modsys, units and refsys.
C*    PARAMETERS
C*       INPUT  :
C*          nclkey   - Key of surface.
C*          primtyp  - Primitive type.
C*          primdat  - Primitive data.
C*       OUTPUT :
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine ptprim(nclkey,primtyp,primdat)

      include 'com.com'
      include 'wrksys.com'

      integer*4 nclkey
      integer*2 primtyp
      real*8 primdat(16)

      real*8 f_dot, f_mag
      real*8 fct, ufct /0.039370079d0/
      integer*2 i2v3 /3/, i2v4 /4/

c
c...Plane
c
      fct = 1.0d0
      if (primtyp.eq.3) then
        if (ifl(72).eq.1) then
          call conent (primdat, sc(56), i2v4)
          call conent (primdat(5), sc(56), i2v3)
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat(5), primdat(5), ufct)
        endif
        if (lwrk) then
          call conent (primdat, wrkmx, i2v4)
          call conent (primdat(5), wrkmx, i2v3)
        endif
        call unitvc (primdat,primdat)
        primdat(4) = f_dot(primdat,primdat(5))
c
c...Sphere
c
      else if (primtyp.eq.4) then
        if (ifl(72).eq.1) then
          call conent (primdat, sc(56), i2v3)
          fct = f_mag(sc(56))
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (lwrk) then
          call conent (primdat, wrkmx, i2v3)
          fct = fct*wrkscl
        endif
        primdat(4) = primdat(4)*fct
c
c...Cylinder
c
      else if (primtyp.eq.5) then
        if (ifl(72).eq.1) then
          call conent (primdat, sc(56), i2v3)
          call conent (primdat(4), sc(56), i2v4)
          fct = f_mag(sc(56))
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (lwrk) then
          call conent (primdat, wrkmx, i2v3)
          call conent (primdat(4), wrkmx, i2v4)
          fct = fct*wrkscl
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(7) = primdat(7)*fct
        primdat(8) = primdat(8)*fct
c
c...Cone
c
      else if (primtyp.eq.6) then
        if (ifl(72).eq.1) then
          call conent (primdat, sc(56), i2v3)
          call conent (primdat(4), sc(56), i2v4)
          fct = f_mag(sc(56))
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (lwrk) then
          call conent (primdat, wrkmx, i2v3)
          call conent (primdat(4), wrkmx, i2v4)
          fct = fct*wrkscl
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(8) = primdat(8)*fct
        primdat(9) = primdat(9)*fct
c
c...Torus
c
      else if (primtyp.eq.8) then
        if (ifl(72).eq.1) then
          call conent (primdat, sc(56), i2v3)
          call conent (primdat(4), sc(56), i2v4)
          fct = f_mag(sc(56))
        endif
        if (ifl(264).eq.1) then
          call vctmsc (primdat, primdat, ufct)
          fct = fct*ufct
        endif
        if (lwrk) then
          call conent (primdat, wrkmx, i2v3)
          call conent (primdat(4), wrkmx, i2v4)
          fct = fct*wrkscl
        endif
        call unitvc (primdat(4),primdat(4))
        primdat(7) = primdat(7)*fct
        primdat(8) = primdat(8)*fct
      endif

      call ncl_put_sf_primdat(nclkey,primtyp,primdat)

9999  return
      end
