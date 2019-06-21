C*********************************************************************
C*    NAME         :trimsf.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       trimsf.f , 25.1
C*     DATE AND TIME OF LAST MODIFICATION
C*       04/29/15 , 15:10:48
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine trimsf1
C*       Create a trimmed surface.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine trimsf1

      include 'com.com'
      include 'mocom.com'

      real*8 ptx(6),tmp
      real*4 uv(2),dirmod(3),uvmod(2),u,v,bx1(4)
      equivalence (sc(19),uv),(sc(14),uvmod)
      integer*4 key1,key2,k1ext,nclkey
      integer*2 nwds,ityp1,ityp2,ietype,ncvs,iredef,mmod,isrf
      integer*2 itsk,iextyp
      equivalence (ifl(54),isrf)
      integer*2 iblnk /1/

      call gtdesc (sc(11),key1,nwds,ityp1)
      call gtdesc (sc(12),key2,nwds,ityp2)
      ncvs = isc10(3)
      iredef = isc10(4)
c
c..... iredef = 0 is for the 'surf/redef' command
c..... iredef = 1 is for the 'redef/sf1,...' command
c..... iredef = 2 is for the 'redef/out,cv1' command
c..... iredef = 3 is for the 'redef/sf1,remove,2,1,3' command
c
      iextyp = 0

      if (iredef .eq. 1) then

        if (ncvs.eq.0 .and. key2.eq.0) then
c
c..... REDEF/sf1:
c..... get base if trimmed, do natural extension if revolve-primitive
c
          call sftype (key1, ietype)
          if (ietype .eq. TRIMSF) then
            call trmbse (key1,nclkey,ifl(296),ifl(2))
          else
            defwf = (ifl(296) .eq. 1)
            call trmrev (key1,nclkey,ifl(296),ifl(2))
          endif
          goto 20
        endif

        isc10(3) = 0

        if (ityp1 .eq. 0) then
c
c..... REDEF/out,cv1: create a planar surface and trim
c
          ityp1 = PLANE
          if (ityp2 .eq. SURF) goto 9000
        endif

        if (ityp1 .eq. SURF) then
c
c..... REDEF/sf1,out,sf2[,mod]
c..... process the modifier
c
          mmod = sc(13)
          if (mmod .eq. 1) then
            dirmod(1) = uvmod(1)
            dirmod(2) = uvmod(2)
          else if (mmod .eq. 2) then
            dirmod(1) = 0
            dirmod(2) = 0
            dirmod(3) = 0
            if (sc(14) .eq. 638) then
              dirmod(1) = 1
            else if (sc(14) .eq. 641) then
              dirmod(1) = -1
            else if (sc(14) .eq. 639) then
              dirmod(2) = 1
            else if (sc(14) .eq. 642) then
              dirmod(2) = -1
            else if (sc(14) .eq. 640) then
              dirmod(3) = 1
            else if (sc(14) .eq. 643) then
              dirmod(3) = -1
            else
              mmod = 0
            endif
          else if (mmod .eq. 3) then
            if (sc(14) .eq. 663) mmod = 4
          else if (mmod .eq. 4) then
            call gtgeom(sc(14),ptx,nclkey,nwds,itype)
            u = uv(1)
            v = uv(2)
            isrf = 1
            call sfinit(sc(11),isrf,u,v)
            s(8,isrf) = ptx(1)
            s(9,isrf) = ptx(2)
            s(10,isrf) = ptx(3)
            call surfpn(u,v,0)
            if (ifl(2) .ne. 0) goto 9000
            mmod = 1
            dirmod(1) = u
            dirmod(2) = v
          else if (mmod .eq. 5) then
            call gtgeom(sc(14),ptx,nclkey,nwds,itype)
            mmod = 2
            dirmod(1) = ptx(1)
            dirmod(2) = ptx(2)
            dirmod(3) = ptx(3)
          else
            mmod = 0
          endif

          k1ext = 0
          if (key2 .eq. 0) then
            ncvs = ncvs + 1
            goto 15
          endif

          itsk = 2

          if (ityp2 .eq. CURVE .or. ityp2 .eq. CIRCLE) itsk = 3

          if (ityp2 .eq. CURVE) goto 15
c
c..... Skip cvio for all curves
c
c            call cvtype1 (key2,ityp2)
c            if (ityp2 .eq. 5) goto 15
c         endif


10        call cvio(itsk)
          if (ifl(2).eq.502 .and. iextyp.eq.0) then
            iextyp = isc10(4) ! primitive type
            if (iextyp .eq. PLANAR) then
              ifl(270) = 1
              sc(199) = 4
            else if (iextyp .ge. SPHERE) then
c
c..... make a larger surface and try again
c
              k1ext = 0
              call extprm (key1,itype,key2,k1ext,bx1,ifl(2))
              if (ifl(2).ne.0 .or. k1ext.eq.0) goto 9000
              nwds = 1
              ietype = SURF
              call ptdsc3 (k1ext,nwds,ietype,sc(11))
            else
              goto 9000
            endif
            itsk = 4
            err = .false.
            goto 10
          endif

          if (ifl(2).gt.0) goto 9000
          
          if (iextyp .eq. PLANAR) then
            k1ext = key1
            call extpln (k1ext,bx1)
          endif

15        if (k1ext .gt. 0) then
c
c..... if the original sf1 does not intersect sf2, we trim the extended surface
c..... by the intof curve so that the original sf1 is extended (not trimmed)
c
            nclkey = k1ext
            call extrdf (key1,bx1,ncvs,uv,nclkey,ifl(296),ifl(2))
          else
c
c..... the generic case: trim sf1 by the its intersection with sf2
c
            call trmrdf (key1,key2,ityp2,ncvs,uv,mmod,dirmod,nclkey,
     x                                         ifl(296),k1ext,ifl(2))
          endif
        else if (ityp1.eq.PLANE) then
          if (ityp2.eq.SURF) then
c
c..... REDEF/pl1,out,sf2:
c..... intersect plane and surface, then create a trimmed planar surface
c
            tmp = sc(11)
            sc(11) = sc(12)
            sc(12) = tmp
            call cvio(1)
            if (ifl(2).gt.0) goto 9000
            key2 = 0
          endif          
          call trmpln (key1,key2,ncvs,nclkey,ifl(296),ifl(2))
        endif          
      else if (iredef.eq.0 .or. iredef.eq.2) then
        call ptrmsf (key1, key2, ncvs, uv, nclkey, ifl(2))
      else if (iredef .eq. 3) then
        call trmrmv (key1,nclkey,ifl(296),ifl(2))
      endif

20    if (nclkey.eq.0) goto 9000
      nwds = 1
      ietype = SURF
      call ptdsc3 (nclkey, nwds, ietype, tv)
      rest = tv
c
c..... qar 95048 - defwf is on iff went through trmrev, in which case
c..... we need to call vstore to record the new surface
c
      if (defwf) call vstore
      if (.not. ldspsf) call blkgeo (nclkey, iblnk)

      if (ifl(2).eq.0) then
        if (iredef.eq.2 .and. ifl(296).eq.0) then
          call vstore
          call dspent (nclkey,ietype)
        endif
        goto 9999
      endif

9000  if (ifl(2).eq.0) ifl(2) = 163
      err = .true.


9999  ifl(270) = 0
      ifl(212) = 0
      call ncl_free_uv

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine extpln (skey,uvmod)
C*       Create an expanded planar surface so that it includes the
C*       calculated plane/surface intersection.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine extpln (skey,bx1)

      include 'com.com'
      include 'mocom.com'

      integer*4 skey
      real*4 bx1(4)

      integer*2 k,np,isrf
      real*8 pte(3),sv(9),u8,v8,asw
      real*8 cp0(3),cp1(3),cp2(3),cp3(3)
      real*4 u,v,u0,u1,v0,v1,ui,vi,u00,u11,v00,v11

      equivalence (ifl(54),isrf)

      call ncl_getn_uv (pte,np)

      ui = pte(1)
      vi = pte(2)

      a = sc(199)
      b = (a - 1.)/2
      isrf = 1
      if (ifl(270).ne.isrf .or. b.lt.0) then
        ifl(2) = 163
        return
      endif
c
c..... the original surface limits inside the extension
c
      u0 = b/a
      u1 = (1 + b)/a
      v0 = b/a
      v1 = (1 + b)/a
      u00 = u0
      u11 = u1
      v00 = v0
      v11 = v1
c
c..... find the minimal expansion that includes the intof points
c
      do k = 1, np
        call ncl_get_ent(k,pte)
        u = pte(1)
        v = pte(2)
        if (u .lt. u0) u0 = u
        if (u .gt. u1) u1 = u
        if (v .lt. v0) v0 = v
        if (v .gt. v1) v1 = v
        u8 = u
        v8 = v
        call uevsft(u8,v8,isrf,pte,ifl(2))
        call ncl_replace_uv (k,pte)
      enddo
c
c..... Get the four XYZ corner points at the expanded UV box corners
c
      u8 = u0
      v8 = v0
      call uevsft(u8,v8,isrf,sv,ifl(2))
      call vctovc (sv,cp0)
      u8 = u1
      v8 = v0
      call uevsft(u8,v8,isrf,sv,ifl(2))
      call vctovc (sv,cp1)
      u8 = u0
      v8 = v1
      call uevsft(u8,v8,isrf,sv,ifl(2))
      call vctovc (sv,cp2)
      u8 = u1
      v8 = v1
      call uevsft(u8,v8,isrf,sv,ifl(2))
      call vctovc (sv,cp3)
c
c..... create a new planar surface by the corner points
c
      call plnsrf (cp0,cp1,cp2,cp3,skey,asw,ifl(2))

      ifl(270) = 0
      call sfinit(asw,isrf,ui,vi)
c
c..... replace the intof curve points by the UV points on the new surface
c
      do k = 1, np
        call ncl_get_ent(k,pte)
        s(8,isrf)=pte(1)
        s(9,isrf)=pte(2)
        s(10,isrf)=pte(3)
        call surfpn(u,v,0)
        pte(1) = u
        pte(2) = v
        pte(3) = 0
        call ncl_replace_uv (k,pte)
      enddo
c
c..... bx1 holds the original surface UV-box inside the new surface
c
      a = u1 - u0
      bx1(1) = (u00 - u0)/a
      bx1(2) = (u11 - u0)/a
      a = v1 - v0
      bx1(3) = (v00 - v0)/a
      bx1(4) = (v11 - v0)/a

      return
      end
