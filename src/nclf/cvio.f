C*********************************************************************
C*    NAME         :  cvio.f
C*       CONTAINS:
c*
c*                 cvptsn
c*                 cvio
c*                 cvcntr
c*                 sfsio
c*                 cvouts
c*
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       cvio.f , 25.2
C*     DATE AND TIME OF LAST MODIFICATION
C*       08/18/15 , 10:58:11
C*********************************************************************
C
c*********************************************************************
c*  subroutine name: cvptsn (itsk,n,px,vx)
c*
c*  purpose of subroutine: this routine handles the curve definition:
c*                          curve/intof,sf/pl,sf/pl,pt
c*
c*      a curve as the intersection of a surf and another surf or plane
c*      sf/pl = a surf or plane id.  one of the two sf/pl entities must
c*      be a surface.  the curve defined is along the intersection of
c*      these two sf/pl entities.  no extention of surfaces are used.
c*      the optional near point specifies which of curve segment should
c*      be used in cases where more than one curve segment is formed by
c*      the intersection.
c*
c*      the method used for this curve definition is to generate a
c*      series of points along the intersection of the specified surf/
c*      plane geometry then pass them to the curve/fit routine for the
c*      final curve canonical data generation.
c*
c*    PARAMETERS
C*       INPUT  :
C*          itsk    - 1 = generate xyz points of the intersection
C*                    2 = generate uv points of the intersection
C*       OUTPUT :
C*          n       - number of points generated
c*          px      - array of xyz points
c*          vx      - slope at the first and last point in px
c*
c*********************************************************************

      subroutine cvptsn (itsk,n,px,vx)

      include 'com4a.com'
      include 'mocom.com'
c
      integer*2 itsk
      integer*4 n
      real*8 px(3000),vx(6)
c
c... motion common equivalences
c
      real*4 ad(300)
      equivalence (d,ad)

      common/pblok/p

      real*8 p(400)
      real*4 ap(800)
      integer*2 ip(1600)
      equivalence (p,ap,ip)

      real*8 ptx(12),pts(3),pte(3),
     *       asn,svsc35,svsc10,svsc11,d1,d2,tolsq,zero/0.d0/,
     *       f_dist
      integer*4 nclkey
      integer*2 ksn(4), nwds, itype, iend,itry_sf
      equivalence (asn,ksn)
      equivalence (isrf,ifl(54)),(ia,ifl(51)),(ntk,ifl(79))
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
      integer*2 ifl2
      logical lerr

      iend = 1
      irpt=isc10(3)
c          save current part surface and process a psis statement
      svsc35=sc(35)
      asn=0.
      ksn(1)=713
      sc(10)=asn
      call motimm
      if (ifl(2) .gt. 0) goto 997
c
c...get near pt (use sf center if none given)
c
      if (irpt.ne.0) then
         itry_sf = 0
         asn = sc(13)
         call gtgeom(asn,ptx,nclkey,nwds,itype)
         goto 9
      else
         itry_sf = 1
         asn = sc(11)
         call xyzvc (zero,zero,zero,ptx)
      endif

5     continue

      u = u1
      v = v1
      call srfevl(asn,u,v,ptx)
      ifl(2) = 0
      err = .false.

9     call conv8_4 (ptx,t(1,3),3)
c
c...set up to move
c
      sc(11) = sc(12)
      call uvprmv
      if (ifl(2) .gt. 0) goto 997
c
c... move to point on both surfaces
c
      maxp = ifl(91)*3
      if (maxp.gt.3000) maxp = 3000
      call uvmove (1,maxp,pts,px,vx,0)
c
c... aak 08-jan-1998: new error handling:
c... if error occured:
c...    1. if nearpt was given, try center of 1st surface as nearpt;
c...       if still error, try center of the 2nd srf as nearpt
c...    2. if nearpt was not given, try center of the 2nd srf as nearpt
c
      if (ifl(2).ge.1) then
         if (itry_sf.eq.2) goto 997
         itry_sf = itry_sf + 1
         ifl(2) = 0
         err = .false.
         call xyzvc (zero,zero,zero,ptx)
         asn=sc(10+itry_sf)
         if (ksn(4).eq.PLANE) goto 9

         if (itry_sf.eq.2) then
            d1 = sc(19)
            sc(19) = sc(20)
            sc(20) = d1
         endif

         goto 5
      endif
c
c... move - stop at first sf edge found
c
15    call conv4_4 (t(1,ia),t(1,3),9)
c
c... dont use wrapped surface
c
      noclos = .true.
      call uvmove (2,3000,pts,px,vx,0)
      if (ifl(2) .gt. 0) goto 997
c
c...move back to another sf edge
c
      if (ntk.ne.0) then
         call conv8_4 (pte,t(1,3),3)
         call conv8_4 (px(ntk-2),t(1,3),3)
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
      endif

      isav = itsk

      call uvmove (2,3000,pts,px,vx,isav)
      if (ifl(2) .gt. 0) goto 997

      if (ntk.le.0) then
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
         call uvmove (2,3000,pts,px,vx,isav)
         if (ifl(2) .gt. 0) goto 997
      endif

      np = ntk/3
      if (np.lt.3) goto 90
c
c...eliminate points too close together
c...Apply only for ncl curves as ios surfaces, uv curves will
c...check points in interpolator
c
      if (itsk .ne. 2) then
         i=0
         tolsq=sc(27)**2
         np=ntk/3
      endif

90    CONTINUE

      if (np .lt. 2) then
        ifl(2) = 378
        goto 998
      endif
c
c...calculate distance from near point to first and last generated
c...points then decide which end is nearer the near point
c
      d1 = f_dist (ptx,pts)**2
      if (sc(169).gt.9.00699 .and. sc(169).lt.9.00801) then
c
c...this code is wrong but fixes the problem of cv/io,pl,sf changing
c...direction of curve in releases 9.007 and 9.008 reported in
c...fsr 60281 IJD 4-Feb-00
c
          d2 = f_dist (ptx,px(ntk-2))**2
      else if (itsk .ne. 0) then
          call ncl_get_ent(np,pte)
          d2 = f_dist (ptx,pte)**2
      else
          d2 = f_dist (ptx,px(ntk-2))**2
      endif
      if (d1.gt.d2+tolsq*100.) then
        call ncl_revers_uv (pts,np)
      else
        call ncl_getn_uv (pts,np)
      endif

      if (itsk .eq. 1) then
         call ncl_weed_uv (sc(27),np)
      else
         call ncl_weed_uv (1.d-3,np)
      end if
      n   = np
      go to 9990
c
c...error exit.
c
997   if (ifl(2).eq.385) then
        write(errcom,9010)s(5,1),s(6,1),s(7,1)
9010    format(3f10.3)
      endif
c
c... aak 08-jan-1998: give "Impossible geometry case" error
c
      ifl(2) = 163
998   err=.true.
c
9990  continue
      noclos=.false.
c          restore the part surface in effect prior to this cv statement
      call gtdesc(svsc35,nclkey,nwds,itype)
      if (nclkey.eq.0) goto 9995
      svsc10=sc(10)
      svsc11=sc(11)
      ifl2 = ifl(2)
      lerr = err
      sc(11)=svsc35
      asn=0.
      ksn(1)=713
      sc(10)=asn
      ifl(2) = 0
      err = .false.
      call motimm
      sc(10)=svsc10
      sc(11)=svsc11
      if (lerr) then
        ifl(2) = ifl2
        err = lerr
      endif
      goto 99999

c          use z-zero default plane as part surface
9995  asn=0.
      ksn(1)=6
      d(1)=asn
      d(3)=0.
      d(4)=0.
      d(5)=1.
      d(6)=0.
      sc(35)=svsc35

99999 return
      end

c**********************************************************************
c*  subroutine name: cvio (itsk)
c*
c*  purpose of subroutine: this routine handles the curve definition:
c*                          curve/intof,sf/pl,sf/pl,pt
c*
c*      a curve as the intersection of a surf and another surf or plane
c*      sf/pl = a surf or plane id.  one of the two sf/pl entities must
c*      be a surface.  the curve defined is along the intersection of
c*      these two sf/pl entities.  no extention of surfaces are used.
c*      the optional near point specifies which of curve segment should
c*      be used in cases where more than one curve segment is formed by
c*      the intersection.
c*
c*      the method used for this curve definition is to generate a
c*      series of points along the intersection of the specified surf/
c*      plane geometry then pass them to the curve/fit routine for the
c*      final curve canonical data generation.
c*
c*    PARAMETERS
C*       INPUT  :
C*          itsk      1 = generate xyz points of the intersection
C*                    2 = generate uv points of the intersection
C*                   -1 = generate xyz points for fillet
C*       OUTPUT :
C*          NCL_uvintof - list containing curve points
c*
c*********************************************************************

      subroutine cvio (itsk)

      include 'com.com'
      include 'mocom.com'

      integer*2 itsk
c
c... motion common equivalences
c
      real*4 ad(300)
      equivalence (d,ad)

      common/pblok/p
      real*8 p(400)
      real*4 ap(800)
      integer*2 ip(1600)
      equivalence (p,ap,ip)

      common/cviofl/cvion,iadvcs
      logical cvion
      !integer*2 iadvcs, isrf
      integer*2 iadvcs

      real*8 ptx(12),pte(3),vx(6),sn(3),d1sv,sc144,
     *       asn,svsc35,svsc10,svsc11,d1,d2,zero/0.d0/,f_dist,f_dot
      integer*4 nclkey,key11,key12
      integer*2 ksn(4),nwds,itype,iprim1,iprim2,itry_sf,nsf
      equivalence (asn,ksn)
      equivalence (isrf,ifl(54)),(ia,ifl(51)),(ntk,ifl(79))
      real*4 u1,v1,u2,v2,uv(4), tt(30,3)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))

      integer*2 i,k,ione/1/,ivxfl,isf11,isf12
      real*4 u1sav,v1sav,u2sav,v2sav
      real*8 told,umin,umax,vmin,vmax,u8,v8
      integer*2 ifl2,np
      logical lv92,lv925,lv93,lv94,lerr,lcvsf,lredef,lno331
      
      !real*4 tt(30,3)
      
      tt(1,1)=1.0
      tt(1,2)=1.0
      tt(1,3)=1.0


      lv92 = sc(169).lt.9.249d0
      lv925 = sc(169).lt.9.25d0
      lv93 = sc(169).lt.9.349d0
      lv94 = sc(169).lt.9.449d0

      lno331 = .false.
      lredef = .false.
      if (itsk .gt. 2) then
        if (itsk .eq. 3) lno331 = .true.
        if (itsk .eq. 4) lredef = .true.
        itsk = 2
      endif

      if (itsk .eq. 1) then
        ivxfl = 1
      else
        ivxfl = 0
      endif

      lcvsf = .false.

      told = sc(27)
      if (ifl(264).eq.1) told = told*0.03937d0

      jtrim = 0
      nsf = 0
      ifl(331) = 0
      ifl(332) = 0

      itry_sf = 0
c
c..... set jtrim - the number of trimmed surfaces in the statement,
c..... for trimming the intersection to the trim boundaries.
c..... Note: at the moment we do not trim if cv/io is done for building
c..... a fillet surface
c
      if (itsk .ge. 1) then
        call gtdesc (sc(11),key11,nwds,itype)
        call sftype (key11, isf11)
        if (itype.eq.SURF .and. isf11.gt.NETSF .and. .not.lv925) nsf = 1
        if (isf11 .eq. TRIMSF) jtrim = 1
        call gtdesc (sc(12),key12,nwds,itype)
        if (itsk.eq.2 .and. nsf.eq.1 .and. (itype.eq.CURVE .or.
     x       itype.eq.LINE .or. itype.eq.CIRCLE)) lcvsf = .true.
        if (itype.eq.SURF) then
          call sftype (key12, isf12)
          if (isf12.gt.NETSF .and. .not.lv925) nsf = nsf + 1
          if (isf12 .eq. TRIMSF) then
            jtrim = jtrim + 1
            if (jtrim.eq.1 .or. itsk.eq.2) then
              asn = sc(11)
              sc(11) = sc(12)
              sc(12) = asn
              nclkey = key11
              key11 = key12
              key12 = nclkey
              i = isf11
              isf11 = isf12
              isf12 = i
            endif
          endif
        endif
      endif
      if (jtrim.gt.0 .and. .not.lv92) then
        u1sav = u1
        v1sav = v1
        u2sav = u2
        v2sav = v2
        i = 1
        ifl(330+i) = 1
        call tbdini (key11,i,told,ifl(330+i))
        if (ifl(2).eq.466) goto 997
        call gtmmuv(i,umin,vmin,umax,vmax)
        if (u1.lt.umin .or. u1.gt.umax) u1 = (umin + umax)/2
        if (v1.lt.vmin .or. v1.gt.vmax) v1 = (vmin + vmax)/2
        if (jtrim .gt. 1) then
          i = 2
          ifl(330+i) = 1
          call tbdini (key12,i,told,ifl(330+i))
          if (ifl(2).eq.466) goto 997
          call gtmmuv(i,umin,vmin,umax,vmax)
          if (u2.lt.umin .or. u2.gt.umax) u2 = (umin + umax)/2
          if (v2.lt.vmin .or. v2.gt.vmax) v2 = (vmin + vmax)/2
        endif
      endif

      if (itsk .eq. -1) itsk = 1

      irpt = isc10(3)
      if (nsf .eq. 0 .or. lcvsf .or.
     x  (lredef.and.ifl(270).eq.1)) goto 444
      if (nsf .eq. 2) then
        call ncl_get_sf_primtyp(key11,iprim1)
        if (iprim1 .ne. 3) goto 444
        call ncl_get_sf_primtyp(key12,iprim2)
        if (iprim2 .ne. 3) goto 444
      endif

      if (irpt .ne. 0) then
        asn = sc(13)
        call gtgeom(asn,ptx,nclkey,nwds,itype)
      endif
      sc144 = sc(144)
      d1sv = d(1)
      if (nsf .eq. 1) then
        call sfplio (key11,key12,irpt,ptx,np,ifl(2))
        if (ifl(2).ne.0 .and. .not.lv94) then
          ifl(2) = 0
          goto 444
        endif

        u = u1
        v = v1
        isrf = 1
        d1 = 1.
        call sfinit(sc(11),isrf,u,v)
        cvion = .true.
        if (np.gt.1 .and. iadvcs.eq.1 .and. .not.lv94) iadvcs = 2
        do k = 1, np
            call ncl_get_ent(k,pte)
            s(8,isrf)=pte(1)
            s(9,isrf)=pte(2)
            s(10,isrf)=pte(3)
            call surfpn(u,v,1)
            pte(1) = s(5,isrf)
            pte(2) = s(6,isrf)
            pte(3) = s(7,isrf)
            call ncl_replace_uv (k,pte)
c
c..... fsr 60994 - add tangent vectors to call ncl_fix_tol later
c
            if (iadvcs .eq. 2) then
              if (k .gt. 1) then
                call vcmnvc (pte,ptx,vx)
                d2 = f_dot (vx,sn)
                call avcplbvc (d1,vx,-d2,sn,vx(4))
                call ncl_push_uv (vx(4))
              endif
              call vctovc (pte,ptx)
              sn(1) = s(1,isrf)
              sn(2) = s(2,isrf)
              sn(3) = s(3,isrf)
            endif
        enddo
c
c..... last tangent vector
c
        if (iadvcs .eq. 2) then
          d2 = f_dot (vx,sn)
          call avcplbvc (d1,vx,-d2,sn,vx(4))
          call ncl_push_uv (vx(4))
        endif

        cvion = .false.
      else
        call flatio (key11,key12,irpt,ptx,np,ifl(2))
      endif
      if (ifl(2) .eq. 0) then
        ivxfl = 0
        svsc35 = sc(35)
        ifl(212) = 2
        if (nsf.eq.2 .and. np.lt.2) then
          isc10(4) = iprim1
          ifl(2) = 502
          err=.true.
        else if (itsk .eq. 2) then
          u = u1
          v = v1
          isrf = 1
          call sfinit(sc(11),isrf,u,v)
          cvion = .true.
          do k = 1, np
            call ncl_get_ent(k,pte)
            s(8,isrf)=pte(1)
            s(9,isrf)=pte(2)
            s(10,isrf)=pte(3)
            call surfpn(u,v,1)
            pte(1) = u
            pte(2) = v
            pte(3) = zero
            call ncl_replace_uv (k,pte)
          enddo
          cvion = .false.
        endif
        d(1) = d1sv
        sc(144) = sc144
        call gtdesc(svsc35,nclkey,nwds,itype)
        if (nclkey .eq. 0) goto 9995
        goto 99999
      else
        ifl(2) = 0
      endif
c
c..... save current part surface and process a psis statement
c
444   continue
      svsc10=sc(10)
      svsc11=sc(11)
      svsc35=sc(35)
      asn=0.
      ksn(1)=713
      sc(10)=asn
      call gtdesc (sc(11),key11,nwds,itype)
      if (itype .eq. SURF) ifl(46) = 1
      call motimm
      ifl(46) = 0
      if (ifl(2) .gt. 0) goto 997
c
c...get near pt (use sf center if none given)
c
      if (irpt.ne.0) then
         asn = sc(13)
         call gtgeom(asn,ptx,nclkey,nwds,itype)
         goto 9
      else
         if (lredef .and. ifl(270).eq.1) then
           itry_sf = 1
           u8 = u1
           v8 = v1
           isrf = 1
           call uevsft(u8,v8,isrf,ptx,ifl(2))
           if (ifl(2) .gt. 0) goto 997
           goto 9
         endif
         if (.not.lv925 .and. nsf.gt.0 .and. .not.lcvsf) then
           call frstpt (key11,key12,pte,vx,ifl(2))
           if (ifl(2) .eq. 0) then
             asn = sc(11)
             u = u1
             v = v1
             call srfevl(asn,u,v,ptx)
             err = .false.
             sc(11) = sc(12)
             do i = 1,3
               t(i,3) = pte(i)
               t(i+3,3) = 0.
               t(i+6,3) = vx(i)
             enddo
             call uvprmv
             if (ifl(2) .gt. 0) goto 997
             goto 15
           else
             ifl(2) = 0
           endif
         endif
         itry_sf = 1
         asn = sc(11)
         call xyzvc (zero,zero,zero,ptx)
      endif

5     continue
      if (ifl(331).eq.1 .and. lno331) ifl(331) = 0
      u = u1
      v = v1
      call srfevl(asn,u,v,ptx)
      ifl(2) = 0
      err = .false.

9     call conv8_4 (ptx,t(1,3),3)
c
c...set up to move
c
      sc(11) = sc(12)
      call uvprmv
      if (ifl(2) .gt. 0) goto 997
c
c... move to point on both surfaces
c
      if (lv92) then
        maxp = ifl(91)*12
        if (maxp .lt. 3000) maxp = 3000
      else
        maxp = 50
        !maxp = 3000
      endif
      isav  = 0
      if (itry_sf .gt. 2) isav = 1
      call uvmove (1,maxp,pte,pte,vx,isav)
c
c... if error occured:
c...    1. if nearpt was given, try center of 1st surface as nearpt;
c...       if still error, try center of the 2nd srf as nearpt
c...    2. if nearpt was not given, try center of the 2nd srf as nearpt
c
      if (ifl(2).ge.1) then
10       if ((lv92 .and. itry_sf.ge.2) .or. itry_sf.ge.4) goto 997
         itry_sf = itry_sf + 1
         if (itry_sf.eq.1 .or. itry_sf.eq.3) then
           asn = svsc11
         else
           asn = sc(12)
         endif
         if (ksn(4).eq.PLANE) goto 10

         if (itry_sf.eq.2 .or. itry_sf.eq.4) then
            d1 = sc(19)
            sc(19) = sc(20)
            sc(20) = d1
         endif

         ifl(2) = 0
         err = .false.
         goto 5
      endif
      
      call conv4_4 (tt(1,ia),t(1,3),9)
      
      call conv4_4 (t(1,ia),t(1,3),9)
c
c... move - stop at first sf edge found
c
15    continue
c
c... don't use wrapped surface
c
      noclos = .true.

      maxp = ifl(91)*12
      if (maxp .lt. 3000) maxp = 3000
      call uvmove (2,maxp,pte,pte,vx,0)
      if (ifl(2) .gt. 0) goto 997
c
c...move back to another sf edge
c
      if (ntk.ne.0) then
         call conv4_4 (t(1,ia),t(1,3),3)
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
      endif

      isav = itsk
      if (jtrim .ge. 1) isav = 3

      call uvmove (2,maxp,pte,pte,vx,isav)
      if (ifl(2) .gt. 0) goto 997

      if (ntk.le.0) then
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
         call uvmove (2,maxp,pte,pte,vx,isav)
         if (ifl(2) .gt. 0) goto 997
      endif
c
c..... trim curve to trimmmed surface boundaries
c
      np = ntk/3

      if (np .lt. 2) then
        if (.not.lv93 .and. itry_sf.eq.0) then
          call ncl_nul_uv
          itry_sf = 1
          asn = sc(11)
          goto 5
        endif
        ifl(2) = 378
        goto 998
      endif

      if (jtrim .ge. 1) then
        if (jtrim .eq. 2) then
          isav = 3
        else
          isav = itsk
        endif
c
c..... note that before a ncl_trim_cv call the list contains both XYZ
c..... (at even places) and UV (at odd places) points. the last
c..... ncl_trim_cv call deletes the unneeded half of the list
c
        call ncl_trim_cv (np,key11,isav,told,irpt,ptx)
        if (jtrim .eq. 2) then
          u = 0.5
          v = 0.5
          isrf = 2
          call sfinit(sc(12),isrf,u,v)
          do 17 k = 1, np
            call ncl_get_ent(2*k,pte)

            do 16 i=1,3
            s(i+7,isrf)=pte(i)
16          continue

            call surfpn(u,v,1)
            pte(1) = u
            pte(2) = v
            pte(3) = zero
            call ncl_replace_uv (2*k-1,pte)
17        continue
          call ncl_trim_cv (np,key12,itsk,told,irpt,ptx)
        endif
      endif
c
c...eliminate points too close together
c
      if (itsk .eq. 1) then
         call ncl_weed_uv (sc(27),np)
      else
         call ncl_weed_uv (1.d-3,np)
      endif
c
c...calculate distance from near point to first and last generated
c...points then decide which end is nearer the near point
c..... insert first and last vectors if not ssplin
c
      if (jtrim .ge. 1 .or. lredef) goto 9990
      call ncl_get_ent(ione,pte)
      d1 = f_dist (ptx,pte)
      call ncl_get_ent(np,pte)
      d2 = f_dist (ptx,pte)
      if (d1.gt.d2+sc(27)) then
        call ncl_revers_uv (pte,np)
        if (ivxfl .eq. 1) then
          call mnvc (vx(4))
          call ncl_insert_uv (vx(4),ione,ione)
          call mnvc (vx)
          call ncl_push_uv (vx)
        endif
      else if (ivxfl .eq. 1) then
        call ncl_insert_uv (vx,ione,ione)
        call ncl_push_uv (vx(4))
      endif
      goto 9990
c
c...error exit.
c
997   if (ifl(2).eq.385) then
        write(errcom,9010)s(5,1),s(6,1),s(7,1)
9010    format(3f10.3)
      endif
c
c... aak 08-jan-1998: give "Impossible geometry case" error
c
      ifl(2) = 163
      if (itry_sf.ge.4 .and. nsf.eq.2 .and. iprim1.ge.PLANAR) then
        ifl(2) = 502
        sc(10) = svsc10
        isc10(4) = iprim1
        sc(11) = svsc11
      endif

998   err=.true.

9990  continue
      noclos=.false.
      
      call rstpsis (svsc35)
      goto 99999
c
c..... set z-zero default plane as part surface
c
9995  asn=0.
      ksn(1)=6
      d(1)=asn
      d(3)=0.
      d(4)=0.
      d(5)=1.
      d(6)=0.
      sc(35)=svsc35

99999 if (ifl(2).le.0 .and. np.gt.0) then
        isc10(3) = np
        if (itsk.eq.1 .and. ivxfl.eq.1) ifl(212) = jtrim+1
      endif

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  rstpsis (svsc35)
c **                                                                  **
c **  purpose of subroutine: reset the Part Surface
c **
c **********************************************************************
c **********************************************************************

      subroutine rstpsis (svsc35)

      include 'com8a.com'
      include 'mocom.com'

      real*8 svsc35

      real*8 asn,svsc10,svsc11
      integer*2 nwds,itype,ifl2
      integer*4 nclkey
      logical lerr
      integer*2 ksn(4)
      equivalence (asn,ksn)

      call gtdesc(svsc35,nclkey,nwds,itype)
      if (nclkey .eq. 0) then
c
c..... set z-zero default plane as part surface
c
        asn = 0.
        ksn(1) = 6
        d(1) = asn
        d(3) = 0.
        d(4) = 0.
        d(5) = 1.
        d(6) = 0.
        sc(35) = svsc35
        return
      endif

      svsc10 = sc(10)
      svsc11 = sc(11)
      ifl2 = ifl(2)
      lerr = err
      sc(11) = svsc35
      asn = 0.
      ksn(1) = 713
      sc(10) = asn
      call motimm
c
c..... qar 96237 fix: reset psis if cannot recover it
c
      if (ifl(2).eq.122) then
        sc(35) = 0
        ifl(2) = ifl2
        err = lerr
      endif

      if (lerr) then
        ifl(2) = ifl2
        err = lerr
      endif

      sc(10) = svsc10
      sc(11) = svsc11
      
      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  sfsio
c **                                                                  **
c **  purpose of subroutine: create a spline as a horizontal contour
c **  around a collection of surfaces (maybe offset)
c **
c **********************************************************************
c **********************************************************************

      subroutine sfsio

      include 'com8a.com'
      include 'sfsio.com'

      common/keycom/keyold, keyhld, istold
      integer*4 keyold, keyhld
      integer*2 istold
      common/cviofl/cvion,iadvcs
      logical cvion
      integer*2 iadvcs

      real*8 tol,zbot,svsc35
      integer*2 nwds,itype,conpoc,iprim,mmod
      integer*2 ktv(4),krest(4)
      equivalence (tv,ktv),(rest,krest)
      integer*4 botkey,nclkey
      
      real*8 ptx(3),nvec(3)
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))

      integer*4 isub,isav
      character*8 cmname
      character*64 labsav
      character*6 nname
      integer*2 parslb,iret,itypsv,istsv,idstsv,ncv
      integer*2 i2v4 /4/

c      real*8 rtok
c      logical leq1
c      equivalence (token2,leq1),(rtok,leq1)

      itypsv = ityp
      istsv = ist
      idstsv = idst
      labsav = savid2
      isav = isvsub
      isub = 0
      svsc35 = sc(35)

      nvec(1) = 0
      nvec(2) = 0
      nvec(3) = 1
      if (ifl(72) .eq. 1) then
        call conent(nvec,sc(56),i2v4)
      endif
      
      if (ifl(296) .eq. 1) then
        defwf = .true.
        cmname = savid2(1:6)
        isub = isvsub
        nname = cmname(1:6)
        if (isub .eq. 0 .and.
     1     (isc10(2) .eq. 23 .or. isc10(2) .eq. 24 .or.isc10(4) .eq. 3))
     2        then
          iret = parslb(nname,isub)
          isvsub = isub
          ifl(296) = iret
        else
          ifl(296) = 0
        endif
      endif

      call gettol(tol)

      iprim = 3
      if (isc10(3) .gt. 0) then
        call gtdesc(sc(11),botkey,nwds,itype)
        if (itype .eq. SURF) then
          call ncl_get_sf_primtyp (botkey,iprim)
          if (iprim .ne. 3) then
           ifl(2) = 19
           goto 999
          endif
        endif
        mmod = isc10(4) ! direction modifier
        zbot = sc(12) ! offset distance
      else
        botkey = 0
        zbot = sc(11)
      endif

      ptx(1) = 0
      ptx(2) = 0
      ptx(3) = 0

      conpoc = 0
      if (isc10(2) .eq. 24) then
        conpoc = 1
      else if (isc10(2) .eq. 25) then
        conpoc = 2
        if (mmod .eq. 1) then
          call gtgeom(sc(13),ptx,nclkey,nwds,itype)
        else if (mmod .eq. 2) then
          ptx(1) = u1
          ptx(2) = v1
        endif
        cvion = .true.
      endif

      call sfsini
      if (iprim .eq. 3) then
        call nsfsio (botkey,zbot,nvec,ptx,mmod,tol,conpoc,
     x                nname,isub,ifl(2))
c      else
c        call sfsio1 (botkey,zbot,mmod,iprim,tol,conpoc,nname,isub)
      endif
      call sfsfre (ncv)
      cvion = .false.

      if (sclvar_def) then
c
c.... update the number of curves scalar
c
        token2 = sclvar_lab
        ivxsub = sclvar_inx
c        rtok = sc(199)
        call vstchk
        rest = tv + ncv
        idst = 2
        keyold = keyhld
        istold = ist
        ifl(9) = ifl(11)
        ifl(10) = ifl(12)
        call vstore
        sclvar_def = .false.
      endif

      if (ifl(2).gt.0) goto 990

      defwf = .true.
      ifl(329) = 1

      goto 999
c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2) = 5
c
c..... equivalence (lfl(29),err) in com.com
c
      err=.true.

c
c...The key list is freed by the error form
c...when running in interactive mode
c
999   if (ifl(35) .eq. 1) call delsky

      ifl(296) = 0
      idst = idstsv
      ityp = itypsv
      ist = istsv
      savid2 = labsav
      isvsub = isav
      
      call rstpsis (svsc35)
      
      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  cvouts
c **                                                                  **
c **  purpose of subroutine: create a spline as a horizontal contour
c **  around a collection of surfaces (maybe offset)
c **
c **********************************************************************
c **********************************************************************

      subroutine cvouts

      include 'com8a.com'

      integer*4 nclkey
      integer*2 nwds,ietype,jcomp
      integer*2 i2v1 /1/

      integer*4 isub, svinx
      character*64 cmname
      character*64 nname
      integer*2 parslb,iret,ierrno
      logical lb0set

      asn = sc(11)

        isub = 0
        cmname = savid2
        svinx = isvsub
        lb0set = (ifl(296) .eq. 0)

        if (ifl(296) .eq. 1) then
          defwf = .true.
          isub = isvsub
          nname = cmname
          if (isub .eq. 0) then
            iret = parslb(nname,isub)
            isvsub = isub
            ifl(296) = iret
          else
            ifl(296) = 0
          endif
        endif

      call gtdesc(asn, nclkey, nwds, ietype)
      call iscmpc (nclkey, jcomp)
      if (jcomp .eq. 0) goto 990
      call gtccnm (nclkey, nwds)
      ncv =  isc10(3)

      if (ncv .eq. 0) then
        do j = 1,nwds
          call cvoutj (nclkey,j,nname,isub,ierrno)
          ifl(2) = ierrno
          if (ifl(2) .gt. 0) goto 990
          if (ifl(296) .gt. 0) ifl(296) = ifl(296)+1
        enddo
      else if (ncv .eq. 1) then
        j = isc10(4)
        if (j .gt. nwds) j = nwds
        if (j .lt. 1) j = 1
        call cvoutj (nclkey,j,nname,isub,ierrno)
        ifl(2) = ierrno
        if (ifl(2) .gt. 0) goto 990
      else

        do i = 1,ncv
          call getskj (i,j)
          call cvoutj (nclkey,j,nname,isub,ierrno)
          ifl(2) = ierrno
          if (ifl(2) .gt. 0) goto 990
          if (ifl(296) .gt. 0) ifl(296) = ifl(296)+1
        enddo
      endif

      if (lb0set) then
          savid2 = cmname
          isvsub = svinx
      endif
      defwf = .true.
      ifl(329) = 1

      goto 999
c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2) = 5
c
c..... equivalence (lfl(29),err) in com.com
c
      err=.true.

999   call delsky
      ifl(296) = 0

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  cvcntr
c **                                                                  **
c **  purpose of subroutine: create a spline as a horizontal contour
c **  around a collection of surfaces (maybe offset)
c **
c **********************************************************************
c **********************************************************************

      subroutine cvcntr

      include 'com8a.com'
      include 'wrksys.com'

      real*8 d,z,tol,co,f_dot
      real*8 pll(4),nvec(3),pt0(3)

      integer*4 nclkey
      integer*4 ncontr
      integer*2 izro /0/, i2v1 /1/, i2v3 /3/, i2v4 /4/

      integer*2 nwds,itype,iprim,k
      logical*2 lwrksv

      d = sc(11)

      tol = sc(27)
      if (ifl(264).eq.1) tol = tol/25.4d0
      defwf = .true.

      nvec(1) = 0
      nvec(2) = 0
      nvec(3) = 1
      if (isc10(3) .eq. 0) then
        pt0(1) = 0
        pt0(2) = 0
        pt0(3) = sc(12)
      endif

      if (ifl(72) .eq. 1) then
        call conent(nvec,sc(56),i2v4)
        if (isc10(3) .eq. 0) call conent(pt0,sc(56),i2v3)
      endif

      lwrksv = lwrk
      if (lwrk) then
        call conent(nvec,wrkmx,i2v4)
        if (isc10(3) .eq. 0) call conent(pt0,wrkmx,i2v3)
        lwrk = .false.
      endif

      if (isc10(3) .gt. 0) then
        if (isc10(3) .eq. 2) then
          call gtdesc(sc(12),nclkey,nwds,itype)
          call ncl_get_sf_primtyp (nclkey,iprim)
          if (iprim .ne. PLANAR) then
            ifl(2) = 19
            goto 990
          endif
        endif
        call gtplt (sc(12),izro,pll)
        co = f_dot(pll,nvec)
        if (co .lt. 0) then
          do k = 1,3
            nvec(k) = -pll(k)
          enddo
          z = -pll(4)
        else
          do k = 1,3
            nvec(k) = pll(k)
          enddo
          z = pll(4)
        endif
      else
        z = f_dot(pt0,nvec)
      endif

      nclkey = ncontr(d,z,nvec,tol,ifl(2))

      if (nclkey.le.0 .or. ifl(2).gt.0) goto 990

      call ptdesc (nclkey,CURVE,tv)
      rest = tv
      if (.not. dsplcv) call blkgeo (nclkey, i2v1)

      call crvcls (nclkey)

      goto 999
c
c..... error exit.
c
990   if (ifl(2).lt.1) ifl(2) = 5
c
c..... equivalence (lfl(29),err) in com.com
c

999   lwrk = lwrksv
      err = ifl(2) .ne. 0

      return
      end

c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c.....stuff below is not active
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c **********************************************************************
c **********************************************************************
c **  subroutine name:  sfsio2
c **                                                                  **
c **  purpose of subroutine: create a spline as a horizontal contour
c **  around a collection of surfaces (maybe offset)
c **
c **********************************************************************
c **********************************************************************

      subroutine sfsio2(key11,ltrim1,u10,v10,key12,ltrim2,u20,v20,
     x                                      tol,conpoc,nname,isub)

      include 'com8a.com'
      include 'mocom.com'
      integer*4 key11,key12
      real*4 u10,v10,u20,v20
      real*8 tol
      integer*2 conpoc
      integer*4 isub
      logical ltrim1,ltrim2
      character*6 nname

      integer*2 i
      real*4 u1,v1,u2,v2,uv(4)
      equivalence(sc(19),uv),(u1,uv(1)),(v1,uv(2)),(u2,uv(3)),(v2,uv(4))
c
c... motion common equivalences
c
      real*4 ad(300)
      equivalence (d,ad)

      integer*2 ksn(4),itry_sf,isav
      equivalence (asn,ksn)

      real*8 ptx(12),pte(3),vx(6),
     *       asn,svsc11,d1

      integer*2 isrf,ia,ntk

      equivalence (isrf,ifl(54)),(ia,ifl(51)),(ntk,ifl(79))


        u1 = u10
        v1 = v10
        u2 = u20
        v2 = v20

        svsc11 = sc(11)
        itry_sf = 0
        call frstpt (key11,key12,pte,vx,ifl(2))
        if (ifl(2) .eq. 0) then
          asn = sc(11)
          u = u1
          v = v1
          call srfevl(asn,u,v,ptx)
          err = .false.
          sc(11) = sc(12)
          do i = 1,3
            t(i,3) = pte(i)
            t(i+3,3) = 0.
            t(i+6,3) = vx(i)
          enddo
          call uvprmv
          if (ifl(2) .gt. 0) goto 997
          goto 15
        else
          ifl(2) = 0
        endif

4        itry_sf = 1
         asn = sc(11)
         ptx(1) = 0
         ptx(2) = 0
         ptx(3) = 0

5     continue
      u = u1
      v = v1
      call srfevl(asn,u,v,ptx)
      ifl(2) = 0
      err = .false.

9     call conv8_4 (ptx,t(1,3),3)
c
c...set up to move
c
      sc(11) = sc(12)
      call uvprmv
      if (ifl(2) .gt. 0) goto 997
c
c... move to point on both surfaces
c
      maxp = 50
      isav  = 0
      call uvmove (1,maxp,pte,pte,vx,isav)
c
c..... try center of the 1st srf as nearpt, then try center of the 2nd srf
c
      if (ifl(2) .ge. 1) then
10       if (itry_sf .ge. 2) then
           ifl(2) = 0
           return
         endif
         itry_sf = itry_sf + 1
         asn = sc(12)

         d1 = sc(19)
         sc(19) = sc(20)
         sc(20) = d1

         ifl(2) = 0
         err = .false.
         goto 5
      endif
      call conv4_4 (t(1,ia),t(1,3),9)
c
c... move - stop at first sf edge found
c
15    continue

      maxp = ifl(91)*12
      if (maxp .lt. 3000) maxp = 3000
      call uvmove (2,maxp,pte,pte,vx,0)
      if (ifl(2) .gt. 0) goto 997
c
c...move back to another sf edge
c
      if (ntk.ne.0) then
         call conv4_4 (t(1,ia),t(1,3),3)
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
      endif

      isav = 3

      call uvmove (2,maxp,pte,pte,vx,isav)
      if (ifl(2) .gt. 0) goto 997

      if (ntk.le.0) then
         call mnvc4 (t(7,3))
         ad(102)=-ad(102)
         call uvmove (2,maxp,pte,pte,vx,isav)
         if (ifl(2) .gt. 0) goto 997
      endif
c
c..... trim curve to trimmmed surface boundaries
c
      np = ntk/3

      if (np .lt. 2) then

        if (ifl(2).eq.0 .and. itry_sf.eq.0) then
          call ncl_nul_uv
          goto 4
        endif

        ifl(2) = 378
        goto 998
      endif

c
c..... note that before a ncl_trim_cv call the list contains both XYZ
c..... (at even places) and UV (at odd places) points. the last
c..... ncl_trim_cv call deletes the unneeded half of the list
c
        irpt = 0
        call ncl_trim_cv (np,key11,isav,tol,irpt,ptx)
        if (ltrim2) then
          u = u20
          v = v20
          isrf = 2
          call sfinit(sc(12),isrf,u,v)
          do 17 k = 1, np
            call ncl_get_ent(2*k,pte)

            do 16 i=1,3
            s(i+7,isrf)=pte(i)
16          continue

            call surfpn(u,v,1)
            pte(1) = u
            pte(2) = v
            pte(3) = 0
            call ncl_replace_uv (2*k-1,pte)
17        continue
          call ncl_trim_cv (np,key12,isav,tol,irpt,ptx)
        endif

      if (np .lt. 2) then
        ifl(2) = 378
        goto 998
      endif

      if (conpoc .eq. 0) then
        call sfscre (tol,nname,isub,ifl(2))
      else
        call nsfsi1 (tol,ifl(2)) ! store intersections into lists
      endif
      goto 9990
c
c...error exit.
c
997   if (ifl(2).eq.385) then
        write (errcom,9010) s(5,1),s(6,1),s(7,1)
9010    format (3f10.3)
      endif
c
c... aak 08-jan-1998: give "Impossible geometry case" error
c
      ifl(2) = 163
998   err=.true.

9990  continue

      return
      end

c **********************************************************************
c **********************************************************************
c **  subroutine name:  sfsio1
c **                                                                  **
c **  purpose of subroutine: create a spline as a horizontal contour
c **  around a collection of surfaces (maybe offset)
c **
c **      INPUT  :
c **         ityp2      primitive type
c **         key12      surface key
c **
c **********************************************************************
c **********************************************************************

      subroutine sfsio1 (key12,offd,mmod,iprim2,tol,conpoc,nname,isub)

      include 'com8a.com'
      include 'mocom.com'
      integer*4 key12
      real*8 offd,tol
      integer*2 mmod,iprim2,conpoc
      integer*4 isub
      character*6 nname

      integer*2 isf,itype,iprim1,joff,ifl2
      integer*4 key11
      real*4 u1,v1,u2,v2
      real*8 umin,umax,vmin,vmax,asn,svsc35,uu,vv,pvs(9),vmod(3)
      logical ltrim1,ltrim2,lerr

      integer*2 ksn(4)
      equivalence (asn,ksn)

        ifl(332) = 0

        call sftype (key12,itype)
        ltrim2 = (itype.eq.TRIMSF)
        if (ltrim2) then
          ifl(332) = 1
          call tbdini (key12,2,tol,ifl(332))
          if (ifl(2).eq.466) return
          call gtmmuv(2,umin,vmin,umax,vmax)
          u2 = (umin + umax)/2
          v2 = (vmin + vmax)/2
        else
          u2 = 0.5
          v2 = 0.5
        endif

        call ptdesc(key12,SURF,sc(12))

        joff = 0
        if (mmod.ge.638 .and.mmod.le.643 .and. offd.ne.0) then
          call isitwf (key12,joff)
          if (joff .eq. 1) then
            isf = 3
            uu = u2
            vv = v2
            call evstup (key12,isf)
            call uevsft (uu,vv,isf,pvs,ifl(2))
            if (ifl(2) .gt. 0) goto 9990
            call f_cross (pvs(4),pvs(7),pvs(1))
            vmod(1) = 0
            vmod(2) = 0
            vmod(3) = 0
            if (mmod .eq. 638) then
              vmod(1) = 1
            else if (mmod .eq. 639) then
              vmod(2) = 1
            else if (mmod .eq. 640) then
              vmod(3) = 1
            else if (mmod .eq. 641) then
              vmod(1) = -1
            else if (mmod .eq. 642) then
              vmod(2) = -1
            else
              vmod(3) = -1
            endif
            if (vmod(1)*pvs(1)+vmod(2)*pvs(2)+vmod(3)*pvs(3) .lt. 0)
     x        offd = -offd
            call offwf1 (key12,offd)
          endif
        endif

        noclos = .true.
        svsc35 = sc(35)

        isf = 0
30      isf = isf+1
        call gtsky (isf,key11)
        if (key11 .le. 0) goto 9990
        call ncl_get_sf_primtyp (key11,iprim1)
c
c..... maybe will use primitive types later
c
        ifl(331) = 0
        call sftype (key11,itype)
        ltrim1 = (itype.eq.TRIMSF)
        if (ltrim1) then
          ifl(331) = 1
          call tbdini (key11,1,tol,ifl(331))
          if (ifl(2).eq.466) goto 9990
          call gtmmuv(1,umin,vmin,umax,vmax)
          u1 = (umin + umax)/2
          v1 = (vmin + vmax)/2
        else
          u1 = 0.5
          v1 = 0.5
        endif

        call ptdesc(key11,SURF,sc(11))
        asn = 0
        ksn(1) = 713
        sc(10) = asn
        ifl(46) = 1
        call motimm
        ifl(46) = 0
        if (ifl(2) .gt. 0) goto 9990

        call sfsio2 (key11,ltrim1,u1,v1,key12,ltrim2,u2,v2,tol,conpoc,
     x                                                     nname,isub)
        if (ifl(2) .eq. 0) goto 30

9990  continue
c
c..... connect intersections into contours
c
      if (ifl(2).eq.0 .and. conpoc.eq.1) call nsfsi2 (tol,ifl(2))

      noclos=.false.
      if (joff .eq. 1) call offwf1 (key12,-offd)
c
c..... restore the part surface in effect prior to this cv statement
c
      call gtdesc(svsc35,nclkey,nwds,itype)
      if (nclkey .eq. 0) goto 9995

      ifl2 = ifl(2)
      lerr = err
      sc(11) = svsc35
      asn = 0
      ksn(1) = 713
      sc(10) = asn
      call motimm
      if (lerr) then
        ifl(2) = ifl2
        err = lerr
      endif
      return
c
c..... set z-zero default plane as part surface
c
9995  asn = 0
      ksn(1) = 6
      d(1) = asn
      d(3) = 0.
      d(4) = 0.
      d(5) = 1.
      d(6) = 0.
      sc(35) = svsc35

      return
      end
