C*********************************************************************
C*    NAME         :  revpre.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       revpre.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:37
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine revpre
C*       Define a surface of revoulution.
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
      subroutine revpre

      include 'com.com'
      include 'const.com'

      common/keycom/keyold
      integer*4 keyold

      integer*2 i,nwds,ietype,cvtype,ifull,iclsd,itsk,ietyp2
      integer*4 nclkey, keycv, keypt, keyve
      real*8 sa, ea, buf(16),ptn(3),vec(3),r1,r2,vtmp(3),f_dot
c      equivalence (buf(4),vec)
      integer*2 ktv(4)
      equivalence (tv,ktv)
      logical lv91

   
      itsk = isc10(3)

      if (itsk.eq.1) then

        ifull = 2
        keycv = 0
        call gtdesc (sc(11), nclkey, nwds, ietype)
        call gtgeo (nclkey, buf)
        do i = 1,3
          ptn(i) = buf(i)
          vec(i) = buf(i+3)
        enddo
        if (ietype .eq. CIRCLE) then
          r1 = buf(7)
        else
          if (ietype .eq. POINT) then
            vec(1) = 0
            vec(2) = 0
            vec(3) = 1
          endif
          if (isc10(4) .eq. 1) then
            r1 = sc(12)
          else
            call gtdesc (sc(12), nclkey, nwds, ietyp2)
            if (ietyp2.ne.POINT .and. ietyp2.ne.PNTVEC) goto 51
            call gtgeo (nclkey, buf)
            do i = 1,3
              vtmp(i) = buf(i) - ptn(i)
            enddo
            call unitvc(vec,vec)
            r2 = f_dot(vtmp,vec)
            do i = 1,3
              ptn(i) = ptn(i) + r2*vec(i)
              vtmp(i) = buf(i) - ptn(i)
            enddo
            r1 = f_dot(vtmp,vtmp)
            if (r1 .gt. 0) then
              r1 = dsqrt(r1)
            else
              goto 51
            endif
          endif
        endif
        r2 = r1
        goto 10
      else if (itsk.eq.2 .or. itsk.eq.3) then

        ifull = 1
        keycv = 0
        r1 = sc(12)
        r2 = r1

        call gtdesc (sc(11), nclkey, nwds, ietype)
        if (ietype .eq. POINT) then
          call gtgeo (nclkey, ptn)
          vec(1) = 0
          vec(2) = 0
          vec(3) = 0
        else if (ietype .eq. LINE .or. ietype.eq.PNTVEC) then
          call gtgeo (nclkey, buf)
          do i = 1,3
            ptn(i) = buf(i)
            vec(i) = buf(i+3)
          enddo
          if (itsk.eq.3) r2 = sc(13)
        else
          goto 51
        endif

        goto 10

      endif

      call gtdesc (sc(11), keycv, nwds, cvtype)
      call gtdesc (sc(12), keypt, nwds, ietype)
c
c...Get point of revolution and vector when 
c...point is specified
c
      if (ietype .eq. POINT) then
          call gtgeo (keypt, ptn)
          call gtdesc (sc(13), keyve, nwds, ietype)
          if (ietype .eq. 21) then
              call gtgeo (keyve, buf)
              vec(1) = buf(4)
              vec(2) = buf(5)
              vec(3) = buf(6)
          else
              call gtgeo (keyve, vec)
          endif
c
c...PV or LN specified, use both point & vector
c
      else
          call gtgeo (keypt, buf)
          ptn(1) = buf(1)
          ptn(2) = buf(2)
          ptn(3) = buf(3)
          vec(1) = buf(4)
          vec(2) = buf(5)
          vec(3) = buf(6)
      endif

      sa = sc(14)
      ea = sc(15)

      if (dabs (ea-sa) .lt. 0.033) goto 51

      do while (sa .lt. 0.) 
        sa = sa + 360.
      enddo

      do while (sa .ge. 360.) 
        sa = sa - 360.
      enddo

      do while (ea .le. sa) 
        ea = ea + 360.
      enddo

      do while ((ea - sa) .gt. 360.) 
        ea = ea - 360.
      enddo

      if (cvtype .eq. CURVE) call crvcls (keycv)
      ifull = 0
      if ((ea - sa) .eq. 360.) ifull = 1

10    nclkey = keyold
c
c..... defwf must be true for correct labelling
c
      defwf = .true.

      lv91 = sc(169).lt.9.149d0
c
c..... earlier versions create a spline surfaces, later ones create  
c..... NCL surfaces of revolution.
c
      if (lv91) then
        sa = sa/RADIAN
        ea = ea/RADIAN
        call revdef(keycv, ptn, vec, sa, ea, nclkey)
      else
        if (keycv .eq. 0) then
          call revsf(keycv, ptn, vec, r1, r2, ifull, nclkey)
        else
          call revsf(keycv, ptn, vec, sa, ea, ifull, nclkey)
        endif
c
c..... defwf is reset to activate vstore from driver
c
        defwf = .false.
      endif

      if (nclkey .eq. 0) goto 51

      ietype = 9
      call ptdesc (nclkey, ietype, tv)
      ktv(3) = 1

      if (isc10(1) .eq. 607 .and. keycv.gt.0) then
        call gtclsd (keycv, 0, iclsd)
        if (iclsd .eq. 1) then
          iside = 0
          call ptclsd (nclkey, iside, iclsd)
        endif
      else
        call srfcls (nclkey)
      endif

      rest = tv
      goto 99

51      ifl(2) = 51
        err = .true.

99    return
      end
