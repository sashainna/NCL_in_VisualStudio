C*********************************************************************
c*    NAME         :  yfmotion.f
c*       CONTAINS:
c*
c*				yfgoto
c*				yffrom
c*				yfgoxx
c*				yfgosf
c*				yfdrcv
c*				yfgoauto
c*
c*    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
c*              All Rights Reserved.
c*    MODULE NAME AND RELEASE LEVEL
c*       yfmotion.f , 25.1
c*    DATE AND TIME OF LAST MODIFICATION
c*       04/29/15 , 15:10:57
c*********************************************************************
c
c*********************************************************************
c*    E_FUNCTION     : yfgoto(pt,ierr)
c*       This function processes a GOTO command.
c*    PARAMETERS
c*    INPUT  :
c*       pt           Coordinates and vector of GOTO point.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c*********************************************************************
c
      subroutine yfgoto(pt,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr
      real*8 pt(6)
c
      integer*2 ksn(4)
      real*8 asn
      equivalence (asn,ksn)
c
c...Set up goto point
c
      call conv8_8(pt,sc(11),6)
c
c...Output motion
c
      nline = nline + 1
      ifl(270) = -1
      ifl(130) = 0
      ksn(1)   = GOTO
      ksn(2)   = 0
      ksn(3)   = 6
      sc(10)   = asn
      ifl(2)   = 0
      call motimm
      ierr = ifl(2)
c
c...End of routine
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yffrom (pt,ierr)
c*       This function processes a FROM command.
c*    PARAMETERS
c*    INPUT  :
c*       pt           Coordinates and vector of FROM point.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yffrom (pt,ierr)
c
      include 'com8a.com'
c
      integer*4 ierr
      real*8 pt(6),f_mag
c
c...Perform FROM
c
      if (f_mag(pt(4)) .eq. 0.0d0) then
        ierr = 123
      else
        call vctovc(pt,sc)
        call unitvc(pt(4),sc(4))
        ierr  = 0
      endif
c
c...End of routine
c
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfgoxx(ps,ds,cs,nearpt,eval,wchk,ierr)
c*       This function processes the GOFWD,GOLFT,GORGT commands.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Part surface parameters.
c*       ds           Drive surface parameters.
c*       cs           Check surface(s) parameters.
c*       nearpt       Optional near point.
c*       eval         Initial evaluations for PS,DS,CS.
c*    OUTPUT :
c*       eval         Final evaluations for PS,DS,CS.
c*       wchk         Check surface stopped at for multi-check.
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfgoxx(ps,ds,cs,nearpt,eval,wchk,ierr)
c
      include 'com8a.com'
      include 'suvcom.com'
c
      integer*4 ps(4),ds(4),cs(4),nearpt(8),wchk,ierr
      real*8 eval(6)
c
      common/cptcom/chkpt
      real*8 chkpt(5)

      common/hptcom/hldctp
      real*8 hldctp(27)

      common/avdcom/avflgs
      logical avflgs(5)
c
      integer*2 i,ix,ix2,nwds,ietype,iflg
      integer*4 nn

      integer*2 ksn(4)
      integer*4 jsn(2)
      real*8 asn
c
      equivalence (asn,ksn,jsn)

      integer*2 iclass, isubcl
c
c...Initialize routine
c
      ifl(86) = 0
c
c... NCLX_mdl_composite_curve curve on DS is always driven as a single PS
c
      lfl(104) = .false.
c
c...Set PS, DS, & CS
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(11) = asn
c
c... PSIS/...
c
      ksn(1) = 713
c
c.....check if PS is a curve (on DS)
c
      ksn(2) = 0
      if (ps(2) .eq. CURVE) ksn(2) = 1

      ksn(3) = 1
      sc(10) = asn
      ifl(2) = 0
      call motimm
      if (ifl(2) .ne. 0) go to 8000
c
      ksn(1) = ds(4)
      ksn(2) = cs(1)
      ksn(3) = 0
      ksn(4) = 0
      sc(10) = asn
      jsn(1) = ds(1)
      ksn(4) = ds(2)
      sc(11) = asn
      ifl(219) = ds(3)
      ifl(21)  = ds(3)
c
      asn  = 0.0d0
      nwds = 3
      ietype = POINT
      ix  = 2
      ix2 = 12

      do 100 i=1,cs(1)
        ksn(1) = cs(ix+2)
        ksn(2) = cs(ix+3)
        sc(ix2) = asn
        jsn(1) = cs(ix)
        ksn(4) = cs(ix+1)
        sc(ix2+1) = asn

        if (nearpt(i) .eq. 0) then
          chkpt(i) = 0.0d0
        else
          call ptdsc3(nearpt(i),nwds,ietype,chkpt(i))
        endif

        csflgs(i) = .true.
        mcsu(i)   = eval(2*i+3)
        mcsv(i)   = eval(2*i+4)
        avflgs(i) = cs(ix+4).eq.1
        ix  = ix+5
        ix2 = ix2+2
100   continue
c
c...   Store last curve/surface evaluations
c
      if (ps(2) .eq. SURF) then
          psuv = .true.
          psu  = eval(1)
          psv  = eval(2)
      endif
c
      if (ds(2) .eq. CURVE .or. ds(2) .eq. SURF) then
          dsuv = .true.
          dsu  = eval(3)
          dsv  = eval(4)
      endif
c
c...Output motion
c
      nline = nline + 1
      wchk     = -1
      ifl(270) = -1
      ifl(130) = 0

      if (ifl(23).eq.8.or.ifl(23).eq.9) csflgs(1) = .true.

      call motgxx
      if (ifl(2).lt.1) wchk = ifl(328)-1
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                if (ifl(104).eq.1) call modfy (hldctp,hldctp)
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif

          eval(1) = hu(1)
          eval(2) = hv(1)
          eval(3) = hu(2)
          eval(4) = hv(2)
          if (wchk.lt.0) wchk = 0
          ix = wchk*2
          eval(ix+5) = hu(3)
          eval(ix+6) = hv(3)
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfgosf(nsrfs,ps,ds,cs,eval,ierr)
c*       This function processes the GO command.
c*    PARAMETERS
c*    INPUT  :
c*       nsrfs        Number of surfaces to GO to (1-3).
c*       ps           Part surface parameters.
c*       ds           Drive surface parameters.
c*       cs           Check surface(s) parameters.
c*       eval         Initial evaluations for PS,DS,CS.
c*    OUTPUT :
c*       eval         Final evaluations for PS,DS,CS.
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfgosf(nsrfs,ps,ds,cs,eval,ierr)
c
      include 'com8a.com'
      include 'suvcom.com'
c
      integer*4 nsrfs,ps(4),ds(4),cs(4),ierr
      real*8 eval(6)
c
      integer*2 ksn(4),ifl35
      integer*4 jsn(2)
      real*8 asn
c
      equivalence (asn,ksn,jsn)
c
      ifl(2) = 0
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Set PS, DS, & CS
c
      ksn(1) = GO
c
c.....check if PS is a curve (on DS)
c
      ksn(2) = 0
      if (ps(2) .eq. CURVE) ksn(2) = 1

      ksn(3) = nsrfs
      ksn(4) = 0
      sc(10) = asn

      sc(11) = ds(3)
      jsn(1) = ds(1)
      ksn(4) = ds(2)
      sc(12) = asn
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(13) = asn

      sc(14) = cs(3)
      jsn(1) = cs(1)
      ksn(4) = cs(2)
      sc(15) = asn
c
c...Store last surface evaluations
c
      if (ps(2) .eq. SURF) then
          psuv = .true.
          psu  = eval(1)
          psv  = eval(2)
      endif
c
      if (ds(2) .eq. CURVE .or. ds(2).eq.SURF) then
          dsuv = .true.
          dsu  = eval(3)
          dsv  = eval(4)
      endif
c
      if (cs(2) .eq. CURVE .or. cs(2).eq.SURF) then
          csuv = .true.
          csu  = eval(5)
          csv  = eval(6)
      endif
c
c...Perform motion
c...Call mocntl directly to avoid duplicate code
c...Jingrong  -  11/30/99
c
      nline = nline + 1
      ifl35 = ifl(35)
      ifl(35) = 1
      call mocntl
      ifl(35) = ifl35

      if (ifl(2) .gt. 0) goto 8000
      eval(1) = hu(1)
      eval(2) = hv(1)
      eval(3) = hu(2)
      eval(4) = hv(2)
      eval(5) = hu(3)
      eval(6) = hv(3)
c
c...End of routine
c
 8000 ierr   = ifl(2)
      ifl(276) = 0
      ifl(280) = 0
      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfdrcv (ds,ierr)
c*       This function processes the GOFWD/curve-as-ps-ds command.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Drive/Part surface curve.
c*    OUTPUT :
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfdrcv (ds,ierr)

      include 'com.com'

      integer*4 ds(4), ierr
c
      integer*4 nn
      integer*2 nwds, ietype, iflg
      integer*2 iclass, isubcl
      data iclass /5200/, isubcl /5/
c
c...Initialize routine
c
      ifl(86) = 0
c
c...Call motion routine
c
      nline = nline + 1
      sc(10)   = 0.0d0
      isc10(1) = GOFWD
      nwds     = 1
      ietype   = ds(2)
      call ptdsc3(ds(1), nwds, ietype, sc(11))
      ifl(21)  = ds(3)
      ifl(276) = 1
      call motgxx
c
c...Store cldata
c
      if (ifl(2) .eq. 0) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) call csdist (iclass, isubcl)
              call ptclos
          endif
      endif

      ierr = ifl(2)

      return
      end
c
c*********************************************************************
c*    E_FUNCTION     : yfgoauto(ps,ds,cs,nearpt,eval,wchk,ierr)
c*       This function processes the AutoGofwd commands.
c*    PARAMETERS
c*    INPUT  :
c*       ps           Part surface parameters.
c*       ds           Drive surface attributes.
c*       cs           Check surface(s) parameters.
c*       nearpt       Optional near point.
c*       eval         Initial evaluations for PS,DS,CS.
c*    OUTPUT :
c*       eval         Final evaluations for PS,DS,CS.
c*       ierr         Return status, non-zero on error.
c*    RETURNS      :
c*       UU_SUCCESS iff no error; else UU_FAILURE
c*    SIDE EFFECTS : none
c*    WARNINGS     : none
c********************************************************************/
c
      subroutine yfgoauto(ps,ds,cs,nearpt,eval,ncs,wchk,ierr)
c
      include 'com8a.com'
      include 'suvcom.com'
c
      integer*4 ps(4),ds(8),cs(2520),nearpt(504),wchk,ierr,ncs
      real*8 eval(1012)
c
      common/cptcom/chkpt
      real*8 chkpt(5)

      common/hptcom/hldctp
      real*8 hldctp(27)

      common/avdcom/avflgs
      logical avflgs(5)

      common/autofwd/scauto,csauto,avauto,chkauto,muauto,mvauto,
     *               nlkahd,lcheck,nlstcs
      common/flcspl/lcspl
      real*8 scauto(1008),chkauto(504)
      real*4 muauto(504),mvauto(504)
      integer*2 nlkahd(504),nlstcs,nds,nlook
      logical avauto(504),csauto(504),lcheck,lcspl
c
      logical AutoGofwd
      equivalence (ifl(353),AutoGofwd)

      integer*2 i,ix,nwds,ietype,nents,isvix,ichk,isf, iflg
      integer*4 nn,nsf,nclkey,keyent
      real*8 buf(14),f_dist,f_dot,d1,dhld,htv,plend(6),u8

      integer*2 ksn(4)
      integer*4 jsn(2)
      real*8 asn

      equivalence (asn,ksn,jsn), (sc,asc)

      integer*2 iclass, isubcl
c
c...Initialize routine
c
      ifl(86) = 0
c
c... NCLX_mdl_composite_curve curve on DS is always driven as a single PS
c
      lfl(104) = .false.
c
c... Set auto gofwd flag.
c
      AutoGofwd = .true.
      lcheck = .true.
      lcspl = .false.
      nlstcs = ncs
      nds = ds(2)
      lautgf = .true.
c
c...Set PS, DS, & CS
c
      jsn(1) = ps(1)
      ksn(4) = ps(2)
      sc(11) = asn
c
c... PSIS/...
c
      ksn(1) = 713
c
c.....check if PS is a curve (on DS)
c
      ksn(2) = 0
      if (ps(2) .eq. CURVE) ksn(2) = 1

      ksn(3) = 1
      sc(10) = asn
      ifl(2) = 0
      call motimm
      if (ifl(2) .ne. 0) go to 8000
c
c... Store first DS information
c
      ksn(1) = GOFWD
      ksn(2) = ds(2)
      ksn(3) = 0
      ksn(4) = 0
      sc(10) = asn
      jsn(1) = ds(4)
      ksn(4) = ds(5)
      sc(11) = asn
      ifl(219) = ds(1)
      ifl(21) = ds(1)

      if (ds(3) .eq. 0 .and. ds(5).eq.CURVE) then
        lcheck = .false.
        nlook = 1
        nclkey = ds(4)
        call gtdesc(tv,nclkey,nwds,ietype)
        call gtccnm (nclkey, nents)
        if (nents.gt.1) then
          do i=1,nents
            call gtcent (nclkey, i, keyent, buf, ietype,ierr)
            if (keyent.lt.0) keyent = -keyent
            call cvallpv(keyent, ietype, sc, buf)
            d1 = f_dist(sc, buf)
            if (i.eq.1 .or. d1.lt.dhld) then
              dhld  = d1
              isvix = i
              call conv8_8(buf,plend,6)
            endif
          enddo
          call gtcent (nclkey, isvix, keyent, buf, ietype,ierr)
          if (ierr.eq.0) then
            if (keyent.lt.0) keyent = -keyent
            call ptdsc3(keyent,1,ietype,sc(11))
            htv = tv
            ichk=0
            do i=isvix+1,nents
              call gtcent (nclkey, i, keyent, buf, ietype,ierr)
              if (ierr .eq. 0) then
                ichk = ichk+1
                if (keyent.lt.0) keyent = -keyent
                call ptdsc3(keyent,1,ietype,htv)
                scauto(ichk*2) = htv
                asn = 0.d0
                ksn(2) = 1
                scauto(ichk*2-1) = asn
                csauto(ichk) = .false.
                nlkahd(ichk) = nlook
                avauto(ichk) = .false.
                if (ichk.le. 5)  then
                  sc(10+ichk*2) = asn
                  sc(11+ichk*2) = htv
                  csflgs(ichk) = .false.
                endif
              endif
            enddo
            isc10(2) = ichk

            cscond = CS_PAST
            isf = 3
            call evstup (nclkey, isf)
            u8 = 0.
            call uevcvt (u8,isf,ifl(72),buf(4),buf,ifl(2))
            u8 = 1.
            call uevcvt (u8,isf,ifl(72),buf(7),buf,ifl(2))
            if (f_dist(buf(4),buf(7)) .lt. sc(27)) then
              d1 = f_dist(buf(4),plend)
              if (d1.gt.sc(27)*10.d0) then
                do i=1,isvix
                  call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                  if (ierr.eq.0) then
                    if (keyent.lt.0) keyent = -keyent
                    call ptdsc3(keyent,1,ietype,htv)
                    ichk = ichk+1
                    scauto(ichk*2) = htv
                    asn = 0.d0
                    ksn(2) = 1
                    scauto(ichk*2-1) = asn
                    csauto(ichk) = .false.
                    nlkahd(ichk) = nlook
                    avauto(ichk) = .false.
                    if (ichk.le. 5)  then
                      sc(10+ichk*2) = asn
                      sc(11+ichk*2) = htv
                      csflgs(ichk) = .false.
                    endif
                  endif
                enddo
                call unitvc(plend(4),buf)
                buf(4)  = f_dot(buf,plend)
                cscond = CS_ON
              else
                i = 1
                call gtcent (nclkey, i, keyent, buf, ietype,ierr)
                if (ierr.eq.0) then
                  if (keyent.lt.0) keyent = -keyent
                  call ptdsc3(keyent,1,ietype,htv)
                endif
              endif
            else
              call unitvc(buf,buf)
              buf(4)  = f_dot(buf,buf(7))
              call uycspl(keyent,buf)
              lcspl = .true.
              nwds = 4
              call ptdsc3(keyent,nwds,PLANE,htv)
            endif
            ichk = ichk+1
            isc10(2) = ichk
            scauto(ichk*2) = htv
            asn = 0.d0
            ksn(1) = cscond
            ksn(2) = 1
            scauto(ichk*2-1) = asn
            csauto(ichk) = .false.
            nlkahd(ichk) = 1
            avauto(ichk) = .false.
            if (ichk.le. 5)  then
              sc(10+ichk*2) = asn
              sc(11+ichk*2) = htv
              csflgs(ichk) = .false.
            endif
            j = ichk
            do i=1,ichk-1
              j = j-1
              if (nlkahd(i).gt.j) nlkahd(i) = j
            enddo
          endif
          nlstcs = 1
          nlkahd(isc10(2)) = nlstcs
        endif
      endif

c
c... Store CS information
c
      asn = 0.0d0
      nwds = 3
      ietype = POINT
      ix = 1

      nsf = ds(2)+ncs-1
      do 100 i=1, nsf 
        ksn(1) = cs(ix+2)
        ksn(2) = cs(ix+3)
        scauto(2*i-1) = asn
        jsn(1) = cs(ix)
        ksn(4) = cs(ix+1)
        scauto(2*i) = asn

        if (nearpt(i) .eq. 0) then
          chkauto(i) = 0.0d0
        else
          call ptdsc3(nearpt(i),nwds,ietype,chkauto(i))
        endif

        if (i .lt. nds) then
           nlkahd(i) = ds(3)
        else if (i .eq. nds) then
           nlkahd(i) = ncs
        else
           nlkahd(i) = 0
        end if
        csauto(i) = .true.
        muauto(i) = eval(2*i+3)
        mvauto(i) = eval(2*i+4)
        avauto(i) = cs(ix+4).eq.1
        ix  = ix+5
100   continue
c
c...   Store last curve/surface evaluations
c
      if (ps(2) .eq. SURF) then
          psuv = .true.
          psu  = eval(1)
          psv  = eval(2)
      endif
c
      if (ds(5) .eq. CURVE .or. ds(5) .eq. SURF) then
          dsuv = .true.
          dsu  = eval(3)
          dsv  = eval(4)
      endif
c
c...Output motion
c
      nline = nline + 1
      wchk = -1
      ifl(270) = -1
      ifl(130) = 0
c
c...Put the multi CS into sc table for the first DS.
c
      do 200, i = 1, ds(3)
           call conv8_8(scauto(2*i-1),sc(10+2*i),2)
           mcsu(i) = muauto(i)
           mcsv(i) = mvauto(i)
           avflgs(i) = avauto(i)
           csflgs(i) = .true.
200   continue

      call motgxx
      lcspl = .false.
      if (ifl(2).lt.1) wchk = ifl(328)-1
c
c...Store cldata
c
      if (ifl(2) .lt. 1) then
          call ncl_tstptr(nrcn, iflg)
          if (iflg .ne. 0) then
              call ptpnum (nn)
              if (nn .gt. 0) then
                iclass = 5200
                isubcl = 5
                call csdist (iclass, isubcl)
                call conv8_8(hldctp(22),hldctp(7),6)
                iclass = 5220
                isubcl = 3
                numitm = 13
                call putcl (iclass,isubcl,numitm,hldctp)
              endif
              call ptclos
          endif
      endif
c
c...End of routine
c
 8000 ierr   = ifl(2)
      AutoGofwd = .false.
      lautgf = .false.
      return
      end
