C**********************************************************************
C*    NAME         :  motgxx.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       motgxx.f , 26.4
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/12/19 , 14:17:10
C**********************************************************************
C**********************************************************************
C**  PROGRAM NAME: MOTGXX
C**
C**  PURPOSE OF PROGRAM: HANDLE GOFWD,GB,GL,GR WITH 2ND INTOF &
C**                      MULTIPLE CHECK SURFS
C**
C**********************************************************************
C**********************************************************************
 
      subroutine motgxx
 
      include 'com4a.com'
      include 'cmm.com'
      include 'mocom.com'
      include 'gidcom.com'
      include 'gidcm1.com'
      include 'suvcom.com'

      common/avdcom/avflgs
      logical avflgs(5)
      
c
c...  Local variables for Auto Gofwd.
c
      common/autofwd/scauto,csauto,avauto,chkauto,muauto,mvauto,
     *       nlkahd,lcheck,nlstcs
      common/hitflag/lhit
      common/flcspl/lcspl
      real*8 scauto(1008),chkauto(504),sc12,dbgdata(8)
      real*4 muauto(504),mvauto(504)
      integer*2 nlkahd(504),nds,ids,nlstcs,isrf
      logical avauto(504),csauto(504),lcheck,lhit,lcspl
      equivalence (ifl(51),ia),(ifl(54),isrf)

      common/cptcom/chkpt
      real*8 chkpt(5),hsc(22),tvlab,asn,dismin,svdpm,svmxng,crv_tol
      real*4 svpsu,svpsv,svdsu,svdsv,svcsu,svcsv,csdis,csdism,
     *       hldpsu,hldpsv,hlddsu,hlddsv,tmin(9),tiasv(30),s3sav(10)

      common/stdist/csdis0,itry,itfl0
      real*4 csdis0
      integer*2 itry,itfl0,icsfl0
C
C...The addition of cs1thk thru cs5thk is for holding the
C...values of sc(132),  and sc(177), thru sc(180) for
C...processing of multiple thick. sc25 is a holder
C...for the correct value of sc(25) to be processed.
C
      real*8 csthk(5),sc25,sc36,t25,t26,t27,tol,sc160,sc176,svsc12,
     *       svsc25,svsc36
      integer*4 kps,kds,kcs(5),kcps,nclkey,ncl_check_key,kcsv
      integer*2 ltv(4),ksn(4),ntk,ia,ic,mod,isf,
     *          i,ichk, nchk,ichksv, nwds, ietype,isvprt,isvcut,nintof,
     *          npts,idntct,isvnpt,isv246,isv247,isv248,isv249,isv250
      logical lps,lds,lcs,lsvstp,lcomb,lonext,skipflg
C
C... Hard-coded constant:
C
      real*4 BIG_VAL
      parameter (BIG_VAL=1.e+09)
C
C...motion common equivalences
C
      equivalence (ifl(53),ic),(ifl(23),mod)
      equivalence (tvlab,ltv)
      equivalence (asn,ksn)
      equivalence (ifl(79),ntk), (kcps,ifl(337))
      equivalence (ifl4(15),kds), (ifl4(16),kcsv)

      real*8 dtmp, min2_r8, svsc(9)
      logical lavoided
      logical lv90,lv92,lv93

      real*4 asc63,cal,f_dot4,asc(100)
      equivalence (sc,asc),(asc(63),asc63)

      if (ifl(276).eq.1) then
        call gtdesc (sc(11),kds,nwds,ietype)
        call drvwir (kds)
        goto 999
      endif

      lv90 = sc(169).lt.9.099d0
      lv92 = sc(169).lt.9.249d0
      lv93 = sc(169).lt.9.349d0
      ifl(328) = 1
      call conv8_8(sc,svsc,9)

      if (lautgf) then
         ids = 1
         nds = isc10(2)
      end if
 
      tol = sc(27)
C
C...Get the part surface and drive surface id numbers, kps and 
C...kds respectively.
C
      call gtdesc (sc(35),kps,nwds,ietype)
      call gtdesc (sc(11),kds,nwds,ietype)
   
      if (kps.eq.kds) then
        ifl(2)=28
        goto 999
      endif
c
c... AutoGofwd, check if ds type is valid.
c 
      if (lautgf) then
         if (ietype.eq.SURF) then
            call sftype (kds, ietype)
            if (ietype.eq.MESHSF .or. ietype.eq.QUILTSF
     *      .or. ietype.eq.NETSF) then 
               ifl(2) = 30
               goto 999
            end if
         end if
         do i = 1, nlkahd(1)
            chkpt(i) = chkauto(i)
            csflgs(i) = csauto(i)
            avflgs(i) = avauto(i)
            mcsu(i) = muauto(i)
            mcsv(i) = mvauto(i)
         enddo
         if (gfadbg) call gfadbglbl
      end if

10    continue
      isf = 2
      if (.not. lrmill) call uystup(isf,kds,0,0)
      skipflg = .false.
      lhit = .false.

      svpsu = psu
      svpsv = psv
      svdsu = dsu
      svdsv = dsv
      svcsu = csu
      svcsv = csv
      lps   = psuv
      lds   = dsuv
      lcs   = csuv

      t25 = 0.
      t26 = 0.
      t27 = 0.
      csdism = 0.
      sc160 = 0.
      sc176 = 0.

      isvcut = ifl(42)
      isvprt = ifl(154)

      do 15 i=1,9,1
  15    tmin(i) = 0.
C
C...Saving values of multiple thick for restoration after
C...processing
C
      csthk(1)=sc(132)
      csthk(2)=sc(177)
      csthk(3)=sc(178)
      csthk(4)=sc(179)
      csthk(5)=sc(180)
C
C...Initializing sc25 with sc(25) for when there isn't a multiple thick,
C...otherwise it would end up being zero.
C
      sc25=sc(25)

      call conv8_8(sc,hsc,22)
      ichksv   = 1
c
c... Get #of multiple CS.
c
      if (lautgf) then
         nchk     = nlkahd(ids)
c
c... Do not allow # of CS out of range.
c
         if (ids+nchk.gt.nds+nlstcs) nchk = nds+nlstcs-ids
      else
         nchk     = isc10(2)
      end if
      isc10(2) = 1

C
C...Make sure that the check surfaces are not the same as either
C...the part surface or the drive surface.
C
      isf = 3
      do 11 ichk=1,nchk
        call gtdesc (sc(11+ichk*2),kcs(ichk),nwds,ietype)
        if (kps.eq.kcs(ichk) .or. kds.eq.kcs(ichk)) then
          ifl(2)=30
c
c... jingrong 06/28/99 if multi CS, ignore this CS if error occurs.
c goto 999
          if(nchk.eq.1) goto 999
        end if
c
c... AutoGofwd, setup UY struct for openncl and check if cs type is valid. 
c
        if (lautgf) then
           if (ietype.eq.SURF) then
              call sftype (kcs(ichk), ietype)
              if (ietype.eq.MESHSF .or. ietype.eq.QUILTSF
     *           .or. ietype.eq.NETSF) then  
                 ifl(2) = 30
                 goto 999
              end if
           end if
           if (lcspl .and. ids.eq.nds .and. ietype.eq.PLANE) then
             call uystcs (ichk,nchk)
           else
             call uystup(isf,kcs(ichk),ichk,nchk)
           endif
         end if
11    continue

      if (.not.lv92 .and. lcvgid .and. giduon.eq.0) then
        gidu0 = 0.5
        if (autouv) then
          call gtdesc (gidasw, nclkey, nwds, ietype)
          if (nclkey .eq. gdkey) gidu0 = gidu
        endif
      endif
      if (.not.psuv) call getsuv(sc(35),1,psu,psv)
      if (.not.dsuv) call getsuv(sc(11),2,dsu,dsv)
      hldpsu = psu
      hldpsv = psv
      hlddsu = dsu
      hlddsv = dsv
C
C...vp 7/7/97 initialize cv on surface if psis is defined as cv on ds
C
      if (kcps .ne. 0) then
         ifl(2) = ncl_check_key(kds,kps)
         if (ifl(2) .ne. 0) go to 999
C
C... aak 14-nov-1997: evaluate curve with a tolerance better than machining tol.
C... if psmult: drive as a multiple PS; if (!psmult): as one curve
C
         crv_tol = 0.2*tol

         if(.not.psmult) then
            call ncl_cvonsf_init(kds,kps,1,crv_tol,ifl(2))
         else
            call ncl_cvonsf_init_mult(kds,kps,1,crv_tol,ifl(2))
         endif

         if (ifl(2) .ne. 0) go to 999
         call ptdsc3 (kds,1,9,sc(11))
         kcps = kps
      end if
  
      if (lcmm) then
        idntct   = ifl(42)
c
c... to avoid multiple tool paths to be written to the cl file when
c... multiple check surfaces are used
c        ifl(42)  = 2
        isvnpt   = ifl(91)
        isvprt   = ifl(154)
        ifl(154) = 2
        svdpm    = sc(54)
c
c... store the min distance and the max angle
c
        sc(54)   = cmmmxd
        svmxng   = sc(80)
        sc(80)   = cmmmxa
        lsvstp   = lstep
        lstep    = .true.
        isv246   = ifl(246)
        isv247   = ifl(247)
        isv248   = ifl(248)
        isv249   = ifl(249)
        isv250   = ifl(250)
        ifl(246) = cmminm
        ifl(247) = 1
        ifl(248) = 0
        ifl(250) = 0
      endif
C
C... COMBIN motion flag
C
      lcomb = .false.
      if (mod .eq. 8 .or. mod .eq. 9) lcomb = .true.
      if (nchk.eq.1) goto 120

      isvcut   = ifl(42)
C
C...ifl(42) = 2
c...Internal request not to cut. 
C

      ifl(42)  = 2
      isvprt   = ifl(154)
      ifl(154) = 2
      dismin   = BIG_VAL
      dtmp = -1.
      lavoided = .true.
      icsfl0 = 0
C
C...The following loop drives to the different check surfaces and
C...to see which check surface is the closest, so that we may use
C...that particular check surface
C
      do 100 ichk=1,nchk
       if (.not.skipflg) then
         asn     = hsc(10+ichk*2)
         SC(12)  = ksn(1)
         SC(13)  = hsc(11+ichk*2)
C
C...Set CS thick sc(25) to be the thick value of the current
C...check surface.
C...Use ifl(349) to indicate that we are just checking multiple
C...check surfaces.  sc(181) holds the minimum check surface distance
C...so far.  JLS 6/18/99
C
         ifl(349) =1
         sc(181) = dismin
         call gtdesc (sc(13),kcs(ichk),nwds,ietype)
c
c... Determining the right CS modifier for Auto Gofwd
c
         if (lautgf .and. sc(12).eq.0) 
     *        call solve_cs_modifier (skipflg)
         if (ifl(2).gt.0) goto 20
c
c... Set avoid flag if cs is surface and avoid has been specified
c
         lavoid = avflgs(ichk)
         sc(14)  = chkpt(ichk)
         SC(160) = 0.
         SC(176) = 0.
         csuv = csflgs(ichk)
         if (csuv) then
            csu = mcsu(ichk)
            csv = mcsv(ichk)
         endif
C
C...for ta/combin multiple ios on CS are not supported
C...this time
C 
         if (lcomb) ksn(2) = 1
c
c... skip this motion if skip flag is set.
c
         if (skipflg) ksn(2) = 0

         do 18, i=1,ksn(2)         
            sc(25)=csthk(ichk)
            if (lcomb) then
               call mocomb
            else
                call vctovc(sc(1),sc(217))
                call vctovc(sc(4),sc(220))
                call domove
            end if
            if (lavoided) lavoided = lavdd
            if (ifl(2).gt.0) then
c
c..... OML only by now - if the tool is within tolerance to this CS 
c..... at start - mark it
c
              if (ksn(2).eq.1 .and. itfl0.eq.5 .and. icsfl0.eq.0)
     *          icsfl0 = ichk
              goto 20
            endif
c
c... If Auto Gofwd, check if hit cs extension or not. 
c
            if (lautgf.and.ksn(1).eq.0.and.sc(12).eq.CS_TO.and.
     x          .not.lavdd) then
               call if_proj_on_extension (kcs(ichk),ietype,1,lonext)
               if (ifl(2).gt.0) goto 20
               if (lonext) call do_move_past(lcomb)
               if (ifl(2).gt.0) goto 20
            end if
            isc10(1) = 704
18       continue
         dtmp = min2_r8 (sc(160),sc(176))
         if (lv90) dtmp = sc(160)
C
C...Check to see if this distance is smaller than
C...the previous minimum distance, if it is keep the
C...info associated with it incase it turns out to be
C...the desired check surface.
c
c...jingrong 06/24/99 if traveled dis are the same for different CS,pick the
c...last one.
c         if (dismin .gt. dtmp) then

         if (.not.lv92 .and. lhit .and. .not.lautgf .and.
     *     (sc(160).gt.(sc160+tol) .and. sc(176).lt.(sc176-tol)) .or.
     *     (sc160.gt.(sc(160)+tol) .and. sc176.lt.(sc(176)-tol))) then
           if (dtmp .le. dismin+tol) then
             asn = hsc(11+ichksv*2)
             if (ksn(4).eq.SURF .and. sc36.eq.-1.) then
               sc(146) = asn
               asn = hsc(10+ichksv*2)
               svsc12 = sc(12)
               svsc25 = sc(25)
               svsc36 = sc(36)
               sc(12) = ksn(1)
               sc(25) = csthk(ichksv)
               sc(36) = -1.
               lttcsf  = .false.
               asn = 0
               ksn(1) = SURF
               d(101) = asn
               csdis = t(12,ic)
               itfl = -2
               iptk = -1
               ia = ic
               call conv4_4(t(1,ia),tiasv,30)
               call conv4_4(s(1,3),s3sav,10)
               t(25,ia) = t25
               t(26,ia) = t26
               t(27,ia) = t27
               isrf = 3
               call sfinit (sc(146), isrf, t(25,ia), t(26,ia))
               if (ifl(340).ne.2 .and. ifl(361).ne.1) then
                 ifl(333) = ifl(340)
                 call tbdini (kcs(ichksv),isrf,tol,ifl(333))
                 if (ifl(2) .gt. 0) goto 19
               endif
               call csrel
               if (t(12,ia).lt.-tol .and. csdis.gt.-tol) dtmp = BIG_VAL
               call conv4_4(tiasv,t(1,ia),30)
               call conv4_4(s3sav,s(1,3),10)
               sc(12) = svsc12
               sc(25) = svsc25
               sc(36) = svsc36
             endif
           else
             asn = sc(146)
             if (ksn(4).eq.SURF .and. sc(36).eq.-1.) then
               lttcsf  = .false.
               asn = 0
               ksn(1) = SURF
               d(101) = asn
               itfl = -2
               iptk = -1
               ia = ic + 1
               if (ia .gt. 3) ia = 1
               call conv4_4(t(1,ia),tiasv,30)
               call conv4_4(s(1,3),s3sav,10)
               call conv4_4(tmin,t(1,ia),9)
               call csrel
               if (t(12,ia).lt.-tol.and. csdism.gt.-tol) dismin=BIG_VAL
               call conv4_4(tiasv,t(1,ia),30)
               call conv4_4(s3sav,s(1,3),10)
             endif
           endif
         endif
19       if (.not.lv93 .and. 
     x       dtmp.gt.dismin+tol .and. dtmp.lt.dismin+10*tol) then
c
c..... if this CS is a little farther than the current best, but 
c..... the tool at the best CS ending position is within tolerance
c..... to this CS - we choose this one
c
             asn = sc(146)
             if (ksn(4).eq.SURF .and. sc(36).eq.-1.) then
               lttcsf  = .false.
               asn = 0
               ksn(1) = SURF
               d(101) = asn
               itfl = -2
               iptk = -1
               ia = ic + 1
               if (ia .gt. 3) ia = 1
               call conv4_4(t(1,ia),tiasv,30)
               call conv4_4(s(1,3),s3sav,10)
               call conv4_4(tmin,t(1,ia),9)
               call csrel
               if (abs(t(12,ia)) .le. sc(168)) then
                 cal = asc63 + f_dot4(t(4,ia),s(1,3))
                 if (cal.le.0.001) dismin=BIG_VAL
               endif
               call conv4_4(tiasv,t(1,ia),30)
               call conv4_4(s3sav,s(1,3),10)
             endif
         endif
         if (dtmp .le. dismin+tol) then
            call conv4_4(t(1,ic),tmin,9)
            csdism = t(12,ic)
            dismin = dtmp
            sc160 = sc(160)
            sc176 = sc(176)
            ichksv = ichk
            tvlab  = SC(11+ichk*3)
            sc25 = csthk(ichk)
            sc12 = sc(12)
            sc36 = sc(36)
            t25 = t(25,ic)
            t26 = t(26,ic)
            t27 = t(27,ic)
            lhit = .true.
         endif
20       continue
c
         if (lautgf .and. gfadbg) call gfadbgprint(dbgdata,
     *    ids,ichk,sc(12),ifl(2),ifl(50),sc(160),lavdd,skipflg)
c
         call conv8_8(hsc,sc,22)
         ifl(2) = 0
         ifl(349) =0
         SC(10) = hsc(10)
         psuv   = lps
         dsuv   = lds
         csuv   = lcs
         psu    = hldpsu
         psv    = hldpsv
         dsu    = hlddsu
         dsv    = hlddsv
       end if
100   continue
c
c..... OML only by now - if no motions finished successfully, and the tool
c..... is within tolerance to some CS at start - use the latest of those
c
      if (icsfl0.gt.0 .and. ichksv.eq.1 .and. dtmp.eq.-1)
     *  ichksv = icsfl0

      ifl(42)  = isvcut
      ifl(154) = isvprt
      sc(13)   = sc(11+ichksv*2)
      ifl(328) = ichksv
      ntk      = 0
      lavoid   = .false.
      if (lavoided .and. .not.skipflg) then
        ifl(2) = 473
        if (.not.lautgf) goto 900
        ifl(2) = -473
      endif

120   continue
      csuv = csflgs(ichksv)

      if (csuv) then
        csu = mcsu(ichksv)
        csv = mcsv(ichksv)
      endif

      asn    = hsc(10+ichksv*2)
      sc(12) = ksn(1)
      if (lautgf.and..not.skipflg) then
          if (.not.lhit.and.nchk.gt.1) sc(12) = 0
          if (nchk.gt.1.and.lhit) then
             sc(12) = sc12
          else if (sc(12).eq.0) then
             call solve_cs_modifier (skipflg)
          end if
          if (ifl(2).gt.0) goto 900
          sc(160) = 0
      end if
c
      sc(14) = chkpt(ichksv)
      nintof = ksn(2)
      if (lcomb) nintof = 1
c
c... If skip flag is set, skip this motion.
c
      if (skipflg) nintof = 0
C
C...Placing the correct CS value in sc(25) to be processed.
C
      sc(25)=sc25
C
C...Now actually do the motion.
C
      csmult = .false.
      if (nintof.gt.1 .and. .not.lcomb) csmult = .true.
      do 140 i=1,nintof
         if (ifl(365).gt.0) then
            call tllock
         else if (lcomb) then
            call mocomb
         else
            if (i .gt. 1) csmult=.true.
            call vctovc(sc(1),sc(217))
            call vctovc(sc(4),sc(220))
            call domove
         end if
         if (ifl(2) .gt. 0) then
c
c..... OML only by now - if the motion finished unsuccessfully, and the tool
c..... is within tolerance to the CS at start - give the warning 141 instead
c..... of error
c
           if (nintof.eq.1 .and. itfl0.eq.5) then
             ifl(2) = -141
           else
           goto 900
           endif
         endif
C
C...IF CIRC INTERP FLG ON & NOT DNTCUT, CALL CIRREC TO
C...ISSUE 3000-2 RECORD TO CLFILE.
C
         if (ifl(95) .eq. 1 .and. ifl(42) .eq. 0) call cirrec
            isc10(1) = 704
C
C...Reinstate RAPID on multiple intersections
C...Bobby  =  8/10/95
C
         if (rpfron .and. i .lt. nintof) call putcl (2000,5,1,0.d0)
         lmintf(1) = .true.
140   continue
      lmintf(1) = .false.
      csmult = .false.
C
C...TURN OFF INDIRV FLAG
C...JUMPTO MLT CHK LABEL
C
      IFL(22) = 0
      if (KSN(3) .ne. 0) then
         call getran(JB,ksn(3))
         call nclf_src_rec_to_line(jb4(ksn(4)),lnum)
         if (lnum .ne. 0) then
            call putw2 (NLINE)
            IFL(47) = 1
C
C...vp 11/10/97 fix offset by number of continuation
C...lines (not just 1) when it is used
C
            NLINE = lnum - ifl(123) 
            call nclf_src_line_to_rec (nline,ifl4(4))
         end if
      end if
C
      if (.not.lv92 .and. lcvgid) then
        call gtdesc(gidasw,gdkey,nwds,ietype)
        giduon = 0
      endif
      kuv(1) = kps
      kuv(2) = kds
       call gtdesc (sc(13),kcs(ichksv),nwds,ietype)
      kcsv = kcs(ichksv)
      kuv(3) = kcs(ichksv)
      hu(1)  = t(13,ic)
      hv(1)  = t(14,ic)
      hu(2)  = t(19,ic)
      hv(2)  = t(20,ic)
      hu(3)  = t(25,ic)
      hv(3)  = t(26,ic)
      autouv = auvset
c
      if (lautgf) then 
         if(gfadbg.and..not.skipflg) then
            call gfadbgprint(dbgdata,ids,ichksv
     *           ,sc(12),ifl(2),ifl(50),sc(160),lavdd,skipflg)
            call gfadbgline
         end if
         ids = ids + ichksv
         if (ids.le.nds) then
c
c... Get the go type for next motion.
c
            if (.not.skipflg) call solve_go_direction(lcomb,skipflg)
            if (ifl(2).gt.0) goto 900
c
c... Get the next CS informtion.
c
            call conv8_8(sc(11+ichksv*2),sc(11),1)
            do 30 i = 1, nlkahd(ids)
               j = ids+i-1
               call conv8_8(scauto(j*2-1),sc(10+i*2),2)
               chkpt(i) = chkauto(j)
               csflgs(i) = csauto(j)
               avflgs(i) = avauto(j)
               mcsu(i) = muauto(j)
               mcsv(i) = mvauto(j)
30          continue
c
c... Current CS becomes next DS.
c
            kds = kcs(ichksv)
            dsu = csu
            dsv = csv
            if (skipflg) call solve_go_direction(lcomb,skipflg)
            call sendcl
            call ncl_setptr(imotp,isrec3)
            goto 10
         else
            call ncl_setptr(imotp,isrec3)
         end if
      end if
      goto 998
C
C...error - restore sc & ps,ds,cs u & v
C
900   call conv8_8(svsc,sc,9) 

      psu  = svpsu
      psv  = svpsv
      dsu  = svdsu
      dsv  = svdsv
      csu  = svcsu
      csv  = svcsv
      psuv = lps
      dsuv = lds
      csuv = lcs
C
C...Restore the original sc values from the multiple thick.
C
      sc(132) = csthk(1)
      sc(177) = csthk(2)
      sc(178) = csthk(3)
      sc(179) = csthk(4)
      sc(180) = csthk(5)
      if (lautgf) then
        errcom(1:7) = 'FOR DS '
        k = 25
        call ncl_getlab (kds, k, errcom(8:))
      endif

998   sc(10) = hsc(10)
      sc(14) = hsc(14)
      if (lcmm) then
        ifl(42) = idntct
        ifl(91) = isvnpt
        ifl(154) = isvprt 
        sc(54) = svdpm
        sc(80) = svmxng
        lstep = lsvstp
        npts = ifl(250)
        ifl(246) = isv246  
        ifl(247) = isv247  
        ifl(248) = isv248  
        ifl(249) = isv249  
        ifl(250) = isv250 
c
c... dont weed and store points here this is done after motion display
c       call strcmm(npts)
      endif
C
999   continue
C
C... aak 09-jan-1998: reset thr multi-PS flag for CVonSF as PS;
C... don't reset for a surface
C
      if (kcps .ne. 0) psmult = .false. 
C
C...Check Surface needs to be restored to it's original value.
C
      sc(25)=sc(132)
c
c... Reset AutoGofwd flag.
c
      lautgf = .false.
      itfl0 = 0

      RETURN
      END
c
c***********************************************************************
c
c   SUBROUTINE:  if_proj_on_extension (kcs,ietype,kfl,lonext)
c
c   FUNCTION:  This routine determines if the check surface contact
c              point is on the extension of the check surface.
c
c   INPUT:  kcs      I*4   D1  -  Key of check surface.
c           ietype   I*2   D1  -  Geometry type (9 = surface).
c           kfl      I*2   D1  -  1 = Determine if any part of the tool
c                                 will touch check surface, used for
c                                 GOFWDA logic.  2 = Determine only if
c                                 tool contact point is on surface extension.
c
c   OUTPUT: lonext   L*2   D1  -  TRUE if contact point is on surface
c                                 extension.
c
c***********************************************************************
c
      subroutine if_proj_on_extension (kcs,ietype,kfl,lonext)
c
      include 'com.com'
      include 'mocom.com'
      include 'wrksys.com'
c
c...  motion common equivalences
c
      real*4 ad(300),asc(100)
      real*8 ctol
      integer*2 ia,ic,jd(600)
      equivalence (ifl(53),ic),(ifl(51),ia)
      equivalence (sc,asc),(sc(168),ctol),(d,ad,jd)
c
c...  local variables
c
      real*8 pt1(3),ta(6),dis,dis1,s8(10),s8sav(10),ht(4),vec(3)
      real*8 dt1,dt2,sbe,cbe,tbe,hdia,crad,thgt,cute,plfwd(4),ftol
      real*8 f_dot,f_mag,surf_struct(20)
      real*4 prj(3),ve(3),sec,u,v,t3sv(9),f_dist4
      integer*2 ietype,iasv,i,n
      integer*4 kcs,reproj_on_lnci
      logical lonext
      integer*2 ionext
      real*4 f_mag4,f_dot4
      integer*2 i2v3 /3/
c
c... Save off tool and surface values
c
      iasv = ia
      ia = ic
      call conv4_4(t(1,ia),t3sv,9)
      call conv4_8(s(1,3),s8sav,10)
      call conv4_8(t(1,ia),ta,6)
      call conv4_8(t(7,ia),plfwd,3)
c
c... If TL_ON slightly offset tool end to the right to varify position
c
      ftol = sc(27)
      if (ifl(21).eq.0) then
         call f_cross(plfwd,ta(4),vec)
         call uvcplvc (ta,vec,ta,2.*ftol)
         call conv8_4(ta,t(1,ia),3)
      end if
c
      sbe  = asc(63)
      cbe  = asc(64)
      if (cbe.eq.0.0d0) cbe = 1.0d0
      tbe  = sbe/cbe
c
c... Reduce tool by tolerance
c
      hdia = tool(1)/2.0-ftol
      crad = tool(2)-ftol
      thgt = tool(3)-ftol*2
      if (thgt.lt.0.d0) thgt = 0.d0
      cute = tool(6)
c
c... dt1 is const of plane through te perpto ta
c... dt2 is height up tool axis where corner radius ends
c
      dt1 = f_dot(ta,ta(4))
      dt2 = crad*(1-sbe)
c
c... Initialize variables.
c
      ifl(54) = 3
      lonext = .true.
      n = 1
      ht(1) = 0.
      if (ietype.eq.SURF) then
         n = 4
         u = t(25,ia)
         v = t(26,ia)
c
c... Zero tool axis to stay out of corner logic in usurfpn
c
         t(4,ia) = 0.0
         t(5,ia) = 0.0
         t(6,ia) = 0.0
c
c... Create 3 heights up tool to project onto cs
c
         ht(2) = dt2
         ht(3) = tool(3)
         ht(4) = tool(3)/2.0d0
      end if
c
c... Try n points up tool axis until we find tool contacting real cs
c
      do 80 i=1,n
        if (lonext) then
          call uvcplvc (ta,ta(4),s8(8),ht(i))
          call conv8_4(s8(8),s(8,3),3)
c
c... cs is sf     
c
          if (ietype.eq.SURF) then
             call surfpn (u,v,1)
             if (ifl(2).gt.0) goto 80 
             call conv4_8 (s(1,3),surf_struct(1),7)
             surf_struct(8) = u
             surf_struct(9) = v
c
c... If trimmed sf & spt is out of boundary, reproj it to the boundary
c
             call ncl_reproj_on_trimsf (kcs,surf_struct)
             call conv8_8 (surf_struct,s8,7)
c
c... cs is cv 
c
          else if (ietype.eq.CURVE) then
             call curvpv(s(5,3),s(6,3),s(7,3),ve(1),ve(2),ve(3))
             if (ifl(2).gt.0) ifl(2) = 256
c
c... cs is circle
c
          else if (ietype.eq.CIRCLE) then
             call circpv(s(5,3),s(6,3),s(7,3),ve(1),ve(2),ve(3))
             ionext =  reproj_on_lnci (s(5,3),kcs)
             if (ionext.lt.0) ifl(2) = 256
             lonext = (ionext.gt.0)
c
c... cs is line
c
          else if (ietype.eq.LINE) then
             call conv8_4(d(106),ve,3)
             sec = f_mag4(ve)
             if (sec.eq.0) then
                ifl(2) = 256
             else
                call unitvc4(ve,ve)
                call vctovc4(s(8,3),prj)
                ionext = reproj_on_lnci (prj,kcs)
                if (ionext.lt.0) ifl(2) = 256
                lonext = (ionext.gt.0)
                call vctovc4(prj,s(5,3))
             end if
c
c... error. invalid cs type.
c
         else
            ifl(2) = 30
         end if
         if (ifl(2).gt.0) goto 80
c
c.. TLON
c
         if (ifl(21).eq.0 .or. kfl .eq. 2) then
c
c... project point on cs to the extension
c
            if (ietype.eq.CURVE) then
               call vcmnvc4(s(8,3),s(5,3),prj)
               sec = f_dot4(prj,ve)
               call uvcplvc4(s(5,3),ve,prj,sec)
               lonext = f_dist4(s(5,3),prj).gt.ftol
c
            else if (ietype.eq.SURF) then
                call vcmnvc(s8(8),s8(5),vec)
                call uvcplvc(s8(8),s8,pt1,-f_dot(vec,s8))
                call vcmnvc(s8(5),pt1,vec)
                lonext = f_mag(vec).gt.ftol
            end if
            goto 80
          end if
c
c...csn is crossf v & ta
c
         if (ietype.ne.SURF) then
            call f_cross4(ve,t(4,ia),ve)
c
c... if sec zero, some special case exists.   ta//v?
c
            sec = f_mag4(ve)
            if(sec.eq.0.) then
                 ifl(2) = 256
                 goto 80
            end if
      
            if (f_dot4(ve,t(7,ia)).lt.0) call mnvc4(ve)
            call unitvc4(ve,s(1,3))
            s(4,3) = f_dot4(s(1,3),s(5,3))
            call conv4_8(s(1,3),s8,7)
        end if
c
c... Project surface point onto tool axis
c
          dis1 = f_dot(s8(8),s8)-s8(4)
          call uvcplvc (s8(5),s8,pt1,dis1)
c
c... Determine if surface pt projects onto tool between top and bottom.
c
          dis1 = f_dot(pt1,ta(4))-dt1
          if (dis1.ge.0.0d0 .and. dis1.le.thgt) then
c
c... Calculate radius of tool at point on axis that surface point projects to
c
            if (dis1.ge.dt2.or.(ietype.ne.SURF.and..not.lcntct)) then
              dis1 = cute+crad*cbe+(dis1-dt2)*tbe
            else
              dis  = crad**2 - (dt2-dis1+crad*sbe)**2
              dis1 = 0.0d0
              if (dis.gt.0.0d0) dis1 = dsqrt(dis)+cute
            endif
c
c... Project surface point onto plane through external point perpto forward
c
            plfwd(4) = f_dot(s8(8),plfwd)
            call plndis(plfwd,s8(5),dis)
            call uvcplvc (s8(5),plfwd,vec,-dis)
c
c... Calc distance from projected point to tool axis & compare to tool radius
c
            call ptlnds (vec,ta,dis)
            lonext = dis.gt.dis1+sc(25)
            if (.not. lonext) then
c
c... Project surface point onto plane through external point perpto cs
c
              call vctovc(s8,plfwd)
              plfwd(4) = f_dot(s8(8),plfwd)
              call plndis(plfwd,s8(5),dis)
              call uvcplvc (s8(5),plfwd,vec,-dis)
c
c... Calc distance from projected point to tool axis & compare to tool radius
c
              call ptlnds (vec,ta,dis)
              lonext = dis.gt.dis1+sc(25)
            endif
        endif
      end if
80    continue
c
c... Restore tool and surface tables.
c
      call conv4_4(t3sv,t(1,ic),9)
      call conv8_4(s8sav,s(1,3),10)
      ia = iasv
c
      return
      end
c************************************************************
c     SUBROUTINE reproj_on_lnci(prj, kcs)
C     PURPOSE: Reproject a point onto a line or circle respecting
C              units and modsys.
C     INPUT  : prj    - Point to project
C     OUTPUT : kcs    - Key of line/circle
c
c***********************************************************
c
      integer*4 function reproj_on_lnci(prj, kcs)

      real*4 prj(3)
      integer*4 ncl_reproj_on_lnci8, kcs
      integer*2 i2v3 /3/

      include 'com.com'
      include 'wrksys.com'

      real*8 pt1(3), fct

      call conv4_8(prj,pt1,i2v3)
      if (ifl(264).eq.1) then
        fct = 1.0d0/25.4d0
        call vctmsc(pt1,pt1,fct)
      endif
      if (lwrk) then
        call conent(pt1,wrkmx,i2v3)
      endif
      reproj_on_lnci =  ncl_reproj_on_lnci8 (pt1,kcs)
      if (lwrk) then
        call conent(pt1,invwrk,i2v3)
      endif
      if (ifl(264).eq.1) then
        fct = 25.4d0
        call vctmsc(pt1,pt1,fct)
      endif
      call conv8_4(pt1,prj,i2v3)
c
      return
      end
c***********************************************************
C      SUBROUTINE SOLVE_CS_MODIFIER (SKIPFLG)
C      PURPOSE: GET THE RIGHT CS_MODIFIER DEPENDING ON THE
C               DS AND CS GEOMETRY
C      INPUT  : sc(11),sc(13) table wd of DS & CS
C      OUTPUT : SC(12) - CS MODIFIER
C               SKIPFLG - 1 IF TOOL IS ALREADY AT THE ENDING 
C                           POSITION
C************************************************************
c***********************************************************
      subroutine solve_cs_modifier (skipflg)
c
      include 'com.com'
      include 'mocom.com'
C
      logical skipflg
C
      integer*2 ia,ic,isrf, iasv, icsv
      equivalence (ifl(51),ia),(ifl(53),ic),(ifl(54),isrf)

      common/hitflag/lhit
      logical lhit
c
c...local variables
c
      integer*2 cicvln
      integer*4 kcs, kds
      integer*2 istanto, idstyp,icstyp, nwds,iscnct
      real*4 ctol,co,t3sv(9),s3sv(10),s2sv(10),tfwd(3),f_dist4,f_dot4
      real*8 pt1(3),pt2(3)
      logical lonext
c
c... Get cs,ds info
c
      call gtdesc (sc(11),kds,nwds,idstyp)
      call gtdesc (sc(13),kcs,nwds,icstyp)
      skipflg = .false.
c
c... if ds/cs is a plane, drive CS_TO
c
      if(icstyp.eq.PLANE) then
        sc(12) = CS_TO
        t(12,2) = 0
        goto 99
       end if
c
c... save sf and tool info, Initialize variables.
c
      call conv4_4(t(1,3),t3sv,9)
      call conv4_4(s(1,3),s3sv,10)
      call conv4_4(s(1,2),s2sv,10)
      iasv = ia
      icsv = ic
      istanto = 0
      lonext = .false.
      ctol = sc(27)
      ia = 3
      ic = 3
      ifl(49) = 0
c
c... If ds tanto cs, drive TNTO
c
      if (idstyp.eq.SURF.and.icstyp.eq.SURF) then
         call ncl_2sf_tanto(kds,kcs,istanto,iscnct,ifl(2))
c
c... call ncl_2cv_tanto only if cs and ds are both curves
c

	else if (idstyp.ne.PLANE .and. cicvln(idstyp).eq.1
     x			.and. cicvln(icstyp).eq.1) then
         call ncl_2cv_tanto(kds,kcs,istanto,iscnct,pt1,pt2,ifl(2))
      end if
      if (ifl(2).ne.0) goto 98
      if (istanto.eq.1) then
        sc(12) = TNTO
        if (idstyp.eq.LINE .and. icstyp.eq.LINE) skipflg = .true.
c
c... If TLON, CS_ON
c
      else if (ifl(21).eq.0) then
        sc(12) = CS_ON
      else
c
c... Otherwise, always try CS_TO first
c
        sc(12) = CS_TO
        call premov
c
c... get initial fwd, see if projects on cs extension
c
        if (isc10(1).ne.GOFWD) then
             isrf = 2
             call dsrel
             call f_cross4(s(1,2),t(4,3),tfwd)
             call unitvc4(tfwd,tfwd)
             if (f_dot4(tfwd,t(7,3)).lt.0) call mnvc4(tfwd)
             call vctovc4(tfwd,t(7,3))
        end if
        isrf = 3
        call csrel
c
c... if the tool is moving parellel to cs,set 163 error flag.
c
        if (abs(f_dot4(t(7,3),s(1,3))).lt.0.001 .and.lhit
     *     .and. iscnct.eq.0) ifl(2) = 163
        if (ifl(2).gt.0) goto 98
c
c... CS_TO special case: right on the ending position,SKIP.
c
        if (abs(t(12,3)).lt.ctol) then
             call if_proj_on_extension(kcs,icstyp,1,lonext)
c
c... If tool is right on the ending position, skip this surface
c
             if (.not.lonext) skipflg = .true.
        end if
c
c... CS_TO special case: position has been pasted, CS_PAST. 
c
        if (t(12,3).lt.ctol .and. .not.(icstyp.eq.SURF .and.
     *   lavoid .and. f_dist4(s(5,3),s(8,3)).gt.2.*tool(2)))
     *      sc(12) = CS_PAST
        if (lhit .and. t(12,3).lt.-sc(181)) sc(12) = CS_TO
      end if
c
c... TNTO or CS_PAST
c
      if (sc(12).ne.CS_TO.and..not.skipflg) then
        call premov
        if (ifl(2).gt.0) then
          if (ifl(2).eq.173.and.
     x        idstyp.eq.CIRCLE.and.icstyp.eq.CIRCLE) then
             ifl(2) = 0
             skipflg = .true.
           endif
           goto 98
        endif
        isrf = 3
        call csrel
        if (ifl(2).gt.0) goto 98
c
c... TNTO specical case: position has been pasted, CS_TO.
c
        if (sc(12) .eq. TNTO .and. ifl(21).ne.0 .and.
     *      isc10(1).ne.GOFWD) then
           co = f_dot4(t(7,3),s(1,3))
           call if_proj_on_extension(kcs,icstyp,1,lonext)
c
c... if fwd // cs plane normal, recalculate the t(12,3).
c
           if (abs(co).lt.0.001 .and. .not.lonext) then
              call f_cross4(t(7,3),t(4,3),t(7,3))
              if (ifl(21).eq.-1) call mnvc4(t(7,3))
              isrf = 3
              call csrel
              if (t(12,3).lt.ctol) sc(12) = CS_TO
           end if
        end if
c
c... TNTO or CS_PAST special case: right on the ending postion, SKIP.
c
        if (abs(t(12,3)).lt.ctol) then
             if (sc(12).eq.CS_PAST) then
                lonext = .false.
c
c... get initial fwd, see if projects on cs extension
c
                if (isc10(1).eq.GOFWD) then
                   isrf = 2
                   call dsrel
                   call f_cross4(s(1,2),t(4,3),tfwd)
                   call unitvc4(tfwd,tfwd)
                   if (f_dot4(tfwd,t(7,3)).lt.0) call mnvc4(tfwd)
                   call vctovc4(tfwd,t(7,3))
                end if
                call if_proj_on_extension(kcs,icstyp,1,lonext)
                skipflg = .not.lonext
             else
                d1 = f_dist(sc,pt1)
                d2 = f_dist(sc,pt2)
                skipflg = d1 .lt. d2
             end if
c
c... CS_PAST special case: postion has been pasted, CS_TO.
c
        else if (sc(12).eq.CS_PAST) then
             if (t(12,3).lt.0) sc(12) = CS_TO
        end if
      end if
c
c... store t(12,3) value in t(12,2) for later use.
c
      t(12,2) = t(12,3)
c
c... restore surface and tool info.
c
98     call conv4_4(t3sv,t(1,3),9)
       call conv4_4(s3sv,s(1,3),10) 
       call conv4_4(s2sv,s(1,2),10)
       ia = iasv
       ic = icsv
99     return
       end
c
c***********************************************************
c***********************************************************
C      SUBROUTINE SOLVE_GO_DIRECTION (KCS,LCOMB)
C      PURPOSE: GET THE RIGHT GO DIRECTION FOR THE NEXT MOTION
C               IN AUTO GOFWD, DEPENDING ON 
C      INPUT  : KCS - KEY OF THE CS OF THE CURRENT MOTION
C               LCOMB - MOTION COMBINE FLAG
C      OUTPUT : ISC10(1) - GO DIRECTION
c**********************************************************
c***********************************************************
       subroutine solve_go_direction (lcomb,skipflg)
c
      include 'com.com'
      include 'mocom.com'
C
      real*8 asn
      integer*2 ksn(4)
      equivalence (asn,ksn)
c
c...local variables
c
      real*4 co
      integer*2 istanto,iscnct,idstyp,icstyp, nwds
      integer*4 kcs,kds
      real*8 sc12, pt1(3), pt2(3)
      logical lonext, lcomb,skipflg
      real*4 f_dot4
c
c... if this motion has been skipped
c
      if (skipflg) then
         isc10(1) = GOFWD
         sc12 = sc(12)
         call solve_cs_modifier(skipflg)
         if (skipflg .or. sc(12).eq.CS_TO
     *      .and. t(12,2).lt.0) isc10(1) = GOBACK
         sc(12) = sc12
         ifl(2) = 0
         goto 99
       end if
c
c... Get cs,ds info
c
      call gtdesc (sc(11),kds,nwds,idstyp)
      call gtdesc (sc(13),kcs,nwds,icstyp)
c
c... CASE TNTO, GOFWD
c
      if (sc(12).eq.TNTO) then
         isc10(1) = GOFWD
c
c... CASE TLON
c
      else if (sc(12).eq.CS_ON) then
         co = f_dot4(s(1,3),t(7,3))
         if (abs(co) .lt. 0.5) then
            isc10(1) = GOFWD
         else
            call if_proj_on_extension (kcs,icstyp,1,lonext)
            if (ifl(2).gt.0) goto 99
c
c... if a right pt proj on cs extension, GOLFT; otherwise GORGT.
c
            if (lonext) then
               isc10(1) = GOLFT
            else
               isc10(1) = GORGT
            end if
         end if
c
c... CASE CS_TO
c
      else if (sc(12).eq.CS_TO) then
         lonext = .false.
c
c...ksn(1) was used here without ever being set and we don't know why.
c
c        if (ksn(1).eq.0) call if_proj_on_extension(kcs,icstyp,1,lonext)
         call if_proj_on_extension(kcs,icstyp,1,lonext)
c
c... if cs proj pt on cs extension, continue to do CS_PAST
c
         if(lonext) then
             call do_move_past (lcomb)
             if(ifl(2).gt.0) goto 99 
         else
c
c... if TLRGT,GORGT; otherwise if TLLFT, GOLFT;
c
            if (ifl(21).eq.1) then
               isc10(1) = GORGT
            else if (ifl(21).eq.-1) then
               isc10(1) = GOLFT
            end if
            co = f_dot4(s(1,3),t(7,3))
            if (abs(co).lt.0.5) then
               iscnct = 0
               call gtdesc (sc(11),kds,nwds,idstyp)
               call gtdesc (sc(13),kcs,nwds,icstyp)
               if (idstyp.eq.SURF.and.icstyp.eq.SURF) then
                  call ncl_2sf_tanto(kds,kcs,istanto,iscnct,ifl(2))
               else if (idstyp.ne.PLANE) then
                  call ncl_2cv_tanto(kds,kcs,istanto,iscnct,pt1,pt2,
     x                              ifl(2))
               end if
               if (iscnct.eq.1) isc10(1) = GOFWD
            end if
         end if
      end if
c
c... CASE CS_PAST
c
      if (sc(12).eq.CS_PAST) then
c
c... if TLRGT, GOLFT; otherwise if TLLFT, GORGT;
c
         if (ifl(21).eq.1) then
            isc10(1) = GOLFT
         else if (ifl(21).eq.-1) then
            isc10(1) = GORGT
         end if
         co = f_dot4(s(1,3),t(7,3))
         if (abs(co).lt.0.5) then
            iscnct = 0
            call gtdesc (sc(11),kds,nwds,idstyp)
            call gtdesc (sc(13),kcs,nwds,icstyp)
            if (idstyp.eq.SURF.and.icstyp.eq.SURF) then
               call ncl_2sf_tanto(kds,kcs,istanto,iscnct,ifl(2))
            else if (idstyp.ne.PLANE) then
               call ncl_2cv_tanto(kds,kcs,istanto,iscnct,pt1,pt2,ifl(2))
            end if
            if (iscnct.eq.1) isc10(1) = GOFWD
         end if
      end if

99    return
      end
c
c*******************************************************
c*******************************************************
C     SUBROUTINE DO_MOVE_PAST (LCOMB)
C     PURPOSE: COTINUE THE MOTION FROM A CS_TO POSITION
C              TO THE CS_PAST POSITION
C     INPUT  : LCOMB - MOTION COMBINE FLAG)
C     OUTPUT : NONE
C******************************************************
c*******************************************************
c
      subroutine do_move_past (lcomb)
c
      include 'com.com'
 
      logical lcomb

      sc(12) = CS_PAST
      isc10(1) = GOFWD
c
c..... added for fsr 61178
c
      call sendcl

      if (lcomb) then
         call mocomb
      else
         call vctovc(sc(1),sc(217))
         call vctovc(sc(4),sc(220)) 
         call domove
      end if

      return
      end
c*******************************************************
c*******************************************************
C     SUBROUTINE GFADBGPRINT
C     PURPOSE: PRINT OUT DEBUG INFORMATION FOR AUTO GOFWD  
C     INPUT :
C     OUTPUT : NONE
C******************************************************
C******************************************************
      subroutine gfadbgprint(data,ids,ichk,
     *           mod,err,iptk,dis,avoid,skip)
c
      real*8 data(8),mod,dis
      integer*2 ids,ichk,err,iptk
      logical avoid,skip

      data(1) = ids
      data(2) = ichk
      data(3) = mod
      data(4) = err
      data(5) = iptk
      data(6) = dis
      data(7) = 0
      if (avoid) data(7) = 1
      data(8) = 0
      if (skip) data(8) = 1
      call gfadbgdata(data)
c
      return
      end
c
c*******************************************************
C     subroutine sendcl
C     Purpose: Write currently saved motion points to internal clfile
C     input  : None
C     output : None
c*******************************************************
c
      subroutine sendcl
c
      include 'com.com'

      common/hptcom/hldctp
      real*8 hldctp(27)

      integer*4 nn
      integer*2 i, iclass, isubcl, numitm, iflg
      logical lv92

      lv92 = sc(169) .lt. 9.249d0

      call ncl_tstptr(nrcn,iflg)
      if (iflg .ne. 0 .and. sc(169) .gt. 8.229) then
          call ptpnum (nn)
          if (nn .gt. 0) then
cc            if (lexpcl) then
            if (.not. lv92) then
              iclass = 5200
            else
              iclass = 5000
            endif
            isubcl = 5
            call csdist (iclass, isubcl)
cc            if (lexpcl) then
            if (.not. lv92) then
              do 60 i=1,6
60            hldctp(i+6) = hldctp(i+21)
              iclass = 5220
              isubcl = 3
              numitm = 13
              call putcl (iclass,isubcl,numitm,hldctp)
            endif
          endif
          call ptclos
      endif

      return
      end
