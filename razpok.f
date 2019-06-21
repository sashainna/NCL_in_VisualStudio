c **********************************************************************
c**    NAME           : razpok.f
c**      CONTAINS:
c**        subroutine razpok
c**        subroutine lacdir
c**        subroutine pokinp
c**        subroutine tasetp
c**        subroutine mxcreate
c**        subroutine chkntr
c**        subroutine pokntr
c**        subroutine pokxit
c**
c**    MODULE NAME AND RELEASE LEVEL
c**       razpok.f , 24.7
c**    DATE AND TIME OF LAST MODIFICATION
c**       11/25/13 , 08:41:09
c **********************************************************************
c**
c** copyright (c) 1990 MILLS DATA SYSTEMS CORPORATION
c**
c **********************************************************************
c **********************************************************************
c **  subroutine name:  razpok
c **
c **  purpose of subroutine: Handle Raphi Razi's pocket.
c **     input  :
c **        itsk    - 1 if pocket has a label, 0 if it does not;
c **                  2 if called from watrln
c **
c **********************************************************************
c **********************************************************************

      subroutine razpok (itsk)

      include 'com4a.com'
      include 'vocab.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'

      common/rrfr/ fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov
      real*8 fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov

      common/tacom/ tabot
      real*8 tabot

      common/tamxcom/ tainv(12)
      real*8 tainv

      integer*2 itsk

      integer*2 ierr
      integer*4 kk, nnppr
      real*8 xx,yy,ta0(3)

      real*8 asn, co, dsec, ftmp, comax, tltmax, sc1lev,sc123
      real*8 buf(14), mx(12), mxprev(12),botsv(4),topsv(4),clsv(4),
     x       mxsv(12), sc1pt(3), bufl(6)
      real*4 step,dislev,den,crad,ro,toolrd,toolcr,tlstep,hs,sc23,sc24
      real*4 fstoff(120)
      integer*4 loops, klimb, nclkey, nkey2, keypok, ier
      integer*2 nwds, ietype, ix, jx, istep, lstfl
      integer*2 istrt, iend, idir, ilev, nlevs, iclr,islin

      integer*2 isn(4), isvasw(480)
      equivalence (rsvasw, isvasw)
      equivalence (isn(1), asn)

      real*8 r8jb(36)
      equivalence (jb, r8jb)

      logical trans,tpdst,warnng,ltoppl,lsc1pt
      logical lv823,lv84,lv90,lv91,lv92,lv93,lv94

      integer*2 POCKET
      parameter (POCKET = 22)

      integer*2 izro /0/, i2v3 /3/, i2v4 /4/

      if (ifl(2).gt.0) goto 999
      ier = 0
      ierr = 0
      tol = sc(27)
      if (tol .le. 0.0) tol = .001
      lv823 = sc(169).ge.8.229999d0
      lv84  = sc(169).lt.8.499999d0
      lv90  = sc(169).lt.9.099999d0
      lv91  = sc(169).lt.9.149999d0
      lv92  = sc(169).lt.9.249999d0
      lv93  = sc(169).lt.9.349999d0
      lv94  = sc(169).lt.9.449999d0
      tpdst = .false.
      ltoppl = .false.
      trans = .false.
      lsc1pt = .false.

      islin = 0
      lacefl = 0
      lboth = .false.
      if (lctyp .eq. 1) then
        lacefl = 1
      else if (lctyp .eq. 2) then
        lboth = .true.
        lacefl = 1
      else if (lctyp .eq. 3) then
        lacefl = 2
      else if (lctyp .eq. 4) then
        lboth = .true.
        lacefl = 2
      endif

      bddir = scrdir
      bthk = 0
      sc23 = sc(23)
      sc24 = sc(24)
      sc123 = sc(123)
c
c... set up transformation matrix if out of xy plan
c
      ta0(1) = sc(4)
      ta0(2) = sc(5)
      ta0(3) = sc(6)
      lwatfl = (itsk.eq.2)
      if (tamode .ge. 1) call tasetp (ta0)

      if (.not.lwatfl) then
        sc(23) = psthk
        sc(24) = dsthk
        sc(202) = offthk
      endif      
      tltmax = 1.d0
      if (lv90) tltmax = .999999d0
      if (lv84) tltmax = .9999d0
      if (ta0(3).lt.tltmax) then
        trans = .true.
        mx(9) = ta0(1)
        mx(10) = ta0(2)
        mx(11) = ta0(3)
        if (dabs(ta0(2)).lt..9999d0) then
          mx(1) = ta0(3)
          mx(2) = 0.d0
          mx(3) = -ta0(1)
        else
          mx(1) = 0.d0
          mx(2) = 0.d0
          mx(3) = ta0(2)
        endif
        dsec = dsqrt(mx(1)**2+mx(2)**2+mx(3)**2)
        mx(1) = mx(1)/dsec
        mx(2) = mx(2)/dsec
        mx(3) = mx(3)/dsec
        mx(5) = mx(10)*mx(3) - mx(11)*mx(2)
        mx(6) = mx(11)*mx(1) - mx(9)*mx(3)
        mx(7) = mx(9)*mx(2) - mx(10)*mx(1)
        mx(4) = 0.d0
        mx(8) = 0.d0
        mx(12) = 0.d0
      endif
c
c... Init bottom plane
c
      do k=1,4
        botpl(k) = 0
      enddo
c
c... check bottom for surface or canted plane
c        
      asn = sc(11)
      if (isn(4).eq.SURF) then
        call gtdesc(asn,nclkey,nwds,ietype)
        call ncl_get_sf_primtyp(nclkey,ietype)
        if (ietype.eq.3 .and. tamode .ge. 1) then
            if (.not.trans) isn(4) = PLANE
        else if (ietype.eq.3) then
            isn(4) = PLANE
        endif
      endif
      
      if (isn(4) .eq. surf) then
        sfcut = .true.
      else
        sfcut = .false.
c
c... Get bottom plane
c
        call gtplt (sc(11),izro,botpl)
        co = ta0(1)*botpl(1)+ta0(2)*botpl(2)+ta0(3)*botpl(3)
        comax = 1.d0
        if (lv90) comax = .999999d0
        if (sc(169).lt.8.34999) comax = .999
        if (dabs(co) .lt. comax) sfcut = .true.
        if (trans) call transf(botpl, mx, nwds, PLANE)
        if (botpl(3).lt.0.) then
          if (.not.lv93 .and. botpl(3).eq.-1) botpl(3) = 1
          botpl(4) = -botpl(4)
        endif
      endif
c
c... Check if top is a plane or a distance
c
      if (isc10(2) .eq. 2) then
c
c..... it is a plane
c
        call gtplt (sc(12), izro, toppl)
        co = ta0(1)*toppl(1)+ta0(2)*toppl(2)+ta0(3)*toppl(3)
        ltoppl = (tamode .ge. 1)

        if (dabs(co).lt.0.001 .or.
     x      (tamode.eq.0 .and. dabs(co).lt.0.999)) then
          ifl(2) = 403
          goto 999
        endif
        if (trans) call transf(toppl, mx, nwds, PLANE)
        if (toppl(3).lt.0.) then
          if (ltoppl) then
            toppl(1) = -toppl(1)
            toppl(2) = -toppl(2)
            toppl(3) = -toppl(3)
          else  if (.not.lv93 .and. toppl(3).eq.-1) then
            toppl(3) = 1
          endif
          toppl(4) = -toppl(4)
        endif
      else
c
c..... it is a distance
c
        toppl(4) = sc(12)
        if (lv823) then
          if (sfcut) then
            tpdst = .true.
          else
            toppl(4) = toppl(4) + botpl(4)
          endif
        endif
        toppl(1) = 0.
        toppl(2) = 0.
        toppl(3) = 1.
      endif
c
c... Check if a Z-level value or a PLANE name was passed.
c
      if (cltyp .eq. 0 .or. cltyp.eq.2) then
        clpl(1) = 0.
        clpl(2) = 0.
        clpl(3) = 1.
        clpl(4) = clrlvl+toppl(4)
        iclr = 0
      else
c
c... Get the clearance plane if a label or a key was passed.
c
        call gtclpl(clrlv_lab,clrlv_inx,clpl)
        if (ifl(2) .gt. 0) goto 999
c
c... Check if plane is a pure Z-plane
c
        co = ta0(1)*clpl(1)+ta0(2)*clpl(2)+ta0(3)*clpl(3)
        if (dabs(co) .lt. 0.999) then
          ifl(2) = 403
          goto 999
        endif
        if (trans) call transf(clpl, mx, nwds, PLANE)
        if (clpl(3).lt.0.) clpl(4) = -clpl(4)
        iclr = 1
      endif
c
c... set up and load data for pocket kernel, RRPOCK
c
      call pokinp (ta0,trans,mx,ier)
      if (ifl(2).ne.0 .or. ier.gt.1) goto 999
c
c... max number of cut loops
c
      loops = 9999
      if (nloops.gt.0) loops = nloops
c
c... climb milling assumes a clw rotating, tool moving cclw
c... arround loops, and from the centroid to perimeter of pocket.
c
      klimb = 1
c
c... is tool moving clw around loops
c
      if (pdir .eq. clw) klimb = -klimb
c
c... are the loops from outside in
c
      if (ispirl.eq.in) klimb = -klimb
c
c... Set the "lift between sections" flag.
c
      lifts = 0
c
c...      if (sfcut .or. slift .eq. up) lifts = 1
c
      if (slift .eq. up) lifts = 1
c
c... Set the "Machine corner with arc" flag.
c
      larcs = 0
      if (corner .eq. arc) larcs = 1
c...  optional radius for inside corners
      rcorn = 0
      if (arcrad .gt. 0) rcorn = arcrad
      if (ifl(264).eq.0) rcorn = rcorn*25.4
c...  flag for S-shaped transitions between loops
      ltrarc = 0
      if (transt .eq. arc) ltrarc = 1
c
c... 1st offset = (cutterad + DSTK) x mm/in x IN/OUT from centroid
c
      toolrd = sc(28)/2.
      toolcr = sc(29)

      tlstep = sc(28)-sc(29)*2.
      if (tlstep.lt.tol) tlstep = sc(28)/2.
      if (tlstep.lt.tol) tlstep = tol*500.

      crad = sc(28)/2.+sc(24)
      if (ifl(264).eq.0) crad = crad*25.4
      do 120 jx=1,lanes
120   fstoff(jx) = crad*isvasw(jx*4-1)
c
c... step only set to effective tool flat if maxstep < tol
c
      step = maxstp
      if (step.lt.tol) step = tlstep
      stpmin = minstp
      if (stpmin.gt.step) stpmin = step
      if (stpmin.lt.tol) stpmin = step/2.
c
c... coverage testing parameter
c
      stpcov = toolrd-toolcr
      den = 0
      hs = hscalp
      if (toolcr .gt. tol .and. hs .gt. 0.01*tol) then
        if (hs .ge. toolcr) then
          den = toolcr
        else
          den = 2*toolcr*hs - hs**2
          if (den .le. 0.) then
            den = 0
          else
            den = sqrt(den)
          endif
        endif
      endif

      if (stpcov .lt. tol) then
        stpcov = stpcov + den
        if (stpcov .lt. tlstep) stpcov = tlstep
      else
        stpcov = stpcov + den
      endif

      if (ifl(264).eq.0) then
c
c... convert english to metric
c
        istrt = 1
        iend = lanes
        fact = 25.4
        call scllan (istrt, iend, fact)
        step = step*fact
        stpmin = stpmin*fact
        stpcov = stpcov*fact
        tol = tol*fact
        if (lv823.and.look.eq.1) then
          xlook = xlook*fact
          ylook = ylook*fact
        endif
      endif

      if (lacefl.eq.0) call initpokpols(tol)

c
c... enter kernel pocket
c
      do k=1,4
        botsv(k) = botpl(k)
        topsv(k) = toppl(k)
        clsv(k) = clpl(k)
      enddo
      do k=1,12
        mxsv(k) = mx(k)
      enddo
125   continue
      call rrpock (fstoff, step, loops, klimb, ier)
      if (ier .gt. 1) goto 999

      if (lwatfl .and. lanes.eq.locapo+1) then
        call wentry (loops,ier)
        if (ier .gt. 0) then
          call rrdel
          goto 999
        endif
      endif

      if (ifl(264) .eq. 0) then
c
c... convert metric back to english
c
        istrt = locapo
        iend = lanes
        fact = 1./25.4
        if (istrt.le.0 .or. iend.lt.istrt) then
           ier = 1
           goto 999
        endif
        call scllan (istrt, iend, fact)
        step = step*fact
        stpmin = stpmin*fact
        tol = sc(27)
        if (tol .le. 0.0) tol = .001
      endif
      if (trans) then
c
c... invert xyplan matrix, first saving it as mxprev
c
        do 130 k=1,12
          mxprev(k) = mx(k)
130     continue

        dsec = mx(5)
        mx(5) = mx(2)
        mx(2) = dsec
        dsec = mx(9)
        mx(9) = mx(3)
        mx(3) = dsec
        dsec = mx(10)
        mx(10) = mx(7)
        mx(7) = dsec
      endif
c
c... set up feed rates
c
      fgen = 1.d0
      fmov = 1.d0
      fpos = 2.d0
      fret = 3.d0
      fent = 4.d0
      ftrn = 5.d0
      flst = 6.d0
      ffrs = 7.d0
c
c... set up start end and direction parameter
c
      istrt = locapo
      iend = lanes-1
      idir = 1
      ix0 = 0
      if (ispirl.eq.in) then
        istrt = lanes-1
        iend = locapo
        idir = -1
        lan0 = 0
        do 140 ix=locapo,lanes-1
          if (lane(ix).eq.999) goto 140
          if (look.eq.0 .and. offprt.eq.1 .and. lwatfl) then
            if (loop(ix).eq.1 .and. lane(ix).gt.lan0) then
              ix0 = ix
              lan0 = lane(ix)
            endif
          endif
          kk = loca(ix)
          nnppr = loca(ix+1)-kk
          call rrflip(kk,nnppr,ierr)
          if (ierr .gt. 0) then
            ier = ierr
            goto 999
          endif
  140   continue
      endif
c
c...if lifting between sections, prepare to finish islands last
c
      ilstrt = 0
      if (lifts.eq.1) then
        do 160 ix=locapo,lanes-1
          if (lane(ix).gt.100 .and. lane(ix).lt.999) then
            ilstrt = ix
            if (idir.gt.0) then
              iend = ix-2
            else
              istrt = ix-2
            endif
            goto 170
          endif
  160   continue
      endif
  170 continue

      if (nwrn .le. 0 .and. sc(169).ge.9.549 .and. look.eq.0 .and. 
     x    lacefl.eq.0 .and. offprt.eq.0) then

        do ix=locapo,lanes-1
          ishuff(ix) = 0
          if (ix.gt.locapo .and. lane(ix-1).eq.999 .and.
     x                           lane(ix+1).eq.999) ishuff(ix) = 1
        enddo
      endif

  180 continue
      buf(4) = 0.
      buf(5) = 0.
      buf(6) = 1.

      asn = sc(11)
      if (sfcut .or. (tamode.ge.1 .and. isn(4).eq.SURF)) then
c...
c... surface on pocket bottom, put xyplane pts to surface
c...
        lsc1pt = (.not.lv94 .and. tpdst .and. iclr.eq.0)
        if (lsc1pt) then
c
c..... if both clpl and toppl are defined as a distance, use the starting
c..... point level for the first surface projection
c
          sc1pt(1) = sc(1)
          sc1pt(2) = sc(2)
          sc1pt(3) = sc(3)
          if (trans) call conent(sc1pt, mxprev, i2v3)
          sc1lev = sc1pt(3)
        endif
        call rrpsrf (idir, trans, mx, ltoppl, lsc1pt, sc1lev, ier)
        lsc1pt = .false.
        if (ier .ne. 0) go to 999
        if (tamode .ge. 1) then
          tabot = botpl(2)
          botpl(1) = tol
          botpl(2) = 0
        endif
        botpl(4) = botpl(2)
c       if (tpdst) toppl(4) = toppl(4) + botpl(4)
c
c..... QAR 92361 - clpl also needs to be adjusted if defined incrementally
c
        if (tpdst) then
          toppl(4) = toppl(4) + botpl(4)
          if (cltyp .eq. 0 .or. cltyp.eq.2) clpl(4) = clpl(4) + botpl(4)
        endif
      endif
c
c... Handle number of passes (levels)
c
      dislev = toppl(4)-botpl(4)-sc(23)
      if (numlvl.lt.0.) then
c
c... the NUMLVL parameter is a negative number, that is
c... the number of passes desired.  Figure the actual
c... step down distance between each level.
c
        den = -numlvl
        if (ltoppl .and. dislev.lt.tol) den = 1
      else if (numlvl .eq. 0.) then
c
c... the NUMLVL parameter is zero, use half the
c... cutter diameter to calculate the number of levels.
c
        den = (2.*dislev)/sc(28)
      else if (lvdep .eq. 1) then
c
c..... the flag corresponding to DEPTH after the numlvl parameter -
c..... see FSR 60386; interpreted as "make uniformly distributed passes
c..... the maximum depth given by numlvl
c
        nlevs = dislev/numlvl
        if (nlevs*numlvl .lt. dislev) nlevs = nlevs + 1
      else
c
c... the NUMLVL parameter is a positive number, use
c... that number to figure how many levels can be cut with
c... that much distance between each level.
c
        den = dislev/numlvl
      endif
      if (numlvl.le.0 .or. lvdep.eq.0) nlevs = den + .5
      if (nlevs.lt.1) nlevs = 1
      dislev = dislev/nlevs
c
c... set up clearance plane, display flag, and output step counts
c
      buf(3) = clpl(4)
      buf(4) = 0.
      buf(5) = 0.
      buf(6) = 1.
cc      ifl(270) = -1
      istep = 0
      call pklnew (keypok)
      if (ier .eq. 0) call pokhdr (trans, mx, ier)
      if (ier .ne. 0) goto 999

      if (tamode .ge. 1) then
        bthk = toppl(4)-sc(23)
      endif

c
c... process the output layers from top to bottom
c
      do 300 ilev=1,nlevs
      if (tamode .eq. 0) then
        den = toppl(4)-dislev
        botpl(4) = den
c      botpl(4) = toppl(4)-dislev
      else
        bthk = bthk-dislev
      endif
      do 240 ix=istrt,iend,idir
      if (fmov.ne.ffrs .or. lane(ix-1).ne.999) fmov = fgen
      kk = loca(ix)
      if (lane(ix).eq.999 .or. ix.eq.istrt) then
c
c... lift up or 1st move
c
        jx = ix
        if (lane(ix).eq.999) then
c
c... lift up - move to clpl at retract fedrat
c
          jx = ix - idir
          iclr = cltyp
          if (iclr.eq.2) iclr = 3
c          if (sfcut .and. trans .and. lacefl.gt.0)
c.... qar 95250 - need to transform if not lace also
c
          if (sfcut .and. trans)
     x      call conent(buf, mxprev, i2v3)

            if (tamode .ge. 1) then
              buf(4) = 0
              buf(5) = 0
              buf(6) = 1
            endif
          lstfl = 0
          call pokxit (idir,jx,klimb,istep,iclr,lstfl,buf,trans,mx,ier)
          if (ier .ne. 0) goto 999
          jx = ix + idir
        endif
c
c... move to start point at posn fedrat and do entry
c
        warnng = (entry.eq.OMIT .and. ix.eq.istrt .and. ilev.le.1)
        if (.not. warnng) then

          if (ispirl.eq.in .and. look.eq.0 .and. jx.eq.ix0) look = -1

          if (tamode .ge. 1) then
c
c..... the line below commented because of qar 95189
c            buf(3) = clpl(4)
c            if (iclr.eq.0) buf(3) = clpl(4)
            if (iclr.ne.1) buf(3) = clpl(4)
            buf(4) = 0
            buf(5) = 0
            buf(6) = 1
          endif

          call pokntr(idir,jx,klimb,step,istep,buf,trans,mx,mxsv,ier)
          if (ier .ne. 0) goto 999
          if (ishuff(jx) .eq. 2) then
            ishuff(jx) = 0
            goto 180
          endif
          if (lanes.lt.locapo .and. look.eq.2) then
            do k=1,4
              botpl(k) = botsv(k)
              toppl(k) = topsv(k)
              clpl(k) = clsv(k)
            enddo
            do k=1,12
              mx(k) = mxsv(k)
            enddo
            goto 125
          endif
        endif
c
c...   Store the entry point if this is the last level.
c
        if (ilev.eq.nlevs) then
          call rrget(kk,xx,yy,ier)
          if (ier .gt. 0) goto 999
          buf(1) = xx
          buf(2) = yy
          buf(3) = botpl(4)
          if (sfcut) then
            i = -1
            mtype = 0
            call srfout (kk, i, mtype, buf, trans, mx, ier)
          endif

          if (nwrn .eq. 1) warnng = .false.
          if (warnng) then

            i = 1
            a0 = sc(1)
            b0 = sc(2)
            a1 = xx
            b1 = yy
c
c..... isvasw(4*i-1) is 1,0,-1 if the perimeter condition is in,on,out (with
c..... i = 1); for islands it is 1,0,-1 if the condition is out,on,in
c
215         if (isvasw(4*i-1) .eq. -1) then
              i = i+1
              if (i .ge. islnds + 2) goto 219
              goto 215
            endif
c
c..... fstoff(i) = crad * isvasw(4*i-1)
c
            if (ifl(264).eq.0) then
              ro = fstoff(i) - tol*25.4
            else
              ro = fstoff(i) - tol
            endif
            if (ro .lt. 0.) ro = 0.

            call chkgou (0,i,ro,a0,b0,a1,b1,ierr,ier,0)
            if (ier .ne. 0) goto 999
            if (ierr .ne. 0) then
              ifl(2) = -473
              goto 219
            endif

            i = i + 1
            if (i .lt. islnds + 2) goto 215

          endif

219       continue

          if (tamode .ge. 1) then
            bufl(1) = xx
            bufl(2) = yy
            bufl(3) = botpl(4)
            bufl(4) = 0
            bufl(5) = 0
            bufl(6) = 1
            call conent(bufl, tainv, i2v3)
            call conent(bufl(4), tainv, i2v4)
          else
            do k = 1,6
              bufl(k) = buf(k)
            enddo
          endif

          if (trans) then
            call conent(bufl, mx, i2v3)
            call conent(bufl(4), mx, i2v4)
          endif

          call pklspt (bufl)

        endif
        fmov = ffrs         
        if (lane(ix).eq.999) goto 240
      else
        if (fmov.eq.ffrs .and. lane(ix-1).eq.999) then
c
c.....Comment out to make entry feedrate is respected on all entries
c.....into the pocket
cc          ftmp = ffrs
            ftmp = fent
        else
          ftmp = ftrn
        endif
        if (lv823 .and. loop(ix).eq.1) then
          ftmp = 8.0d0
c
c..... fsr 61085 - final pass feedrate is set one step before the final
c..... pass loop when lace or scrub
c
          if (lacefl.eq.2 .and. loop(ix).eq.1 .and. fhld.eq.flst)
     x       ftmp = flst
        endif
        call pokfed (ftmp, fhld, mtype, ier)
      endif
      if (lv823.and.loop(ix).eq.1) then
        fmov = flst
        if (ilev.eq.nlevs .and. bthk.eq.0 .and. finthk.ne.0) then
          bthk = finthk
        endif
      endif
      mtype = 0

220   call rrget(kk,xx,yy,ier)
      if (ier .gt. 0) goto 999
      if (xx .ge. 0.9*flarc) then

        call pokcir (kk, istep, buf, trans, mx, ier)
        if (ier .ne. 0) goto 999
        kk = kk + 4
        mtype = 0
      else
        buf(1) = xx
        buf(2) = yy
        buf(3) = botpl(4)
        if (sfcut) then
c
c... surface cutting
c
          call srfout (kk, istep, mtype, buf, trans, mx, ier)
        else
          call poksto (buf, mtype, trans, mx, ier)
        endif
        if (ier .ne. 0) goto 999
c
c..... fsr 61085 - set final pass feedrate one step before the final
c..... pass loop when lace or scrub
c
        if (lacefl.eq.2 .and. kk+1.eq.loca(ix+1) .and.
     x                                  loop(ix+1).eq.1) then
                  fmov = flst
        endif

        call pokfed (fmov, fhld, mtype, ier)
        kk = kk + 1
      endif
c
c..... if interrupt switch on, abort motion
c
        call ckintr(ifl(86),ifl(35))
        if(ifl(86).ne.0) then
          ifl(2)=149
          goto 999
        endif
      if (kk.lt.loca(ix+1)) goto 220
240   continue
c
c... move away from wall maybe and retract to clearance pln
c.....Added option to do final retract without going to the
c.....clearance plane - ASF 11/20/13.
c
      if (lwatfl) then
        call nclf_wat_get_lastfl(lstfl)
      else if (ilev.eq.nlevs) then
        lstfl = 1
      else
        lstfl = 0
      endif
      if (rtract.eq.1 .or. ilstrt.gt.0 .or. ilev.lt.nlevs .or.
     x    (rtract.eq.2.and.(ilstr.gt.0.or.ilev.lt.nlevs)) .or.
     x    (lstfl.eq.1.and.ilev.eq.nlevs.and.rtract.eq.2)) then
        iclr = cltyp
        if (iclr.eq.2) then
          if (ilstrt.gt.0) then
            iclr = 3
          else
            if (ilev.eq.nlevs) iclr = 0
          endif
        endif
        ftmp = 9.0d0
        call pokfed (ftmp, fhld, mtype, ier)
c
c..... Eduard 5/7/1999. If sfcut is true, have to retransform the buffer
c..... to the xyplan coords, since it has been transformed by srfout.
c
        if (sfcut .and. trans)  call conent(buf, mxprev, i2v3)
        if (tamode .ge. 1) then
          buf(1) = xx
          buf(2) = yy
          buf(3) = botpl(4)
          buf(4) = 0
          buf(5) = 0
          buf(6) = 1
        endif
        call pokxit (idir,iend,klimb,istep,iclr,lstfl,buf,trans,mx,ier)
        if (ier .ne. 0) goto 999
        if (lv92 .and. sfcut .and. trans)  call conent(buf, mx, i2v3)
      endif
c
c... finish islands if necessary
c
        if (ilev.eq.nlevs .and. fmov.eq.flst .and. finthk.ne.0) then
          bthk = finthk
        endif

      if (ilstrt .gt. 0) then
        do 260 ix = ilstrt,lanes-1
          if (lane(ix).eq.999) goto 260
c
c... check if the islands are outside of the pocket
c
          if (lacefl.eq.0) then
            call islandinside(ix,islin,ier)
            if (islin .eq. 0) goto 260
          endif
                    
c
c... do entry on islands
c
          if (tamode .ge. 1) then
            buf(3) = clpl(4)
            buf(4) = 0
            buf(5) = 0
            buf(6) = 1
          endif

          call pokntr (idir, ix, klimb, step, istep, buf,
     x                 trans, mx, mxsv, ier)
          if (ier .ne. 0) goto 999
          if (ishuff(ix) .eq. 2) then
            ishuff(ix) = 0
            goto 180
          endif
          kk = loca(ix)
          mtype = 0
250       call rrget(kk,xx,yy,ier)
          if (ier .ne. 0) goto 999
          if (xx .ge. 0.9*flarc) then
            call pokcir (kk, istep, buf, trans, mx, ier)
            if (ier .ne. 0) goto 999
            kk = kk+4
            mtype = 0
          else
            buf(1) = xx
            buf(2) = yy
            buf(3) = botpl(4)
            if (sfcut) then
c
c... surface cutting
c
              call srfout (kk, istep, mtype, buf, trans, mx, ier)
            else
              call poksto (buf, mtype, trans, mx, ier)
            endif
            if (ier .ne. 0) goto 999
            call pokfed (fmov, fhld, mtype, ier)
            kk=kk+1
          endif
          call ckintr(ifl(86),ifl(35))
          if(ifl(86).ne.0) then
            ifl(2)=149
            goto 999
          endif
          if (kk.lt.loca(ix+1)) goto 250
c
c... move away from island & floor and retract to clearance pln
c
          if (rtract.eq.1.or.ilstrt.lt.lanes-1.or.ilev.lt.nlevs .or.
     x       (rtract.eq.2.and.(ilstrt.lt.lanes-1.or. 
     x        ilev.lt.nlevs.or.lwatfl))) then
            iclr = cltyp
            if (iclr.eq.2.and.ilstrt.eq.lanes-1.and.ilev.eq.nlevs)
     x         iclr = 0
            ftmp = 9.0d0
            call pokfed (ftmp, fhld, mtype, ier)
            if (sfcut .and. trans .and. lacefl.gt.0)
     x        call conent(buf, mxprev, i2v3)
            if (tamode .ge. 1) then
              buf(4) = 0
              buf(5) = 0
              buf(6) = 1
            endif
            call pokxit(idir,ix,klimb,istep,iclr,lstfl,buf,trans,mx,ier)
            if (ier .ne. 0) go to 999
          endif
260     continue
      endif
      toppl(4) = toppl(4)-dislev
300   continue

      call rrdel
      call pklwrt
c
c... Save the pocket if it was named
c
      if (itsk.eq.1) then
        token2 = savid2
        ivxsub = isvsub
        call vstchk
        if (ityp.eq.2 .and. ist.eq.POCKET) then
          call gtdesc (tv, nkey2, nwds, ietype)
          call pkldel (nkey2)
        endif
        idst = POCKET
        ifl(9) = ifl(11)
        ifl(10) = ifl(12)
        rest = 0.d0
        call ptdesc (keypok,POCKET,rest)
      else
c
c...  Pocket not named, just do it.
c
        call dopock (keypok)
        call pkldel (keypok)
c
c...  Save ending point & feed rate
c
c
c..... Eduard 12/13/1999. The conent call below
c       if (trans) call conent (buf, mx, 3)
c..... was wrong for a pocket with retract,off (FSR 60255).
c..... It is needed only after the call conent(buf, mxprev, i2v3)
c..... made before going into pokxit ("if" bracket after label 240).
c..... Transformation is still needed when pocketing canted plane
c..... - IJD 11Jan01
c
        if (lv92) then
        if (trans .and. .not. sfcut) call conent (buf, mx, 3)
        sc(1) = buf(1)
        sc(2) = buf(2)
        sc(3) = buf(3)
        endif
      endif

999   if (nwrn.eq.-2 .and. ifl(2).eq.-475) ifl(2) = 0
      if (ier .ne. 0) then
        if (lwatfl .and. ier.eq.13) then
          ifl(2) = 13
        else
          call pokerr (ier)
        endif
      endif

      bthk = 0
      call scrfin
      if (.not.lwatfl) call frpkys
      if (lacefl.eq.0) call freepokpols
      call ncl_genpocket_reset

      sc(23) = sc23
      sc(24) = sc24
      sc(123) = sc123

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  lacdir
c **
c **  purpose of subroutine: process lace pocket direction and start point
c **********************************************************************
c **********************************************************************
      subroutine lacdir (xrr,yrr,nrr,trans,mx,diam,ier)

      include 'com.com'
      include 'rrdm.com'
      include 'pokcom.com'

      real*4 xrr(3000),yrr(3000)
      real*8 mx(12),diam

      integer*2 nrr
      integer*4 ier
      logical trans

      real*8 buf(12),d
      real*4 x0,y0,eps,v,u00,u10,u01,u11,u
      real*4 XC,YC,R,X1,Y1,X2,Y2
      integer*4 nclkey
      integer*2 nwds,ietype,ix,klok,in,k
      logical lstart

      eps = sc(27)
c
c..... start point
c
        lstart = .false.
        if (.not.lwatfl .and. sc(14).ne.0) then
          call gtgeom (sc(14), buf, nclkey, nwds, ietype)
          if (ietype.eq.POINT .or. ietype.eq.PNTVEC) then
            if (trans) call transf(buf, mx, nwds, ietype)
            x0 = buf(1)
            y0 = buf(2)
            lstart = .true.
          endif
        endif

        lctflg = .false.
c
c..... direction vector
c
        xlace = 0
        ylace = 0
        if (lcdir .eq. 6) then
          call gtgeom (dirvec, buf, nclkey, nwds, ietype)
          if (ietype .eq. PNTVEC) then
            buf(1) = buf(4)
            buf(2) = buf(5)
            buf(3) = buf(6)
          endif
          if (trans) call transf(buf, mx, nwds, ietype)
          d = buf(1)**2 + buf(2)**2
          if (d .lt. 1.d-8) then
            ier = 13
            return
          endif
          d = sqrt(d)
          xlace = buf(1)/d
          ylace = buf(2)/d
        else if (lcdir.eq.0) then
          xlace = 1
        else if (lcdir.eq.1) then
          xlace = -1
        else if (lcdir.eq.2) then
          ylace = 1
        else if (lcdir.eq.3) then
          ylace = -1
        else
          call lcrect (xrr,yrr,nrr-1,lcdir-3,diam,xlace,ylace,
     x                 vmin,vmax,u00,u10,u01,u11,eps,ier)
          if (ier .gt. 0) return
        endif

        if (xlace .ne. 1.) lctflg = .true.

        ix = 1
        if (lctflg) call lctran (xrr,yrr,nrr,ix)
        if (lcdir.eq.4 .or. lcdir.eq.5) goto 150
c
c..... calculate vmin, vmax, and the u-ranges at vmin and vmax
c
        vmin = yrr(1)
        vmax = vmin
        u00 = xrr(1)
        u01 = u00
        u10 = u00
        u11 = u00
        k = 1
 100    k = k+1
        if (xrr(k) .eq. flarc) then
          r = xrr(k+1)
          klok = yrr(k+1)
          xc = xrr(k+2)
          yc = yrr(k+2)
          x1 = xrr(k-1)
          y1 = yrr(k-1)
          x2 = xrr(k+3)
          y2 = yrr(k+3)
          u = xc
          v = yc + r
          CALL INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,u,v,IN)
          if (in .eq. 1) call lcmmx (u,v,u00,u10,u01,u11,eps)
          v = yc - r
          CALL INARC (XC,YC,R,KLOK,X1,Y1,X2,Y2,u,v,IN)
          if (in .eq. 1) call lcmmx (u,v,u00,u10,u01,u11,eps)
          k = k+2
        else
          u = xrr(k)
          v = yrr(k)
          call lcmmx (u,v,u00,u10,u01,u11,eps)
        endif
        if (k .lt. nrr) goto 100
c
c..... if starting point, determine if we go up or down, and whether
c..... the direction needs to be reversed
c
 150    if (lstart) then
          call lctrans (x0,y0,1)
          if (abs(y0 - vmin) .gt. abs(y0 - vmax)) then
            v = vmin
            vmin = vmax
            vmax = v
          endif
          if (lacefl.eq.2 .or. lcdir.eq.4 .or. lcdir.eq.5) then
c
c..... if the direction is LONG or SHORT, reverse the vector if
c..... it results in a start closer to the starting point
c
            if ((vmin.le.vmax .and.
     x          abs(x0 - u00) .gt. abs(x0 - u10)) .or.
     x          (vmin.gt.vmax .and.
     x          abs(x0 - u01) .gt. abs(x0 - u11))) then

              xlace = -xlace
              ylace = -ylace
              lctflg = (xlace .ne. 1.)

              k = 0
 200          k = k+1
              if (xrr(k) .ne. flarc) then
                xrr(k) = -xrr(k)
              else
                k = k+1
              endif
              if (k .lt. nrr) goto 200
            endif
          endif
c        else if (lacefl.eq.1 .and. (xlace+ylace).lt.0) then
        else if ((xlace+ylace).lt.0) then
          v = vmin
          vmin = vmax
          vmax = v
        endif

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  tasetp (ta0)
c **  purpose of subroutine: calculate the average perimeter plane.
c **    input:
c **        ta0    - current tool axis
c **    output:
c **        ta0    - the plane vector, in same general direction as input
c **                 in case of errors the vector is returned unchanged
c **********************************************************************
c **********************************************************************

      subroutine tasetp (ta0)

      include 'com.com'
      include 'rrdm.com'
      include 'wrksys.com'
      include 'pokcom.com'

      real*8 ta0(3)

      real*8 asn, co, tol8, dd, asn1
      real*8 f_dist
      real*8 buf(14), endpt(3)
      integer*2 ksn(4),isvasw(480)
      equivalence (asn,ksn),(rsvasw,isvasw)

      integer*4 nclkey, pntyp, sfkey
      integer*2 nwds, ietype, ix, j, k
      integer*2 npts,irev,ier
      integer*2 i2v3 /3/
      integer*2 lstored
      
      logical lv97
      lv97 = sc(169).lt.9.749d0
      sfkey = 0

        ier = 0

        ix = 1
        asn = rsvasw(ix)
        if (ksn(4).eq.14) then
c
c.....                                                Point array
c
          token2 = rsvtok(ix)
          ivxsub = rsvsub(ix)
          do 20 j=1,ksn(1)
            ivxsub = j
            call vstchk
            if (ityp.ne.2 .or. ist.ne.3) then
              if (j.gt.1 .and. ityp.eq.2 .and. ist.eq.1) goto 20
              ier = 177
              goto 999
            endif
            call gtgeom (tv, buf, nclkey, nwds, ietype)
            call ncl_push_uv (buf)
            if (j.eq.1) call vctovc (buf,endpt)
20        continue

          dd = f_dist (buf,endpt)
          if (dd .gt. sc(27)) call ncl_push_uv (endpt)
        else if (ksn(4).eq.20 .or. ksn(4).eq.21) then
c
c.....                                                Patern or Surface Boundary
c
          if (ksn(4).eq.20) then
c
c.....                                                Patern
c
            call gtdesc(asn, nclkey, nwds, ietype)
            call gtpnnp (nclkey, nwds, pntyp)
          else if (ksn(4).eq.21) then
c
c.....                                                Surface Boundary
c
            if (lwatfl) then
c
c..... Plane already calculated and stored
c
              call gtpln (ix,buf,lstored)
              if (lstored .eq. 1) goto 80
            endif

            call gtnpt (ix,nwds,ier)
            if (ier .ne. 0) goto 999
          endif

          j = 1
          do 40 while (j .le. nwds)
            if (nwds .gt. 3000) then
              ier = 8
              goto 999
            endif
            if (ksn(4).eq.20) then
              call gpnptt (buf, nclkey, j, .false.)
            else if (ksn(4).eq.21) then
              call gtpts (buf,ix,j,ier)
              if (ier .ne. 0) goto 999
            endif
            call ncl_push_uv (buf)
            if (j.eq.1) call vctovc (buf,endpt)
40        j = j + 1

          dd = f_dist (buf,endpt)
          if (dd .gt. sc(27)) call ncl_push_uv (endpt)
        else if (ksn(4).eq.7) then
c
c.....                                                Circle
c
          call gtgeom (asn, buf, nclkey, nwds, ietype)
          if (buf(8).ne.0. .or. buf(9).ne.0. .or. buf(10).ne.0.) then
            ier = 177
            goto 999
          endif
          goto 80
        else
c
c.....                                                Curve
c
          tol8 = sc(27)
          if (ifl(264).eq.1) tol8 = tol8/25.4d0
          call gtdesc(asn, nclkey, nwds, ietype)
          irev = 0
          call cptint (nclkey, tol8, irev, npts)
          do 50 k=1,npts
            call cptnxt (buf, buf(4))
c           if (k.eq.1.and.j.gt.1) goto 50
            if (lwrk) call conent (buf, invwrk, i2v3)
            if (ifl(264).eq.1) then
              buf(1)=buf(1)*25.4d0
              buf(2)=buf(2)*25.4d0
              buf(3)=buf(3)*25.4d0
            endif
            call ncl_push_uv (buf)
50        continue
          call cptend
        endif

        call ncl_getn_uv (endpt,np)
70      if (np .gt. 3000) then
          ier = 8
          goto 999
        endif

        if (.not.lv97 .and. ksn(4).eq.21) then
            asn1= sc(11)
            call gtdesc(asn1, sfkey, nwds, ietype)
        endif
        
        call pts2pl (sfkey,np,buf,sc(27),ier)
        if (ier .ne. 0) goto 999
        if (.not. lv97) then
            call from_unibase (buf,buf,POINT)
            call from_unibase (buf(4),buf(4),VECTOR)
        endif

80      co = ta0(1)*buf(4)+ta0(2)*buf(5)+ta0(3)*buf(6)
        if (co .ge. 0) then
          ta0(1) = buf(4)
          ta0(2) = buf(5)
          ta0(3) = buf(6)
        else
          ta0(1) = -buf(4)
          ta0(2) = -buf(5)
          ta0(3) = -buf(6)
        endif

999   if (.not.lwatfl) call ncl_free_uv
      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokinp
c **
c **  purpose of subroutine: input geometry enties into the parameters
c **                         and arrays of pocket common.
c **
c **********************************************************************
c **********************************************************************

      subroutine pokinp (ta0,trans,mx,ier4)

      include 'com.com'
      include 'const.com'
      include 'rrdm.com'
      include 'wrksys.com'
      include 'pokcom.com'

      logical trans
      real*8 ta0(3),mx(12)
      integer*4 ier4

      real*4 xrr(3000), yrr(3000)
      real*4 vx,vy,ccw,ang,coa,sia,zbot

      real*8 asn, co, buf(14), endx, endy, tol8, xx, yy
      integer*2 ksn(4),isvasw(480),nrr
      equivalence (asn,ksn),(rsvasw,isvasw)

      integer*4 nclkey, keyout, kk, pntyp
      integer*2 nwds, ietype, ix, j, k, l, ilane, ixxy, nents, nppr
      integer*2 lix,npts,irev,ier,jcomp,lcirc,jnext
      logical lptneq, lv94
      integer*2 i2v3 /3/

      integer*2 DEBUGX
      parameter (DEBUGX=0)

c...
c... set up and load data for kernel pocket
c...
      do 10 ix=1,mxln
        loca(ix) = 0
        lane(ix) = 0
        loop(ix) = 0
        mama(ix) = 0
10    continue
      flarc = 1.e25
      islnds = -1
      ixxy = 1
      ilane = 1
      loca(1) = 1
      lanes = isc10(3)
      lcirc = 0
      jnext = 0

      lv94 = sc(169).lt.9.449d0

      look = 0
      xlook = 0.
      ylook = 0.
      ulook = 0.
      lix = 0
      if (isc10(4).eq.1) then
        lix = sc(13)
      else if (isc10(4).eq.2) then
        call gtgeom (sc(13), buf, nclkey, nwds, ietype)
        if (trans) call transf(buf, mx, nwds, ietype)
        xlook = buf(1)
        ylook = buf(2)
        look = 1
      endif

      if (look.eq.0 .and. offprt.eq.1 .and. .not.lwatfl .and.
     x    .not.openfl) then
        look = 1
        xlook = sc(1)
        ylook = sc(2)
      endif

      do 100 ix=1,lanes
        ixxyst = ixxy
        asn = rsvasw(ix)
        if (ksn(4).eq.14) then
c                                                Point array
          token2 = rsvtok(ix)
          ivxsub = rsvsub(ix)
          do 20 j=1,ksn(1)
            ivxsub = j
            call vstchk
            if (ityp.ne.2 .or. ist.ne.3) then
              if (j.gt.1 .and. ityp.eq.2 .and. ist.eq.1) goto 20
              ifl(2) = 177
              call expnm2 (token2, ivxsub, j, errcom)
              goto 999
            endif
            call gtgeom (tv, buf, nclkey, nwds, ietype)
            if (trans) call transf(buf, mx, nwds, ietype)
            xrr(ixxy) = buf(1)
            yrr(ixxy) = buf(2)
            if (j.eq.1) then
              endx = xrr(ixxy)
              endy = yrr(ixxy)
            endif
            ixxy = ixxy+1
            if (ix.eq.1) then
              if (lix.gt.0 .and. lix.eq.j-1) then
                xlook = (xrr(ixxy-1)+xrr(ixxy-2))/2.
                ylook = (yrr(ixxy-1)+yrr(ixxy-2))/2.
                look = 1
                lix = 0
              endif
            endif
20        continue
          if (sc(169).lt.8.29999) then
            lptneq = xrr(ixxy-1) .ne. endx .or. yrr(ixxy-1) .ne. endy
          else
            lptneq = abs(xrr(ixxy-1)-endx).gt.sc(27)
     1          .or. abs(yrr(ixxy-1)-endy).gt.sc(27)
          endif
          if (lptneq) then
            xrr(ixxy) = endx
            yrr(ixxy) = endy
            ixxy = ixxy+1
          endif
        else if (ksn(4).eq.20 .or. ksn(4).eq.21 .or. lwatfl) then
c                Patern        or  Boundary      or  Waterline Polygon
          if (ksn(4).eq.20) then
c                                                Patern
            call gtdesc(asn, nclkey, nwds, ietype)
            call gtpnnp (nclkey, nwds, pntyp)
          else if (ksn(4).eq.21) then
c                  Boundary (could be stored by a waterline algorithm)
            call gtnpt (ix,nwds,ier)
            if (ier .ne. 0) then
              ifl(2) = 177
              goto 999
            endif
          else
c                  Waterline Polygon
            call wgtnpt (ix,nwds,lcirc,ier)
            if (ier .ne. 0) then
              ifl(2) = 177
              goto 999
            endif
            if (lcirc .eq. 1) then
              call wgtci (buf,ix,ier)
              if (ier .ne. 0) then
                ifl(2) = 177
                goto 999
              endif
c              if (trans) call conent(buf, mx, 3)
              ccw = 1.
              call arcinp(xrr,yrr,ixxy,buf(1)+buf(7),buf(2),buf(7),ccw,
     x                buf(1),buf(2))
              call arcinp(xrr,yrr,ixxy,buf(1),buf(2)+buf(7),buf(7),ccw,
     x                buf(1),buf(2))
              call arcinp(xrr,yrr,ixxy,buf(1)-buf(7),buf(2),buf(7),ccw,
     x                buf(1),buf(2))
              call arcinp(xrr,yrr,ixxy,buf(1),buf(2)-buf(7),buf(7),ccw,
     x                buf(1),buf(2))
              xrr(ixxy) = buf(1)+buf(7)
              yrr(ixxy) = buf(2)
              ixxy = ixxy+1

              if (ix.eq.1) then
                if (lix.gt.0 .and. lix.lt.5) then
                  nppr = ixxy-(5-lix)*4-1
                  call midarc (xrr,yrr,nppr,xlook,ylook)
                  look = 1
                  lix = 0
                endif
              endif
              goto 70
            endif
          endif
          j = 1
          do 40 while (j .le. nwds)
            if (ixxy .gt. 3000) then
              ier4 = 8
              goto 999
            endif
            if (ksn(4).eq.20) then
              call gpnptt (buf, nclkey, j, .false.)
            else if (ksn(4).eq.21) then
              call gtpts (buf,ix,j,ier)
              if (ier .ne. 0) then
                ifl(2) = 177
                goto 999
              endif
              if (lwatfl .and. trans) call conent(buf, mx, 3)
            else
              call wgtpt (buf,ix,j,ier,lcirc,jnext)
              if (ier .ne. 0) then
                ifl(2) = 177
                goto 999
              endif
              if (lcirc .eq. -1) then
                ixxy = ixxy-1
                lcirc = 1
              endif
            endif
            if (.not.lwatfl .and. trans) call conent(buf, mx, 3)
            xrr(ixxy) = buf(1)
            yrr(ixxy) = buf(2)
            if (j.eq.1) then
              endx = xrr(ixxy)
              endy = yrr(ixxy)
            endif
            ixxy = ixxy+1
            if (ix.eq.1) then
              if (lix.gt.0 .and. lix.eq.j-1) then
                xlook = (xrr(ixxy-1)+xrr(ixxy-2))/2.
                ylook = (yrr(ixxy-1)+yrr(ixxy-2))/2.
                look = 1
                lix = 0
              endif
            endif
            if (lwatfl .and. lcirc.eq.1) then
              xrr(ixxy) = flarc
              yrr(ixxy) = 0.
              xrr(ixxy+1) = buf(4)
              yrr(ixxy+1) = buf(5)
c              if (trans) call conent(buf(6), mx, 3)
              xrr(ixxy+2) = buf(6)
              yrr(ixxy+2) = buf(7)
c              if (trans) call conent(buf(9), mx, 3)
              xrr(ixxy+3) = buf(9)
              yrr(ixxy+3) = buf(10)
              ixxy = ixxy + 4
              j = jnext
            endif
40        j = j + 1
          if (sc(169).lt.8.29999) then
            lptneq = xrr(ixxy-1) .ne. endx .or. yrr(ixxy-1) .ne. endy
          else
            lptneq = abs(xrr(ixxy-1)-endx).gt.sc(27)
     1          .or. abs(yrr(ixxy-1)-endy).gt.sc(27)
          endif
          if (lptneq) then
            xrr(ixxy) = endx
            yrr(ixxy) = endy
            ixxy = ixxy+1
          endif
        else if (ksn(4).eq.7) then
c                                                Circle
          call gtgeom (asn, buf, nclkey, nwds, ietype)
          if (buf(8).ne.0. .or. buf(9).ne.0. .or. buf(10).ne.0.) then
            ifl(2) = 177
            goto 999
          endif
          co = ta0(1)*buf(4)+ta0(2)*buf(5)+ta0(3)*buf(6)
          if (dabs(co) .lt. 0.999) then
            ifl(2) = 177
            goto 999
          endif
          if (trans) call transf(buf, mx, nwds, ietype)
          ccw = 1.
          call arcinp(xrr,yrr,ixxy,buf(1)+buf(7),buf(2),buf(7),ccw,
     x                buf(1),buf(2))
          call arcinp(xrr,yrr,ixxy,buf(1),buf(2)+buf(7),buf(7),ccw,
     x                buf(1),buf(2))
          call arcinp(xrr,yrr,ixxy,buf(1)-buf(7),buf(2),buf(7),ccw,
     x                buf(1),buf(2))
          call arcinp(xrr,yrr,ixxy,buf(1),buf(2)-buf(7),buf(7),ccw,
     x                buf(1),buf(2))
          xrr(ixxy) = buf(1)+buf(7)
          yrr(ixxy) = buf(2)
          ixxy = ixxy+1
          if (ix.eq.1) then
            if (lix.gt.0 .and. lix.lt.5) then
              nppr = ixxy-(5-lix)*4-1
              call midarc (xrr,yrr,nppr,xlook,ylook)
              look = 1
              lix = 0
            endif
          endif
        else if (ksn(4).eq.9.or.ksn(4).eq.10) then
c                             OPEN OR OFFSET SF BOUNDARY - ASF 8/29/13
          if (ksn(4).eq.9) then
            call gtdesc(asn, nclkey, nwds, ietype)
            call nclf_genpocket_bound(nclkey,ta0,ix,npts,ier)
          else
            call nclf_genpocket_open(ta0,npts,ier)
          endif
          if (npts.eq.0.and.ier.eq.0) then
            isc10(3) = isc10(3) - 1
            if (isc10(3).lt.1) then
              ifl(2) = 177
              goto 999
            endif
            lanes = isc10(3)
            goto 100
          else if (ier.ne.0) then
            ifl(2) = 177
            goto 999
          endif
          do 80 j=1,npts
            if (ixxy .gt. 3000) then
              ier4 = 8
              goto 999
            endif
            call nclf_genpocket_getpt(j,buf)
            if (lwrk) call conent (buf, invwrk, i2v3)
            if (ifl(264).eq.1) then
              buf(1)=buf(1)*25.4d0
              buf(2)=buf(2)*25.4d0
              buf(3)=buf(3)*25.4d0
            endif
            if (trans) call conent(buf, mx, i2v3)
            xrr(ixxy) = buf(1)
            yrr(ixxy) = buf(2)
            ixxy = ixxy + 1
80        continue
          call nclf_clear_ptlst
        else if (ksn(4).eq.11) then
c                             PMILL POINTS - ASF 8/29/13
          call nclf_pmill_npts(npts)
          if (npts.eq.0) then
            isc10(3) = isc10(3) - 1
            if (isc10(3).lt.1) then
              ifl(2) = 177
              goto 999
            endif
            lanes = isc10(3)
            goto 100
          endif
          do 90 j=1,npts
            if (ixxy .gt. 3000) then
              ier4 = 8
              goto 999
            endif
            call nclf_pmill_getpt(j,buf)
            if (lwrk) call conent (buf, invwrk, i2v3)
            if (ifl(264).eq.1) then
              buf(1)=buf(1)*25.4d0
              buf(2)=buf(2)*25.4d0
              buf(3)=buf(3)*25.4d0
            endif
            if (trans) call conent(buf, mx, i2v3)
            xrr(ixxy) = buf(1)
            yrr(ixxy) = buf(2)
            ixxy = ixxy + 1
90        continue
        else
c                                                Composite curve
          call gtdesc(asn, nclkey, nwds, ietype)
          call iscmpc (nclkey, jcomp)
          if (jcomp .eq. 0) then
            nents = 1
          else
            call gtccnm (nclkey, nents)
          endif
          do 60 j=1,nents
            if (jcomp .eq. 0) then
              keyout = nclkey
            else
              call gtcntt (nclkey, j, keyout, buf, ietype, 1, ifl(2))
              if (ifl(2).ne.0) then
                ifl(2) = 177
                call expnam (asn, l, errcom)
                write (errcom(l+1:),1005) j
1005            format (' #',i4)
                goto 999
              endif
              if (ietype.eq.CIRCLE .and. .not.lv94) then
                co = ta0(1)*buf(10)+ta0(2)*buf(11)+ta0(3)*buf(12)
c
c..... if the circle is tilted evolve it as a curve
c
                if (dabs(co) .lt. 0.999) then
                  call gtcccv (nclkey, j, keyout, ifl(2))
                  if (ifl(2).ne.0) then
                    ifl(2) = 177
                    call expnam (asn, l, errcom)
                    write (errcom(l+1:),1005) j
                    goto 999
                  endif
                  ietype = CURVE
                endif
              endif
            endif
            if (ietype.eq.CURVE) then
              tol8 = sc(27)
              if (ifl(264).eq.1) tol8 = tol8/25.4d0
              irev = 0
              if (keyout.lt.0) then
                irev = -1
                keyout = -keyout
              endif
              call cptint (keyout, tol8, irev, npts)
              do 50 k=1,npts
                call cptnxt (buf, buf(4))
                if (k.eq.1.and.j.gt.1) goto 50
                if (lwrk) call conent (buf, invwrk, i2v3)
                if (ifl(264).eq.1) then
                  buf(1)=buf(1)*25.4d0
                  buf(2)=buf(2)*25.4d0
                  buf(3)=buf(3)*25.4d0
                endif
                if (trans) call conent(buf, mx, i2v3)
                xrr(ixxy) = buf(1)
                yrr(ixxy) = buf(2)
                ixxy = ixxy+1
50            continue
              call cptend
            else
              if (trans) call trncnt(buf, mx, ietype)
              if (j.eq.1) then
                xrr(ixxy) = buf(1)
                yrr(ixxy) = buf(2)
                ixxy = ixxy+1
              endif
              if (ietype.eq.LINE) then
                xrr(ixxy) = buf(4)
                yrr(ixxy) = buf(5)
                ixxy = ixxy+1
              else if (ietype.eq.CIRCLE) then
                xrr(ixxy) = flarc
                yrr(ixxy) = 0.
                ixxy = ixxy+1
                xrr(ixxy) = buf(13)
                yrr(ixxy) = 1.
                if (buf(12)*buf(14).lt.0.) yrr(ixxy) = -1.
                ccw = yrr(ixxy)
                ixxy = ixxy+1
                xrr(ixxy) = buf(4)
                yrr(ixxy) = buf(5)
                ixxy = ixxy+1
c
c..... if an arc is more than 90 degrees insert some points. FSR 60504
c
                npts = 1
                ang = dabs (buf(14))
                if (ang .gt. HALF_PI) npts = npts + 1
                if (ang .gt. PI) npts = npts + 1
                if (ang .gt. PI_32) npts = npts + 1
                if (npts .gt. 1) then
                  ang = ang/npts
                  if (ccw .lt. 0) ang = -ang
                  coa = cos(ang)
                  sia = sin(ang)
                  vx = buf(1) - buf(4)
                  vy = buf(2) - buf(5)
                  do k = 2, npts
                    endx = buf(4) + coa*vx - sia*vy
                    endy = buf(5) + sia*vx + coa*vy
                    call arcinp(xrr,yrr,ixxy,endx,endy,buf(13),ccw,
     x                buf(4),buf(5))
                    vx = endx - buf(4)
                    vy = endy - buf(5)
                  enddo
                endif
                xrr(ixxy) = buf(7)
                yrr(ixxy) = buf(8)
                ixxy = ixxy+1
              endif
            endif
            if (ix.eq.1) then
              if (lix.eq.j) then
                if (xrr(ixxy-4).eq.flarc) then
                  nppr = ixxy-5
                  call midarc (xrr,yrr,nppr,xlook,ylook)
                else
                  xlook = (xrr(ixxy-1)+xrr(ixxy-2))/2.
                  ylook = (yrr(ixxy-1)+yrr(ixxy-2))/2.
                endif
                look = 1
                lix = 0
              endif
            endif
60        continue
        endif
70      if (ixxy .gt. 3000) then
          ier4 = 8
          goto 999
        endif
        if (lix.gt.0) then
          if (xrr(ixxy-4).eq.flarc) then
            nppr = ixxy-5
            call midarc (xrr,yrr,nppr,xlook,ylook)
          else
            xlook = (xrr(ixxy-1)+xrr(ixxy-2))/2.
            ylook = (yrr(ixxy-1)+yrr(ixxy-2))/2.
          endif
          look = 1
          lix = 0
        endif
        lane(ix) = ilane
        if (ix .eq. 1) then
          ilane = 100
          if (lacefl.gt.0) then
            call lacdir (xrr,yrr,ixxy-1,trans,mx,sc(28),ier4)
            if (ier4.gt.0) goto 999
          endif
        else if (lacefl.gt.0 .and. lctflg) then
          nrr = ixxy-loca(ix)
          call lctran (xrr,yrr,nrr,ix)
        endif
        ilane = ilane+1
        loop(ix) = 0
        mama(ix) = 0
        loca(ix+1) = ixxy
        islnds = islnds+1
        zbot = buf(3)
        if (DEBUGX .ne. 0) then
          iclsv = 0
          if (islnds .eq. 0) then
            call pokdbg (xrr(ixxyst),yrr(ixxyst),zbot,ixxy,1,0,0,
     1                   "GREEN")
          else
            call pokdbg (xrr(ixxyst),yrr(ixxyst),zbot,ixxy,0,0,1,
     1                   "MAGNTA")
          endif
      endif
100   continue
c...
c... load data from xrr,yrr into a linked list
c...
      call rrini

      do 200 kk=1,loca(lanes+1)-1
           ixxy = kk
           xx = xrr(ixxy)
           yy = yrr(ixxy)
           call rrput(kk,xx,yy,ier)
           if (ier .gt. 0) then
               ier4 = 1
               goto 999
           endif
200   continue

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  mxcreate (buf,xt,yt,zt,tamx)
c **
c **  purpose of subroutine: create an orthogonal transformation matrix that
c **  takes z-axis to the tool-axis vector buf(4:6).
c **
c **********************************************************************
c **********************************************************************

      subroutine mxcreate (buf,xt,yt,zt)

      real*8 buf(6),xt,yt,zt

      common/tamxcom/ tainv(12)
      real*8 tainv

      integer*2 i,k
      real*8 tamx(12),dsec

      tamx(9) = buf(4)
      tamx(10) = buf(5)
      tamx(11) = buf(6)
      if (dabs(buf(5)).lt..9999d0) then
        tamx(1) = buf(6)
        tamx(2) = 0.d0
        tamx(3) = -buf(4)
      else
        tamx(1) = 0.d0
        tamx(2) = 0.d0
        tamx(3) = buf(5)
      endif
      dsec = dsqrt(tamx(1)**2+tamx(2)**2+tamx(3)**2)
      tamx(1) = tamx(1)/dsec
      tamx(2) = tamx(2)/dsec
      tamx(3) = tamx(3)/dsec
      tamx(5) = tamx(10)*tamx(3) - tamx(11)*tamx(2)
      tamx(6) = tamx(11)*tamx(1) - tamx(9)*tamx(3)
      tamx(7) = tamx(9)*tamx(2) - tamx(10)*tamx(1)

      tamx(4) = xt
      tamx(8) = yt
      tamx(12) = zt

      do k=4,12,4
        i=k-3
      tamx(k)=tamx(k)-tamx(i)*buf(1)-tamx(i+1)*buf(2)-tamx(i+2)*buf(3)
      enddo

      tainv(1) = tamx(1)
      tainv(6) = tamx(6)
      tainv(11) = tamx(11)

      tainv(2) = tamx(5)
      tainv(5) = tamx(2)

      tainv(3) = tamx(9)
      tainv(9) = tamx(3)

      tainv(7) = tamx(10)
      tainv(10) = tamx(7)

      do k=4,12,4
        i=k-3
      tainv(k)=-tainv(i)*tamx(4)-tainv(i+1)*tamx(8)-tainv(i+2)*tamx(12)
      enddo

      return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  chkntr (ntry,wall,xt,yt,rad,fact,ier)
c **
c **  purpose of subroutine: check for geometry gouging at entry, if
c **                         found warn or try to avoid
c **
c **********************************************************************
c **********************************************************************

      subroutine chkntr (ix,ntry,wall,xt,yt,step,dis,rad,fact,ier,
     x                   jshuff)

      include 'com4a.com'
      include 'vocab.com'
      include 'rrdm.com'
      include 'pokcom.com'

      logical wall
      integer*2 ntry,ix,jshuff
      integer*4 ier
      real*4 xt(2),yt(2),step,dis,rad,fact

      integer*2 isvasw(480)
      equivalence (rsvasw, isvasw)

      real*8 crad
      real*4 tolm,a0,b0,a1,b1,ro
      integer*2 i,jtry,ientry,jcut

        jtry = 0
c
c..... the gouge checking is done in MM
c
        tolm = tol*fact
        rad = rad*fact

        crad = (sc(28)/2.+sc(24))*fact
        ientry = 0
        if (ntry .eq. helix) ientry = 1

2210    i = 1
        jcut = 0

        a0 = xt(2)*fact
        b0 = yt(2)*fact
        if (ntry .eq. ramp) then
          a1 = xt(1)*fact
          b1 = yt(1)*fact
        endif
c
c..... isvasw(4*i-1) is 1,0,-1 if the perimeter condition is in,on,out (with
c..... i = 1); for islands it is 1,0,-1 if the condition is out,on,in
c..... For now, we do not check the perimeter gouging with the OUT condition,
c..... or an island gouging with the IN condition.
c
2215    if (isvasw(4*i-1) .eq. -1) then
          i = i+1
          if (i .ge. islnds + 2) goto 2219
          goto 2215
        endif

        if (ntry .eq. helix) then
          ro = rad + crad*isvasw(4*i-1) - tolm
        else
          ro = crad*isvasw(4*i-1) - tolm
        endif
        if (ro .lt. 0.) ro = 0.

        call chkgou (ientry,i,ro,a0,b0,a1,b1,jcut,ier,0)
        if (ier .ne. 0) goto 999

        if (jcut .eq. 1) then

          if (ntry.eq.ramp .and. jshuff.eq.1) then
            call flook1 (ix,xt,yt,dis,jshuff,ier)
            if (jshuff.eq.2 .and. ier.eq.0) goto 999
          endif

          if (nwrn .eq. 0) then
            ifl(2) = -473
            goto 2219
          endif
          if (nwrn .le. -1) then
            if (sc(169).ge.9.349d0 .and. look.eq.0 .and. ntry.eq.helix)
     x                                                             then
c
c..... try to find a good starting point, redo the pocketing if found
c
              call flook(sid,ix,rad,crad,tol,tolm,ier)
              if (ier.eq.0 .and. look.eq.2) then
                step = step*fact
                stpmin = stpmin*fact
                tol = tol*fact
                goto 999
              endif
            endif
            if (jtry .gt. 50) then
              ifl(2) = 473
              ier = 7
              goto 999
            endif
            jtry = jtry + 1
            wall = .true.
c
c.....Fixed scaling of dis to match radius scaling - ASF 9/27/13
c
            dis = dis * 0.9
            if (rmpdis .gt. dis) ifl(2) = -475
            xt(2) = xt(1) + 0.9*(xt(2) - xt(1))
            yt(2) = yt(1) + 0.9*(yt(2) - yt(1))
            rad = rad*0.9
            goto 2210
          endif
        endif

        i = i + 1
        if (i .lt. islnds + 2) goto 2215

2219    rad = rad/fact

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokntr
c **
c **  purpose of subroutine: to perform all the necessary motions and
c **                         feed rates for entries into the pocket.
c **                           ier = 4 - errors from surftl
c **
c **********************************************************************
c **********************************************************************

      subroutine pokntr (idir, ix, klimb, step, istep, buf,
     x                   trans, mx, mxsv, ier)

      include 'com4a.com'
      include 'vocab.com'
      include 'const.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'
      include 'wrksys.com'

      common/rrfr/ fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov
      real*8 fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov
      common/tacom/ tabot
      real*8 tabot
      common/tamxcom/ tainv(12)
      real*8 tainv

      integer*2 idir, ix, istep
      integer*4 klimb, ier
      logical trans
      real*4 step
      real*8 buf(14), mx(12), mxsv(12), mvec(3), tbuf(3)

      real*8 znow,ztop,zbot,xx,yy,xx2,yy2 !,dis
      real*4 sid, xt(2), yt(2), dang, zdis,xpt,ypt,arad,crad
      real*4 vx,vy,rad,ang,zdpt,vtmp,cx,cy,ro,fact,dis
      integer*4 itp, ind, kk, kk2
      integer*2 i,iramps,ierr,jcut,ixt,npts,ntry,mtype,jshuff,tntry
      integer*2 helxfl,itran,indir,iwrk
      logical wall,botok,toolow,tanto,lcoupl,lcycle,lfla,lislrc
      integer*2 idx
      real*8 ver
      logical lv101
      
      idx = 169
      call getsc(idx,ver)
      lv101 = ver.gt.10.049

      ierr = 0
      fact = 1.
      if (ifl(264).eq.0) fact = 25.4

      jshuff = ishuff(ix)

      mtype = 0
      if (lane(ix) .eq. 999) goto 999
      kk = loca(ix)
      kk2 = kk + 1
      call rrget(kk,xx,yy,ier)
      if (ier .ne. 0) goto 999
      xpt = xx
      ypt = yy
c
c..... take into account possible island pass in CCW direction
c
      if (lacefl.gt.0 .and. lane(ix).gt.200) then
        lane(ix) = lane(ix) - 100
        klimb = -1
      endif

      ntry = entry
      tntry = ntry
      if (offprt.eq.1) then
        znow = buf(3)
c
c.....Enter off part through open side or waterline with flat helix motion.
c
        crad = sc(28)/2.+sc(24)
c        if (openfl.eq.1.or.ntry.eq.helix) then
c
c.....Added version flag so previous style entry can be used
c.....if desired - ASF 11/20/13.
c
        if (openfl.eq.1.or.(lwatfl.and.lv101)) then
          if (lctyp .gt. 0) ntry = plunge
          if (ntry .lt. 0) then
c        if (lane(ix).le.100 .or. lacefl.eq.0)
          if (lacefl.eq.0)
     x      tanto = .true.
            ntry = -ntry
          endif
          if (rmpdis.gt.0.0) then
            arad = rmpdis
          else
            arad = crad
          endif
          smldis = 0.125*arad
          helxfl = 0
          itran = 0
          iwrk = 0
          if (hldir.eq.cclw) then
            indir = 0
          else
            indir = 1
          endif
          if (ntry.eq.helix) then
            helxfl = 1
            call rrget(kk2,xx2,yy2,ier)
            if (xx2.eq.flarc) then
                kk2 = kk2 + 3
            endif
            call rrget(kk2,xx2,yy2,ier)
            if (ier .ne. 0) goto 999
            mvec(1) = xx2 - xx
            mvec(2) = yy2 - yy
            mvec(3) = 0.
          endif
          if (trans) itran = 1
          if (lwrk) iwrk = 1
          jcut = 0
          if (openfl) then
c
c.....Attempt to enter through designated open sides - ASF 10/9/13
c
            call pokopn (xx,yy,crad,buf,botpl(4),mvec,offdis,helxfl,
     x          islnds,arad,indir,itran,mxsv,iwrk,invwrk,lctyp,jcut,ier)
          else
c
c.....Enter off part for waterline with flat helix.
c
            if (ntry .eq. helix) crad = crad + arad
            call poknt0 (xx,yy,zbot,crad,buf,trans,mx,jcut,ier)
c
c.....Added this check to make sure the entry still behaves like
c.....it did in previous versions of NCL in this case - ASF 2/19/14.
c
            if (ier.eq.0.and.look.eq.2) then
              step = step*fact
              stpmin = stpmin*fact
              goto 999
            endif
          endif
          if (ier .ne. 0) then
            ier = 530
            goto 999
          endif
          if (ntry.eq.helix.and.helxfl.eq.0) goto 50
          if (jcut.eq.0) then
            mtype = 0
            call pokfed (fpos, fhld, mtype, ier)
            if (ntry.ne.helix) then
              buf(3) = znow !toppl(4)
              call poksto (buf, mtype, trans, mx, ier)
              call pokfed (fret, fhld, mtype, ier)
              if (ntry.eq.plunge) then
                buf(3) = botpl(4)
                call poksto (buf, mtype, trans, mx, ier)
              endif
              call pokfed (fent, fhld, mtype, ier)
            else
              buf(3) = botpl(4)
              call nclf_pockgen_helix(arad,smldis,xx,yy,buf,znow,tol,
     x               indir,mvec,itran,mx,ier,fret,fent,fhld,mtype)
              if (ier .ne. 0) then
                ier = 530
                goto 999
              endif
            endif
            fmov = fgen
            goto 999
          endif
        else
c
c.....Assume all sides are open.
c
          call poknt0 (xx,yy,zbot,crad,buf,trans,mx,jcut,ier)
          if (ier .ne. 0) goto 999
          if (jcut .eq. 0) then
            if (look.eq.2) then
              step = step*fact
              stpmin = stpmin*fact
            else if (lwatfl .and. tamode .ge. 1) then
              call mxcreate (buf,buf(1),buf(2),zbot)
              call pokfed (fpos, fhld, mtype, ier)
              buf(3) = znow + zbot
              buf(4) = 0
              buf(5) = 0
              buf(6) = 1
              mtype = 0
              call pokstn (buf, tainv, mtype, trans, mx, ier)
              call pokfed (fret, fhld, mtype, ier)
              buf(3) = zbot
              call pokstn (buf, tainv, mtype, trans, mx, ier)
              call pokfed (fent, fhld, mtype, ier)
              fmov = fgen
              goto 999
            else
              call pokfed (fpos, fhld, mtype, ier)
              mtype = 0
              call poksto (buf, mtype, trans, mx, ier)
              call pokfed (fret, fhld, mtype, ier)
              buf(3) = zbot
              call poksto (buf, mtype, trans, mx, ier)
              call pokfed (fent, fhld, mtype, ier)
              fmov = fgen
            endif
            goto 999
          endif
        endif
        ntry = tntry
      endif

50    wall = .false.
      lfla = .false.
      if (idir .lt. 0 .or. lane(ix) .gt. 100) wall = .true.
      ntry = entry
      lislrc = (lane(ix).gt.100 .and. isle.eq.arc)
      tanto = .false.
      if (ntry .lt. 0) then
c        if (lane(ix).le.100 .or. lacefl.eq.0)
        if (lacefl.eq.0)
     x  tanto = .true.
        ntry = -ntry
      endif
      lcoupl = .false.
      lcycle = .false.
      if (ntry .eq. couple) then
        lcoupl = .true.
        ntry = helix
      else if (ntry .eq. vcycle) then
        lcycle = .true.
        ntry = helix
      endif
      if (lane(ix) .gt. 100) ntry = -plunge
c      if (lacefl.gt.0 .and. (ntry.eq.helix .or. ntry.eq.ramp))
c     x  ntry = plunge
c
c... Check if the LACE/SCRUB entry point has already been cut for Ramp or Helix
c
      if (ix .gt.locapo .and. lacefl.gt.0 .and.
     x    (ntry.eq.helix .or. ntry.eq.ramp)) then
        icut = 1
        call entrypntcut(idir,ix,xpt,ypt,icut)
cc        if (ierr .eq. 0) goto 100
        if (icut .eq. 1) ntry = -plunge
      endif

      if (ntry.eq.helix .and. lacefl.gt.0) then
        wall = .false.
        iramps = nramps
        if (rmpdis .gt. 0.0) then
          dis = rmpdis
        else
          dis = 0.5*sc(28)
        endif
c        dis = .5*dis
        call hlentr (ix,xx,yy,xt,yt,dis,fact,ierr)
        if (ierr .eq. 0) goto 100
        wall = .true.
      endif

      iramps = nramps
c
c... set up start points and direction vector
c
      xt(1) = xx
      yt(1) = yy

      if (wall .or. ntry.ne.plunge) then

        kk  = kk+1
        call rrget(kk,xx,yy,ier)
        if (ier .ne. 0) goto 999
        if (xx .ge. 0.9*flarc) then
          lfla = .true.
          kk  = kk+3
          call rrget(kk,xx,yy,ier)
          if (ier .ne. 0) goto 999
        endif
        xt(2) = xx
        yt(2) = yy
        vx = (xt(2) - xt(1))
        vy = (yt(2) - yt(1))
        dis = sqrt(vx**2+vy**2)
        if (dis .gt. 0.0) then
          vx = vx/dis
          vy = vy/dis
        endif
        if (abs(ntry) .ne. plunge) then
          if (lacefl .gt. 0 .and. dis .gt. sc(28)) dis = sc(28)
          if (ntry .eq. helix) dis = .5*dis
          if (rmpdis .gt. 0.0) dis = rmpdis
          if (tanto .and. (ntry.eq.helix .or. ntry.eq.ramp)) then
            if (ntry.eq.helix) then
              sid = klimb*idir
              vtmp = vx
              vx = -sid*vy
              vy = sid*vtmp
            else
              vx = -vx
              vy = -vy
            endif
c
c..... if doing a tangent entry on an arc, use the true arc tangency
c
            if (lfla) then
              kk = kk-2
              call rrget(kk,xx,yy,ier)
              if (ier .ne. 0) goto 999
              rad = xx
              kk = kk+1
              call rrget(kk,xx,yy,ier)
              if (ier .ne. 0) goto 999
              if (ntry .eq. helix) then
                cx = (xt(1) - xx)/rad
                cy = (yt(1) - yy)/rad
              else
                cx = (yy - yt(1))/rad
                cy = (xt(1) - xx)/rad
              endif
              if (cx*vx + cy*vy .lt. 0) then
                vx = -cx
                vy = -cy
              else
                vx = cx
                vy = cy
              endif
            endif
          endif
          xt(2) = xt(1) + dis*vx
          yt(2) = yt(1) + dis*vy
        endif
      endif
c... offset start points and direction vector if a wall
      if (wall .and. .not.tanto) then
        sid = klimb*idir*step*.5
        if (ntry.ne.helix .and.
     x      (isle.ne.arc.or.ntry.ne.-plunge)) then
          xt(1) = xt(1) - vy*sid
          yt(1) = yt(1) + vx*sid
        endif
        if (abs(ntry) .ne. plunge) then
          xt(2) = xt(1) - vy*dis
          yt(2) = yt(1) + vx*dis
        endif
      endif
c... set up bottom of cut layer

100   continue

      znow = buf(3)
      zbot = botpl(4)
      botok = .true.

      if (tamode .ge. 1) then
        buf(1) = xt(1)
        buf(2) = yt(1)
        buf(3) = tabot
        itp = -1
        ind = 0
        call surftl (itp, ind, buf, trans, mx, ier)
        if (ier .ne. 0) goto 999
        xx = xt(1)
        yy = yt(1)
        call mxcreate (buf,xx,yy,zbot)
        buf(1) = xt(1)
        buf(2) = yt(1)
        buf(3) = zbot
        buf(4) = 0
        buf(5) = 0
        buf(6) = 1
      else if (sfcut .and. (zbot .lt. botpl(1))) then
c... bottom below high surface tool pt
        buf(1) = xt(1)
        buf(2) = yt(1)
        buf(3) = zbot
        itp = -1
        ind = 0
        call surftl (itp, ind, buf, trans, mx, ier)
        if (ier .ne. 0) goto 999
        if (zbot .lt. buf(3)) then
          botok = .false.
          zbot = buf(3)
        endif
      endif
c
c... set up top of layer cut
c
      ztop = toppl(4)
      if (ntry .eq. -plunge) then
        ztop = zbot
      else if (zbot .ge. ztop) then
c... no stock for fancy entry ensure a straight down one
        ztop = zbot
        ntry = plunge
      endif
c
c... set the index for start point
c
      ixt = 1
      if (ntry.eq.ramp .and. nramps-nramps/2*2.eq.1) then
        ixt = 2
        vx = 0
        vy = 0
        rad = 0
      else if (.not.lislrc) then
c
c..... qar 95223 - the block messed up the island 'arc' entry, which is
c..... treated separately
c
        vx = xt(1)-xt(2)
        vy = yt(1)-yt(2)
        rad = sqrt(vx**2+vy**2)
        if (rad .gt. 0.0) then
          vx = vx/rad
          vy = vy/rad
        endif
      endif
c
c..... check if geometry is gouged
c
      if ((ntry.eq.helix .or. ntry.eq.ramp) .and. nwrn.le.0) then
        call chkntr (ix,ntry,wall,xt,yt,step,dis,rad,fact,ier,jshuff)
        if (ier .ne. 0 .or. look.eq.2) goto 999
        if (jshuff .eq. 2) then
          ishuff(ix) = 2
          goto 999
        else if (jshuff .eq. 1) then
          ishuff(ix) = 0
        endif
      endif
c
c... move to start point at posn fedrat
c
      call pokfed (fpos, fhld, mtype, ier)
      buf(1) = xt(ixt)
      buf(2) = yt(ixt)
      buf(3) = znow
c
c..... the block below is for the ARC entry, maxang holds the fraction
c..... of the full revolution
c
      if (ntry.eq.helix .and. nramps.eq.1 .and. maxang.gt.0) then
        if (vx.gt.1.) vx = 1.
        if (vx.lt.-1.) vx = -1.
        ang = acos(vx)
        if (vy.lt.0) ang = -ang
        i = maxang
        dang = (1. - (maxang-i))*TWO_PI

        if (sid .lt. 0.0) then
c
c..... fsr 60981: removed the 'wall' condition
c
          if (wall .or. sc(169).ge.9.549) dang = -dang
        endif

        ang = ang+dang
        buf(1) = xt(2) + dis*cos(ang)
        buf(2) = yt(2) + dis*sin(ang)
      endif

      if (lislrc) then
        sid = klimb*idir

        buf(1) = xt(1) - (vy*sid + vx)*retdis
        buf(2) = yt(1) + (vx*sid - vy)*retdis

        if (tamode .ge. 1) then
          call pokstn (buf, tainv, mtype, trans, mx, ier)
        else
          call poksto (buf, mtype, trans, mx, ier)
        endif
        call pokfed (fret, fhld, mtype, ier)
        buf(3) = ztop+retdis
        if (tamode .ge. 1) then
          call pokstn (buf, tainv, mtype, trans, mx, ier)
        else
          call poksto (buf, mtype, trans, mx, ier)
        endif
        call pokfed (fent, fhld, mtype, ier)

        if (retdis .lt. 4.*tol) then
          npts = 5
        else
          dang = 4*acos(1. - tol/retdis)
          npts = PI/dang + 0.5
        endif
        dang = HALF_PI/npts
        zdis = retdis/npts
        xc = xt(1) - vy*sid*retdis
        yc = yt(1) + vx*sid*retdis
        do i = 1,npts-1
          buf(1) = xc + (sin(i*dang)*vy*sid - cos(i*dang)*vx)*retdis
          buf(2) = yc + (-sin(i*dang)*vx*sid - cos(i*dang)*vy)*retdis
          buf(3) = buf(3) - zdis
          if (tamode .ge. 1) then
            call pokstn (buf, tainv, mtype, trans, mx, ier)
          else
            call poksto (buf, mtype, trans, mx, ier)
          endif
        enddo

      else
        if (tamode .ge. 1) then
          call pokstn (buf, tainv, mtype, trans, mx, ier)
        else
          call poksto (buf, mtype, trans, mx, ier)
        endif
        if (.not.lcycle) then
          call pokfed (fret, fhld, mtype, ier)
          buf(3) = ztop+retdis
          if (tamode .ge. 1) then
            call pokstn (buf, tainv, mtype, trans, mx, ier)
          else
            call poksto (buf, mtype, trans, mx, ier)
          endif
        endif
      endif
c
c... maxang logic
c
      if (nramps.eq.0 .and. maxang.gt.0 .and.
     x     (ntry.eq.ramp .or. ntry.eq.helix)) then
        zdpt = ztop - zbot + retdis
        if (ntry .eq. ramp) then
          dang = 2*dis*tan(maxang/RADIAN)
        else
          dang = 2*PI*dis*tan(maxang/RADIAN)
        endif
        if (dang .gt. 0.0001) iramps = zdpt/dang
        if ((iramps*dang) .lt. zdpt) iramps = iramps+1
        if (ntry .eq. ramp) iramps = 2*iramps
      endif
c
c..... do the helical entry or the ramps
c
      call pokfed (fent, fhld, mtype, ier)
      zdpt = ztop-zbot+retdis

      if (tamode .ge. 1) zdpt = zdpt - bthk

      if (lcycle) zdpt = zdpt - retdis
      if (ntry .eq. helix) then
        if (sc(169).lt.9.349 .or. dis.lt.4.*tol) then
          npts = 10
        else
          dang = acos(1. - tol/dis)
          if (nramps.eq.1 .and. maxang.gt.0) then
            npts = (maxang*PI)/dang + 0.5
          else
            npts = PI/dang + 0.5
          endif
        endif
        dang = npts
        dang = TWO_PI/dang
        if (nramps.eq.1 .and. maxang.gt.0) dang = maxang*dang
        if ((wall.or.tanto) .and. sid .lt. 0.0) dang = -dang
        if (dang .gt. 0 .and. hldir.eq.clw) dang = -dang
        npts = npts*iramps
        zdis = npts
        zdis = zdpt/zdis
        vx = buf(1)-xt(2)
        vy = buf(2)-yt(2)
        rad = sqrt(vx**2+vy**2)
        if (rad.gt.0.) then
          vx = vx/rad
          vy = vy/rad
        endif
        if (vx.gt.1.) vx = 1.
        if (vx.lt.-1.) vx = -1.
        ang = acos(vx)
        if (vy.lt.0) ang = -ang
        if (lcoupl) then
          call pklcou (iramps,zdpt,xt(2),yt(2),rad,trans,mx,ier)
          zdis = 0
        else if (lcycle) then
          ro = abs ((TWO_PI*zdis)/dang)
          call pklcyc (zdpt,rad,ro,fent,retdis,ang,ier)
          mtype = 0
          buf(1) = xt(2)
          buf(2) = yt(2)
          buf(3) = ztop+retdis
          if (tamode .ge. 1) then
            call pokstn (buf, tainv, mtype, trans, mx, ier)
          else
            call poksto (buf, mtype, trans, mx, ier)
          endif
        endif
        do 210 i=1,npts-1
          toolow = .false.
          ang = ang+dang
          buf(1) = xt(2) + rad*cos(ang)
          buf(2) = yt(2) + rad*sin(ang)
          buf(3) = buf(3)-zdis
          if (nwrn.lt.1 .and. sfcut .and. .not.lcycle. and.
     x        (buf(3) .lt. botpl(1))) then
c... z entry below high surface tool pt
            ztop = buf(3)
            itp = -1
            ind = 0
            call surftl (itp, ind, buf, trans, mx, ier)
            if (ier .ne. 0) goto 999
            if (ztop .lt. buf(3)-1.5*tol) then
c... gouging surface with entry
              if (nwrn .le. -1) then
                ifl(2) = -475
                toolow = .true.
              else
                ifl(2) = -473
                buf(3) = ztop
              endif
            else
              buf(3) = ztop
            endif
          endif
          if (.not.lcycle) then
            if (tamode .ge. 1) then
              call pokstn (buf, tainv, mtype, trans, mx, ier)
            else
              call poksto (buf, mtype, trans, mx, ier)
            endif
          endif
          if (toolow) buf(3) = ztop
210     continue
      else if (ntry.eq.ramp.and.iramps.gt.1) then
        zdis = iramps
        zdis = zdpt/zdis
        do 215 i=1,iramps-1
          toolow = .false.
          ixt = 3-ixt
          buf(1) = xt(ixt)
          buf(2) = yt(ixt)
          buf(3) = buf(3)-zdis
          if (nwrn.lt.1 .and. sfcut .and. (buf(3) .lt. botpl(1))) then
c... z entry below high surface tool pt
            ztop = buf(3)
            itp = -1
            ind = 0
            call surftl (itp, ind, buf, trans, mx, ier)
            if (ier .ne. 0) goto 999
            if (ztop .lt. buf(3)-1.5*tol) then
c... gouging surface with entry
              if (nwrn .le. -1) then
                ifl(2) = -475
                toolow = .true.
              else
                ifl(2) = -473
                buf(3) = ztop
              endif
            else
              buf(3) = ztop
            endif
          endif
          if (tamode .ge. 1) then
            call pokstn (buf, tainv, mtype, trans, mx, ier)
          else
            call poksto (buf, mtype, trans, mx, ier)
          endif
          if (toolow) buf(3) = ztop
215     continue
      endif
      if (botok .and. wall .and. ntry .ne. helix) then
        buf(1) = xt(1)
        buf(2) = yt(1)
        buf(3) = zbot
        if (tamode .ge. 1) then
          call pokstn (buf, tainv, mtype, trans, mx, ier)
        else
          call poksto (buf, mtype, trans, mx, ier)
        endif
      endif
      fmov = fgen
      if (ntry .eq. -plunge) fmov = flst

999   return
      end

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pokxit
c **
c **  purpose of subroutine: to perform all the necessary motions and
c **                         feed rates for exits from the pocket.
c **  parameters
c **     input  :
c **        idir    - if > 0 then move away from wall then
C **                  retract straight up, else straight up
c **     output  :
c **                  ier > 0 - errors from srfout
c **
c **********************************************************************
c **********************************************************************

      subroutine pokxit(idir,ix,klimb,istep,iclr,lstfl,buf,trans,mx,ier)

      include 'com4a.com'
      include 'vocab.com'
      include 'const.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'

      common/rrfr/ fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov
      real*8 fgen, fpos, ftrn, fret, fent, flst, ffrs, fhld, fmov

      common/tacom/ tabot
      real*8 tabot

      real*8 buf(14), mx(12), zbot, del
      real*4 hdis, vdis, v1, v2, sec, sid
      integer*4 klimb, itp, ind, ier
      integer*2 icpt, klok, lstfl
      integer*2 lcir, idir, ix, istep
      integer*4 ix1, ix2
      logical trans, wall

      integer*4 kk
      real*8 xx,yy,x1,y1,x2,y2,x3,y3,x4,y4,xc,yc,crad
      real*4 xx4,yy4,x14,y14,x24,y24,x44,y44,ro,alf,dang
      real*4 zdis,disi

      integer*2 i,npts,jcut,jtry,isvasw(480)
      equivalence (rsvasw, isvasw)

      integer*2 DEBUGX
      parameter (DEBUGX=0)
      character*80 dbuf
      byte dout(80)

      integer*2 ixp
      data ixp /0/

      if (DEBUGX .eq. 1) then
        ixp = ixp + 1
        write (dbuf,7000) ixp
 7000   format ("ixp = ",i6)
        call ctob (dbuf,dout)
        call nclxostr(dout)
        idbx = 0
        if (ixp .eq. 12) then
          idbx = 1
        endif
      endif
      hdis = hrtdis
      vdis = vrtdis
      jcut = 0

      if (hdis.eq.0.) hdis = tol*10.
      if (vdis.eq.0.) vdis = tol*10.
      wall = .false.
      if (lacefl.eq.0 .and.
     *    (idir.gt.0 .or. lane(ix).gt.100)) wall = .true.
      if (wall) then
c
c... move away from wall
c
        ix2 = loca(ix+1)-1
        ix1 = ix2-1
        kk = ix2
        call rrget(kk,xx,yy,ier)
        if (ier. gt. 0) goto 999
        xx4=xx
        yy4=yy

        lcir = 0
        if (ix2-loca(ix).gt.3) then
          kk = ix2 - 3
          call rrget(kk,x3,y3,ier)
          if (ier. gt. 0) goto 999
          if (x3 .ge. 0.9*flarc) then

            lcir = 1
            ix1 = ix2-4
            kk = ix2 - 4
            call rrget(kk,x4,y4,ier)
            if (ier. gt. 0) goto 999
            x44=x4
            y44=y4
            kk = ix2 - 2
            call rrget(kk,x2,y2,ier)
            if (ier. gt. 0) goto 999
            x24=x2
            y24=y2
            kk = ix2 - 1
            call rrget(kk,x1,y1,ier)
            if (ier. gt. 0) goto 999
            x14=x1
            y14=y1

            icpt = 2

            klok = y24
            call tanarc(icpt,x14,y14,x24,klok,x44,y44,xx4,yy4,v1,v2)
          endif
        endif
        if (lcir.eq.0) then
          kk = ix2 - 1
          call rrget(kk,x1,y1,ier)
          if (ier. gt. 0) goto 999
          v1 = xx-x1
          v2 = yy-y1
          sec = sqrt(v1**2+v2**2)
          if (sec.gt.0.) then
            v1 = v1/sec
            v2 = v2/sec
          endif
        endif
c... move off at 45
        sid = klimb*idir
        buf(1) = xx+(v1-v2*sid)*.7071*hdis
        buf(2) = yy+(v2+v1*sid)*.7071*hdis

          if (nwrn.le.0 .and. lcir.eq.0) then
            crad = sc(28)/2.+sc(24)
c
c..... if WARN check if the exit move gouges geometry, if AVOID and
c..... there is a gouge, try to move at a different angle
c
            jtry = 0
            dang = 45
210         i = 1

            a0 = xx
            b0 = yy
            a1 = buf(1)
            b1 = buf(2)
            if (ifl(264) .eq. 0) then
              a0 = a0*25.4
              b0 = b0*25.4
              a1 = a1*25.4
              b1 = b1*25.4
            endif
c
c..... isvasw(4*i-1) is 1,0,-1 if the perimeter condition is in,on,out (with
c..... i = 1); for islands it is 1,0,-1 if the condition is out,on,in
c
215         if (isvasw(4*i-1) .eq. -1) then
              i = i+1
              if (i .ge. islnds + 2) goto 219
              goto 215
            endif
c
c..... fstoff(i) = crad * isvasw(4*i-1)
c
            ro = crad*isvasw(4*i-1) - 1.5*tol
            if (ro .lt. 0.) ro = 0.
            if (ifl(264) .eq. 0) ro = ro*25.4

            call chkgou (0,i,ro,a0,b0,a1,b1,jcut,ier,idbx)
            if (ier .ne. 0) goto 999
cc jcut = 0

            if (jcut .ne. 0) then
              if (nwrn.eq.0) then
c.....        output warning
                ifl(2) = -510
                goto 219
              endif
              if (nwrn.le.-1) then
                if (jtry .gt. 18) then
                  ier = 15
                  goto 999
                endif
                jtry = jtry + 1
                dang = dang + 5
                alf = dang/RADIAN
                if (nwrn .eq. -1) ifl(2) = -511
                buf(1) = xx + (v1*cos(alf)-v2*sid*sin(alf))*hdis
                buf(2) = yy + (v2*cos(alf)+v1*sid*sin(alf))*hdis
                goto 210
              endif
            endif
            i = i + 1
            if (i .lt. islnds + 2) goto 215
          endif

219     continue
c..... end of "if (wall) then"
      endif
c... move up retract distance
      zbot = botpl(4)
      if (sfcut .and. (zbot .lt. botpl(1))) then
c... surface bottom tool pt
        if (tamode .ge. 1) then
        buf(3) = tabot
        else
        buf(3) = zbot
        endif
        itp = -1
        ind = 0
        call surftl (itp, ind, buf, trans, mx, ier)
        if (ier .ne. 0) goto 999
        if (zbot .lt. buf(3)) zbot = buf(3)
      endif
      mtype = 0
      if (wall .and. ((isle.eq.arc .and. lane(ix).gt.100) .or.
     x        (lane(ix).lt.100 .and. perim.eq.arc))) then
        if (hdis .lt. 4.*tol) then
          npts = 5
        else
          dang = 4*acos(1. - tol/hdis)
          npts = PI/dang + 0.5
        endif
        dang = HALF_PI/npts
        zdis = vdis/npts
        xc = xx - v2*sid*hdis
        yc = yy + v1*sid*hdis
        do i = 1,npts
          buf(1) = xc + (cos(i*dang)*v2*sid + sin(i*dang)*v1)*hdis
          buf(2) = yc + (-cos(i*dang)*v1*sid + sin(i*dang)*v2)*hdis
          disi = i*zdis

          if (tamode.ge.1) then
            disi = disi + bthk
            buf(1) = buf(1) + disi*buf(4)
            buf(2) = buf(2) + disi*buf(5)
            buf(3) = buf(3) + disi*buf(6)
            call pokst0 (buf, mtype, trans, mx, ier)
          else
            buf(3) = zbot + disi
            call poksto (buf, mtype, trans, mx, ier)
          endif

        enddo
      else
        if (tamode.ge.1) then
          del = vdis + bthk
          buf(1) = buf(1) + del*buf(4)
          buf(2) = buf(2) + del*buf(5)
          buf(3) = buf(3) + del*buf(6)
          call pokst0 (buf, mtype, trans, mx, ier)
        else
          buf(3) = zbot + vdis
          call poksto (buf, mtype, trans, mx, ier)
        endif

      endif
c... move to clear plane at retract fedrat
c.....Added option to do final retract without going to the
c.....clearance plane - ASF 11/20/13.
      if (lstfl.eq.1.and.rtract.eq.2) goto 999
      call pokfed (fret, fhld, mtype, ier)
      mtype = 0
      if (tamode.ge.1) then
        if (iclr.eq.2) then
          del = botpl(4)+clrlvl
        else if (iclr.eq.3) then
          del = toppl(4)+clrlvl-bthk
        else
          del = clpl(4)-bthk
        endif
        del = del - vdis
        buf(1) = buf(1) + del*buf(4)
        buf(2) = buf(2) + del*buf(5)
        buf(3) = buf(3) + del*buf(6)
        call pokst0 (buf, mtype, trans, mx, ier)
      else
        zbot = buf(3)
        if (iclr.eq.2) then
          buf(3) = botpl(4)+clrlvl
c     else if (iclr.eq.0 .or. iclr.eq.3) then
        else if (iclr.eq.3) then
          buf(3) = toppl(4)+clrlvl
        else
          buf(3) = clpl(4)
        endif
        call poksto (buf, mtype, trans, mx, ier)
      endif

999   return
      end
      

c **********************************************************************
c **********************************************************************
c **   subroutine name:  pmofst
c **
c **  purpose of subroutine: Offset a set of points to define cl point
c **                         contour for PMILL.  The points are defined
c **                         by slicing the tessellation with a slicing
c **                         plane and so the assumption that they are
c **                         in the same plane is valid for the POCKET
c **                         routines
c **  parameters
c **     input  : none
c **     output : none
c **
c **********************************************************************
c **********************************************************************      

      subroutine opmill (thk,plnge)
      
      include 'com4a.com'
      include 'vocab.com'
      include 'rrdm.com'
      include 'pokcom.com'
      include 'pokpl.com'

      integer*2 plnge

      integer*2 ksn(4)
      equivalence (asn,ksn)
      real*8 ta0(3),thk

      real*8 asn, dsec, comax
      real*4 step,dislev,den,crad,toolrd,toolcr,tlstep,hs
      real*4 fstoff(120)
      integer*4 loops, klimb, ier
      integer*2 jx, istrt, iend

      integer*2 isn(4), isvasw(480)
      equivalence (rsvasw, isvasw)
      equivalence (isn(1), asn)

      real*8 r8jb(36)
      equivalence (jb, r8jb)

      logical trans
      
      real*4 XA(maxpts),YA(maxpts)
      integer*2 NPPA

      integer*2 POCKET
      parameter (POCKET = 22)

      integer*2 izro /0/, i2v3 /3/, i2v4 /4/

      if (ifl(2).gt.0) goto 999
      ier = 0
      tol = sc(27)
      if (tol .le. 0.0) tol = .001
      trans = .false.

      lacefl = 0
      lboth = .false.

      bddir = scrdir
      bthk = 0
c
c... set up transformation matrix if out of xy plan
c
      ta0(1) = 0.
      ta0(2) =0.
      ta0(3) = 1.
      lwatfl = .false.

      if (.not.lwatfl) then
        sc(23) = psthk
        sc(24) = dsthk
        sc(202) = offthk
      endif
c
c.....Get boundary points
c
      ksn(4) = 11
      isc10(3) = 1
      rsvasw(1) = asn
      call pokinp (ta0,trans,mx,ier)
      call nclf_pmill_resetpts
c
c... max number of cut loops
c
      loops = 1
c
c... climb milling assumes a clw rotating, tool moving cclw
c... arround loops, and from the centroid to perimeter of pocket.
c
      klimb = 1
c
c... is tool moving clw around loops
c
      if (pdir .eq. clw) klimb = -klimb
c
c... are the loops from outside in
c
      if (ispirl.eq.in) klimb = -klimb
c
c... Set the "lift between sections" flag.
c
      lifts = 0
c
c...      if (sfcut .or. slift .eq. up) lifts = 1
c
      if (slift .eq. up) lifts = 1
c
c... Set the "Machine corner with arc" flag.
c
      larcs = 0
      if (corner .eq. arc) larcs = 1
c...  optional radius for inside corners
      rcorn = 0
      if (arcrad .gt. 0) rcorn = arcrad
      if (ifl(264).eq.0) rcorn = rcorn*25.4
c...  flag for S-shaped transitions between loops
      ltrarc = 0
      if (transt .eq. arc) ltrarc = 1
c
c... 1st offset = (cutterad + DSTK) x mm/in x IN/OUT from centroid
c
      if (plnge.eq.0) then
        toolrd = (sc(28)+thk)/2.
        toolcr = sc(29)
        tlstep = sc(28)+thk-sc(29)*2.
      else
        toolrd = 2.*tol
        toolcr = tol
        tlstep = 2.*tol
      endif
      if (tlstep.lt.tol) tlstep = (sc(28)+thk)/2.
      if (tlstep.lt.tol) tlstep = tol*500.

      if (plnge.eq.0) then
        crad = (sc(28)+thk)/2.+sc(24)
      else
        crad = 2.*tol
      endif
      if (ifl(264).eq.0) crad = crad*25.4
      fstoff(1) = crad
      do 120 jx=2,lanes
120   fstoff(jx) = 0 !crad*isvasw(jx*4-1)
c
c... step only set to effective tool flat if maxstep < tol
c
      step = maxstp
      if (step.lt.tol) step = tlstep
      stpmin = minstp
      if (stpmin.gt.step) stpmin = step
      if (stpmin.lt.tol) stpmin = step/2.
c
c... coverage testing parameter
c
      stpcov = toolrd-toolcr
      den = 0
      hs = hscalp
      if (toolcr .gt. tol .and. hs .gt. 0.01*tol) then
        if (hs .ge. toolcr) then
          den = toolcr
        else
          den = 2*toolcr*hs - hs**2
          if (den .le. 0.) then
            den = 0
          else
            den = sqrt(den)
          endif
        endif
      endif

      if (stpcov .lt. tol) then
        stpcov = stpcov + den
        if (stpcov .lt. tlstep) stpcov = tlstep
      else
        stpcov = stpcov + den
      endif

      if (ifl(264).eq.0) then
c
c... convert english to metric
c
        istrt = 1
        iend = lanes
        fact = 25.4
        call scllan (istrt, iend, fact)
        step = step*fact
        stpmin = stpmin*fact
        stpcov = stpcov*fact
        tol = tol*fact
        if (lv823.and.look.eq.1) then
          xlook = xlook*fact
          ylook = ylook*fact
        endif
      endif

      if (lacefl.eq.0) call initpokpols(tol)
      call ARRINP(ier)
      call CONTOUR(fstoff,step,loops,ier)
      call CUTPATH (step,klimb,ier)
      LAN1ST = 1
      K = LANES+1
   15 K = K-1
      IF (K.EQ.0) THEN
        GOTO 999
      ENDIF
      IF (LANE(K).EQ.LAN1ST.AND.LOOP(K).LT.0) THEN
        INDEX1 = K
        INDEX = MAMA(K)
        MAMA(K) = -MAMA(K)
        GOTO 20
      ENDIF
      GOTO 15
20    CALL GETLAN (INDEX,XA,YA,NPPA)
      do 130 jx=1,NPPA
        if (ifl(264).eq.0) then
            xa(jx) = xa(jx)/25.4
            ya(jx) = ya(jx)/25.4
        endif
        call nclf_pmill_push_pt(xa(jx),ya(jx))
130   continue
c
c.....Free memory
c
      call scrfin
      if (.not.lwatfl) call frpkys
      if (lacefl.eq.0) call freepokpols
999   return
      end
