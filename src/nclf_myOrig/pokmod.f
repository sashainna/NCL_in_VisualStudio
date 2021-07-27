C*********************************************************************
C*    NAME         :  pokmod.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
C*       pokmod.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:26
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokmod
C*     process pocket modal values setting command
C*
C*     The enhanced pocket routine requires certain modal values to be
C*     set.  That is the function of the POKMOD statement.  The syntax 
C*     for this statement is: 
C*
C*  POKMOD/#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,#16,#17,#18,#19
C*
C*    Where:
C*
C*     #1 = Number of ramps/revolutions to reach pocket depth: (scalar)
C*            Integer*2
C*     #2 = Entry method: (RAMP, PLUNGE, HELIX or OMIT)
C*            Integer*2 - number of vocabulary word
C*          Optional AVOID/WARN/NOWARN for the entry path
C*     #3 = Ramp distance/Helical radius: (scalar)
C*            Real*4
C*          Optional RETRCT/OFF for no exit move
C*     #4 = Clearance level: (scalar or plane)
C*            Real*4
C*          Optional INCR to cause retraction clearance distance above last
C*          top plane between levels.
C*     #5 = Step down between passes: (scalar)
C*            Integer*2
C*     #6 = Retract distance: (scalar)
C*            Real*4
C*     #7 = Pocket direction: (CCLW or CLW)
C*            Integer*2 - number of vocabulary word
C*     #8 = Spiral direction: (OUT)  (IN to be implemented in the future)
C*            Integer*2 - number of vocabulary word
C*     #9 = Lift tool between pocket sections: (UP or DOWN)
C*            Integer*2 - number of vocabulary word
C*    #10 = Machine corners with arcs: (LINEAR or ARC)
C*            Integer*2 - number of vocabulary word
C*    #11 = Maximum stepover distance: (number)
C*            Real*4
C*    #12 = Minimum stepover distance: (number)
C*            Real*4
C*    #13 = General pocketing feedrate: (scalar)
C*            Real*4
C*    #14 = Positioning feedrate: (RAPID or scalar)
C*            Real*4
C*    #15 = Retract feedrate: (RAPID or scalar)
C*            Real*4
C*    #16 = Entry feedrate: (scalar)
C*            Real*4
C*    #17 = Transition feedrate: (scalar)
C*            Real*4
C*    #18 = "Last pass" feedrate: (scalar)
C*            Real*4
C*    #19 = "First pass" feedrate: (Scalar)
C*            Real*4
C*
C*   The defaults for the POKMOD statement are:
C*
C*   (item #: 1    2 3 4  5  6    7   8  9   10 11 12 13  14    15  16  17 18)
C*
C*     POKMOD/1,RAMP,0,1,-1,.1,CCLW,OUT,UP,SHARP,0,0,0,RAPID,RAPID,-.5,-.5,-1
C*
C*     All items must have a value assigned.
C*
C*     The POKMOD statement is syntax checked and, if syntactically correct,
C*     the modal values are stored in their respective storage variables in
C*     the RRDM common.
C*     
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    CALLED BY    : DRIVER.F
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pokmod

      include 'com4a.com'
      include 'vocab.com'
      include 'rrdm.com'

c        temporary storage variables used until full POKMOD statement
c        has been succesfully syntax checked
      integer*2 tnramp, tentry, tcltyp, tpdir, tspirl, tslift, tcornr
      integer*2 trtrct, tnwrn, tlvdep, ttrans, tisle, tperim, tlctyp
      integer*2 tlcdir, tscrdr, thldir, tlcfin
      real*4 trmpds, tnumlv, tretds, tslang, tmaxst, tminst, tarcrd
      real*4 tgenfr, tposfr, tretfr, tentfr, ttrnfr, tfinfr, tfrsfr
      real*4 tmxang, tvretd, thretd
      real*8 tclrlv,tdirvc
      character*64 tclrlv_lab
      character*80 tcutcm,tempc
      integer*2 li,lj,is1,is4,n
      integer*2 VREVERS,STEP
      parameter (VREVERS=1008)
      parameter (STEP=92)

      integer*4 nclkey,strlen1, tclrlv_inx
      integer*2 nwds,ietype,itemp(4),tctcm1,tctcm2,tjctcm
      real*8 temp
      equivalence (itemp,temp)
      
      !tclrlv = 0.0
      
c*******************************************************
c        Parse the POKMOD statement for valid syntax
c*******************************************************

c        set parsit to skip commas
      ifl(44) = 9

c        parse parameter                          #1 - No. of ramps/revs
      call parsit
      if (scalar) then
          tmxang = 0.
          tnramp = 1
          if (tv .ge. 0.) then
            tnramp = tv
            if (tv.gt.0 .and. tnramp.ne.tv) then
              tmxang = tv
              tnramp = 1
            endif
            if (tnramp .eq. 0) tnramp = 1
          else
            tv = -tv
            if (tv .gt. 89.) then
              tnramp = 1
            else
              tnramp = 0
              tmxang = tv
            endif
          endif
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (386)
          goto 99999
      endif

c        parse parameter                          #2 - Entry method
      ldtext = .true.
      call parsit
      keytxt = 0
      ldtext = .false.
      if (ityp.eq.2 .and. ist.eq.TEXTVAR) then
        call gtdesc(tv,nclkey,nwds,ietype)
        vocab = .true.
        voc = VCYCLE
        keytxt = nclkey
      endif
      thldir = cclw
      if (vocab .and. 
     x    (voc .eq. ramp .or. voc.eq.helix .or. voc.eq.COUPLE .or.
     x     voc.eq.VCYCLE .or.voc .eq. plunge .or. voc.eq.OMIT .or.
     x     voc.eq.OUT)) then
          tentry = voc

          if (tentry.eq.ramp .and. tnramp.eq.1 .and. tmxang.gt.0 .and.
     x                                               tmxang.ne.1) then
            n = tmxang + 0.5
            if (n .gt. 0) tnramp = n
            tmxang = 0
          endif

          call parsit
          if (vocab .and. (voc.eq.PERPTO .or. voc.eq.TANTOV .or.
     x                     voc.eq.CLW .or. voc.eq.CCLW)) then
            if (voc.eq.TANTOV) then
              if (tentry.eq.helix) tentry = -helix
              if (tentry.eq.couple) tentry = -couple
              if (tentry.eq.VCYCLE) tentry = -VCYCLE
              if (tentry.eq.ramp) tentry = -ramp
            else if (tentry.eq.helix .and. voc.eq.CLW) then
              thldir = clw
            endif
            call parsit
          endif
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (387)
          goto 99999
      endif
c
c...  Optional AVOID/WARN/NOWARN
c
      tnwrn = 0
      if (vocab) then
        if (ist.ne.warn.and.ist.ne.nwarn.and.ist.ne.avoid) then
          call error (5)
          goto 99999
        endif
        if (ist.eq.nwarn) tnwrn = 1
        if (ist.eq.avoid) tnwrn = -1
        call parsit
        if (tnwrn.eq.-1 .and .vocab.and.ist.eq.nwarn) then
          tnwrn = -2
          call parsit
        endif
      endif

c        parse parameter                          #3 - Ramp dis/Helical rad
      if (scalar) then
          if (tv .lt. 0) then
              call error (388)
              goto 99999
          endif
          trmpds = tv
          call parsit
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (388)
          goto 99999
      endif
c
c...  Optional OUT,ARC/SHARP
c
      tperim = sharp
      if (vocab .and. ist.eq.out) then
        call parsit
        if (vocab .and. (ist.eq.arc .or. ist.eq.sharp)) then
          if (ist.eq.arc) tperim = arc
          call parsit
        else
          call error (394)
          goto 99999
        endif
      endif
c
c...  Optional ISLAND,ARC/SHARP
c
      tisle = sharp
      if (vocab .and. ist.eq.island) then
        call parsit
        if (vocab .and. (ist.eq.arc .or. ist.eq.sharp)) then
          if (ist.eq.arc) tisle = arc
          call parsit
        else
          call error (394)
          goto 99999
        endif
      endif
c
c...  Optional RETRACT,ON/OFF
c
      trtrct = 1
      if (vocab .and. voc.eq.RETRCT) then
        call parsit
        if (ityp.ne.1.or.ist.ne.ON.and.ist.ne.OFF.and.ist.ne.STEP) then
          call error (5)
          goto 99999
        endif
        if (ist.eq.OFF) then
          trtrct = 0
c
c.....Added STEP rectract option.  Retract only by retract distance and do
c.....not retract to clearance plane (on final pocket exit) - ASF 11/20/13.
c
        else if (ist.eq.STEP) then
          trtrct = 2
        endif
        call parsit
      endif

c        parse parameter                          #4 - Clearance level
c
c        (tcltyp = 0 indicates a scalar
c                    distance was used for the 
c                    clearance level parameter.
c                = 1 indicates a PLANE
c                    variable name was used for the 
c                    clearance level parameter.  The ascii
c                    characters in the POKMOD statement are
c                    stored in the real*8 variable.  They 
c                    include the subscript of a PLANE ID in
c                    the last 2 bytes of the real*8.  The 
c                    the canonical data of the PLANE
c                    variable at the time the POCKET 
c                    statement is executed will be used.
      if (scalar) then
          tclrlv = tv
          tcltyp = 0
      else if (geom .and. (geotyp.eq.plane .or. geotyp.eq.SURF)) then
          if (geotyp.eq.SURF) then
            call gtdesc(tv,nclkey,nwds,ietype)
            call ncl_get_sf_primtyp(nclkey,ietype)
            if (ietype.ne.3) then
              ist = 99
              call error(389)
              goto 99999
            endif
          endif
          tclrlv_lab = token2
          tclrlv_inx = ivxsub
          tcltyp = 1
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (389)
          goto 99999
      endif
c            check for INCR following clear level
      call parsit
      if (vocab .and. voc.eq. INCR) then
        if (tcltyp.eq.1) then
          call error (390)
          goto 99999
        endif
        tcltyp = 2
        call parsit
      endif

c        parse parameter                          #5 - Step down between passes
      if (scalar) then
          tnumlv = tv
          call parsit
          tlvdep = 0
          if (vocab) then
          if (voc.eq.DEPTH) tlvdep = 1
            call parsit
          endif
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (390)
          goto 99999
      endif

c        parse parameter                          #6 - Retract distance
      if (scalar) then
          if (tv .lt. 0) then
              call error (391)
              goto 99999
          endif
          tretds = tv
          thretd = tv
          tvretd = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (391)
          goto 99999
      endif

      call parsit
      if (scalar) then
        if (tv .lt. 0) then
          call error (391)
          goto 99999
        endif
        thretd = tv
        tvretd = tv
        call parsit
        if (scalar) then
          if (tv .lt. 0) then
            call error (391)
            goto 99999
          endif
          tvretd = tv
          call parsit
        endif
      endif

      tlctyp = 0
      if (vocab .and. 
     x    (voc.eq.LACE .or. voc.eq.SCRUB)) then
        tlctyp = 1
        if (voc .eq. SCRUB) then
          tlctyp = 3
c        else if (voc .eq. BOTH) then
c          tlctyp = 3
        endif
        tlcdir = -1
        tspirl = out
        tlcfin = 0
        call parsit
        if (vocab) then
          if (voc .eq. POSX) then
            tlcdir = 0
          else if (voc .eq. NEGX) then
            tlcdir = 1
          else if (voc .eq. POSY) then
            tlcdir = 2
          else if (voc .eq. NEGY) then
            tlcdir = 3
          else if (voc .eq. LONG) then
            tlcdir = 4
          else if (voc .eq. VSHORT) then
            tlcdir = 5
          endif
        else if (geom .and. 
     x               (geotyp.eq.VECTOR .or. geotyp.eq.PNTVEC)) then
          tlcdir = 6
          tdirvc = tv
        endif
        if (tlcdir .lt. 0) then
          call error(11)
          goto 99999
        endif
        goto 50
      endif
  
      if (vocab .and. voc.eq.COLAPS) call parsit
c        parse parameter                          #7 - Pocket direction
      if (vocab .and. 
     x    (voc .eq. cclw .or.  voc .eq. clw)) then
          tpdir = voc
      else 

c            set ist high to get error to put out correct error
          ist = 99
          call error (316)
          goto 99999
      endif

c        parse parameter                          #8 - Spiral direction
      call parsit
      if (vocab .and. 
     x    (voc .eq. out .or. voc .eq. in)) then
          tspirl = voc
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (392)
          goto 99999
      endif

50    call parsit
      tscrdr = 0
      if (tlctyp.gt.0 .and. vocab .and. voc.eq.FINISH) then
        call parsit
        if (vocab .and. (voc.eq.OMIT .or. voc.eq.SAME .or.
     x      voc.eq.VREVERS .or. voc.eq.CLW .or. voc.eq.CCLW)) then
          if (voc .eq. OMIT) then
            tlcfin = fpnon
          else if (voc .eq. CLW) then
            tlcfin = fpclw
          else if (voc .eq. CCLW) then
            tlcfin = fpccw
          else if (voc .eq. VREVERS) then
            tlcfin = fprev
          endif
          call parsit
        endif
      endif

      if (vocab .and. voc.eq.BOTH) then
        tscrdr = 0
        if (tlctyp .eq. 1) then
          tlctyp = 2
        else if (tlctyp .eq. 3) then
          tlctyp = 4
        else
c            set ist high to get error to put out correct error
          ist = 99
          call error (5)
          goto 99999
        endif
        call parsit
        if (vocab .and. (voc.eq.VSHORT .or. voc.eq.SAME .or.
     x                           voc.eq.CLW .or. voc.eq.CCLW)) then
          if (voc .eq. SAME) then
            tscrdr = bsame
          else if (voc .eq. CLW) then
            tscrdr = bcw
          else if (voc .eq. CCLW) then
            tscrdr = bccw
          endif
          call parsit
        endif
      endif

      if (vocab .and. 
     x    (voc .eq. up .or.
     x     voc .eq. down)) then
          tslift = voc
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (393)
          goto 99999
      endif

c        parse parameter                          #10 - Corners with arcs
      call parsit
      if (vocab .and. (voc.eq.sharp .or. voc.eq.arc)) then
          tcornr = voc
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (394)
          goto 99999
      endif
c
c..   Optional IN,rad to round inside corners
c
      call parsit
      tarcrd = 0
      if (vocab .and. voc.eq.IN) then
        call parsit
        if (.not.scalar) then
          call error (7)
          goto 99999
        endif
        tarcrd = tv
        call parsit
      endif
c
c..   Optional ATANGL,ang to control slowdown feed rate.
c
      tslang = 200
      if (vocab .and. voc.eq.ATANGL) then
        call parsit
        if (.not.scalar) then
          call error (7)
          goto 99999
        endif
        tslang = tv
        call parsit
      endif
c
c...  Optional TRANS,ARC/SHARP
c
      ttrans = sharp
      if (vocab .and. ist.eq.transv) then
        call parsit
        if (vocab .and. (ist.eq.arc .or. ist.eq.sharp)) then
          if (ist.eq.arc) ttrans = arc
        else
          call error (394)
          goto 99999
        endif
        call parsit
      endif

c        parse parameter                          #11 - Maximum stepover
      if (scalar) then
          if (tv .lt. 0) then
              call error (395)
              goto 99999
          endif
          tmaxst = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (395)
          goto 99999
      endif

c        parse parameter                          #12 - Minimum stepover
      call parsit
      if (scalar) then
          if (tv .lt. 0) then
              call error (396)
              goto 99999
          endif
          tminst = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (396)
          goto 99999
      endif

      call parsit
c
c..   Optional HEIGHT,scallop to control coverage testing.
c
      thscal = sc(27)
      if (vocab .and. voc.eq.HEIGHT) then
        call parsit
        if (.not.scalar) then
          call error (7)
          goto 99999
        endif
        if (tv .ge. 0.01*sc(27)) thscal = tv
        call parsit
      endif

c        parse parameter                          #13 - General feedrate
      if (scalar) then
          if (tv .lt. 0) then
              call error (397)
              goto 99999
          endif
          tgenfr = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (397)
          goto 99999
      endif

c        parse parameter                          #14 - Positioning feedrate
      call parsit
      if (scalar) then
          tposfr = tv
      else if (vocab .and. voc .eq. rapid) then
          tposfr = 0
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (398)
          goto 99999
      endif

c        parse parameter                          #15 - Retract feedrate
      call parsit
      if (scalar) then
          tretfr = tv
      else if (vocab .and. voc .eq. rapid) then
          tretfr = 0
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (399)
          goto 99999
      endif

c        parse parameter                          #16 - Entry feedrate
      call parsit
      if (scalar) then
          tentfr = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (400)
          goto 99999
      endif

c        parse parameter                          #17 - Transition feedrate
      call parsit
      if (scalar) then
          ttrnfr = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (401)
          goto 99999
      endif

c        parse parameter                          #18 - "Last pass" feedrate
      call parsit
      if (scalar) then
          tfinfr = tv
      else
c            set ist high to get error to put out correct error
          ist = 99
          call error (402)
          goto 99999
      endif

c        parse parameter                          #19 - "First pass" feedrate
      tfrsfr = -1
      tjctcm = 0
      li = 1
c        check for end of statement
      if (nextyp .eq. 11) goto 100
      call parsit
      
      if (scalar) then
        tfrsfr = tv
c       change the default rapid to current, KC 03/23/2018
		if (tfrsfr .eq. 0) tfrsfr = -1
        if (nextyp .eq. 11) goto 100
        call parsit
c       Do not allow RAPID as an option for first pass, KC  03/28/2018
c      else if (vocab .and. voc .eq. rapid) then
c        tfrsfr = 0
c        if (nextyp .eq. 11) goto 100
c        call parsit
      endif

        if (vocab .and. voc.eq.CUTCOM) then
          tjctcm = 1
          tctcm1 = 0
          tctcm2 = 0
          tcutcm = ' '
          call stclix(is1,is4)
          itemp(is4) = OFF
          rct(15) = temp
          do i = 1,15
            if (nextyp .eq. 11) goto 100
            call parsit
            if (vocab .and. voc.eq.NOMORE) goto 100
            if (i.eq.1 .and. vocab .and. 
     *          (ist.eq.left .or. ist.eq.right.or. ist.eq.on)) then
              tctcm1 = ist
            else if ((i.eq.1 .or. i.eq.2) .and. vocab .and. 
     *               (ist.eq.xyplan.or. ist.eq.yzplan .or.
     *                ist.eq.zxplan)) then
              tctcm2 = ist
            else
              lj = li + strlen1(token2)
              if (li .eq. 1) then
                tempc = token2
                lj = lj - 1
              else
                tempc = ',' // token2
              endif           
              tcutcm(li:lj) = tempc
              li = lj + 1
            endif
            if (vocab) then
              itemp(is4) = ist
              rct(tjctcm) = temp
            else if (scalar) then
              rct(tjctcm) = tv
            else
              call error (4)
              goto 99999
            endif
            tjctcm = tjctcm + 1
c            li = lj + 1
          enddo
        endif
        call error (4)
        goto 99999
     
100   continue

c*******************************************************
c        All syntax is correct, load parameter values
c*******************************************************

      nramps = tnramp
      entry  = tentry
      hldir  = thldir
      rmpdis = trmpds
      perim = tperim
      isle = tisle
      clrlv_lab = tclrlv_lab
      clrlv_inx = tclrlv_inx
      clrlvl = tclrlv
      cltyp  = tcltyp
      numlvl = tnumlv
      lvdep  = tlvdep
      retdis = tretds
      hrtdis = thretd
      vrtdis = tvretd
      pdir   = tpdir
      ispirl = tspirl
      lctyp = tlctyp
      lcfin = tlcfin
      lcdir = tlcdir
      scrdir = tscrdr
      dirvec = tdirvc
      slift  = tslift
      rtract = trtrct
      corner = tcornr
      arcrad = tarcrd
      transt = ttrans
      slwang = tslang
      maxstp = tmaxst
      minstp = tminst
      hscalp = thscal
      genfr  = tgenfr
      posfr  = tposfr
      retrfr = tretfr
      entrfr = tentfr
      tranfr = ttrnfr
      finfr  = tfinfr
      frsfr  = tfrsfr
      nwrn   = tnwrn
      maxang = tmxang
      jctcom = tjctcm
      if (jctcom .gt. 1) then
        jctcm1 = tctcm1
        jctcm2 = tctcm2
        cutcm  = tcutcm
      endif

99999 return
      end
