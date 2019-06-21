C*********************************************************************
C*    NAME         :  pokpar.f
C*       CONTAINS: routines to retrieve Advanced POCKET parameters
C*      subroutine pokpar ()
C*      subroutine pmdsav ()
C*      subroutine pmdrst ()
C*      subroutine poksav ()
C*      subroutine pokrst ()
C*      subroutine pokfrm ()
C*      subroutine watfrm ()
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL
C*       pokpar.f , 25.1
C*     DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:26
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : pokpar (fnramp, fentry, fnwrn, frtrct, fcltyp,
C*                               flcflg,fpdir, fspirl, flcdir, fslift,
C*                               fcornr, farcrd, fslang, ftrans, fperim,
C*                               fisle, frmpds, fnumlv, flvdep,
C*                       fretds, fmaxst, fminst, fgenfr, fposfr,
C*                       fretfr, fentfr, ftrnfr, ffinfr, ffrsfr,
C*                       fclrlv, cclrlv, fmxang,
C*                       fjctcm, fctcm1, fctcm2, fctcom)
C*       Retrieves current Advanced POCKET POKMOD parameters from rrdm.com
C*       Fortran common area.  It is used to load the Advanced POCKET POKMOD
C*       statement building form.
C*    PARAMETERS
C*       INPUT  :
C*          none
C*       OUTPUT :
C*          fnramp      number of ramp cuts
C*          fentry      entry type
C*          fnwrn       entry warning
C*                        0 = warn
C*                        1 = nowarn
C*                       -1 = avoid
C*          frtrct      retract at exit
C*                        0 = no retract
C*                        1 = retract
C*          fcltyp      type of clearance plane entry
C*                        0 = scalar value
C*                        1 = PLANE ID
C*                        2 = scalar value with INCR
C*          fpdir       motion spiral direction
C*                        CCLW
C*                        CLW
C*          fspirl      spiral motion
C*                        OUT
C*                        IN
C*          fslift      lift tool between sections
C*                        YES
C*                        NO
C*          fcornr      type of motion on sharp corners
C*                        SHARP
C*                        ARC
C*          fslang      Slow down angle.
C*          frmpds      ramp distance
C*          fnumlv      number of levels to machine or step-down parameter
C*          flvdep      depth flag for the step-down parameter
C*          fretds      retract distance
C*          fmaxds      maximum distance to step over
C*          fminst      minimum distance to step over
C*          fhscal      scallop height for coverage testing
C*          fgenfr      general feedrate
C*          fposfr      positioning feedrate
C*          fretfr      retract feedrate
C*          fentfr      entry feedrate
C*          ftrnfr      transition feedrate
C*          ffinfr      finish feedrate
C*          fclrlv      clearance level amount
C*          cclrlv      clearance level PLANE ID
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pokpar (fnramp, fentry, fhldir, fkytxt, fnwrn, frtrct,
     x                   fcltyp, flcflg, fpdir, fspirl, flcdir, flcfin,
     x                   fslift, fcornr, farcrd, fslang, ftrans, fperim,
     x                   fisle, frmpds, fnumlv, flvdep, fretds, fhretd,
     x                   fvretd, fmaxst, fhscal, fminst, fgenfr, fposfr,
     x                   fretfr, fentfr, ftrnfr, ffinfr, ffrsfr,
     x                   fclrlv, cclrlv, fmxang,
     x                   fjctcm, fctcm1, fctcm2, fctcom)


      include 'com8a.com'
      include 'rrdm.com'
      include 'comgt.com'

      integer*2 fnramp, fentry, frtrct, fcltyp, fpdir, fspirl, fslift
      integer*2 fnwrn,fcornr,fjctcm,fctcm1,fctcm2,flvdep,ftrans,fhldir
      integer*2 fperim,fisle,flcflg,flcdir,flcfin
      integer*4 fkytxt
      real*4 fslang, farcrd, frmpds, fnumlv, fretds, fmaxst, fminst
      real*4 fgenfr, fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr
      real*4 fmxang, fhretd, fvretd, fhscal
      real*8 fclrlv
      character*80 cclrlv
      character*80 fctcom
      integer*2 left/8/,right/24/,on/71/
      integer *2 xyplan/33/,yzplan/37/,zxplan/41/
      integer*2 len
      integer*4 strlen1

      fnramp = nramps
      fentry = entry
      fhldir = hldir
      fkytxt = keytxt
      fnwrn = nwrn
      frtrct = rtract
      fcltyp = cltyp
      flcflg = lctyp
      flcdir = lcdir
      flcfin = lcfin
      fpdir = pdir
      fspirl = ispirl
      fslift = slift
      fcornr = corner
      farcrd = arcrad
      fslang = slwang
      ftrans = transt
      fperim = perim
      fisle = isle
      frmpds = rmpdis
      fnumlv = numlvl
      flvdep = lvdep
      fretds = retdis
      fhretd = hrtdis
      fvretd = vrtdis
      fmaxst = maxstp
      fminst = minstp
      fhscal = hscalp
c
c...per Ken
c
      if (genfr.gt.0.0) then
          fgenfr = genfr
      else
          if (FEEDC(3).gt.0.0) then
              fgenfr = FEEDC(3)
          else
              fgenfr = 20
          endif
      endif
      fposfr = posfr
      fretfr = retrfr
      fentfr = entrfr
      ftrnfr = tranfr
      ffinfr = finfr
      ffrsfr = frsfr
      fmxang = maxang
      len = strlen1(cutcm)
      fctcom = cutcm
      fctcom(len+1:len+1) = char(0)
      fjctcm = 0
      fctcm1 = 3
      fctcm2 = 3
      if (jctcom .gt. 1) then
        fjctcm = 1
        if (jctcm1 .eq. left) then
          fctcm1 = 0
        else if (jctcm1 .eq. right) then
          fctcm1 = 1
        else if (jctcm1 .eq. on) then
          fctcm1 = 2
        endif
        if (jctcm2 .eq. xyplan) then
          fctcm2 = 0
        else if (jctcm2 .eq. yzplan) then
          fctcm2 = 1
        else if (jctcm2 .eq. zxplan) then
          fctcm2 = 2
        endif
      endif
      if (cltyp .ne. 1) then
          fclrlv = clrlvl
      else
          cclrlv = '                '
          call expnm2 (clrlv_lab, clrlv_inx, len, cclrlv)
      endif

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine pmdsav ()
C*       Saves current Advanced POCKET POKMOD parameters from rrdm.com
C*       Fortran common area.  It is used for Preview in the Advanced POCKET
C*       form.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pmdsav ()

      include 'rrdm.com'

      common/rrdmsv/ fctcom, fclrlv, flcvec, farcrd,
     x fslang, frmpds, fnumlv, fretds, fhretd, fvretd, fmaxst, fminst,
     x fgenfr, fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr, fhscal,
     x fmxang,fnramp, fentry, fnwrn, frtrct, fcltyp, fpdir, ftrans,
     x fperim, fisle, fspirl, fslift, fcornr, fjctcm, fctcm1, fctcm2,
     x flvdep, flcflg, flcdir, flcfin, fhldir

      integer*2 fnramp, fentry, fnwrn, frtrct, fcltyp, fpdir, ftrans
      integer*2 fspirl, fslift, fcornr, fjctcm, fctcm1, fctcm2, flvdep
      integer*2 fperim, fisle, flcflg, flcdir, flcfin, fhldir
      real*4 fslang, frmpds, farcrd, fhscal
      real*4 fnumlv,fretds, fmaxst, fminst, fmxang, fhretd, fvretd
      real*4 fgenfr, fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr
      real*8 fclrlv,flcvec
      character*80 fctcom

      fnramp = nramps
      fentry = entry
      fhldir = hldir
      fnwrn = nwrn
      frtrct = rtract
      fcltyp = cltyp
      fpdir = pdir
      fspirl = ispirl
      fslift = slift
      fcornr = corner
      farcrd = arcrad
      fslang = slwang
      ftrans = transt
      frmpds = rmpdis
      fnumlv = numlvl
      flvdep = lvdep
      fretds = retdis
      fhretd = hrtdis
      fvretd = vrtdis
      fmaxst = maxstp
      fhscal = hscalp
      fminst = minstp
      fgenfr = genfr
      fposfr = posfr
      fretfr = retrfr
      fentfr = entrfr
      ftrnfr = tranfr
      fperim = perim
      fisle = isle
      ffinfr = finfr
      ffrsfr = frsfr
      fmxang = maxang
      fjctcm = jctcom
      fctcm1 = jctcm1
      fctcm2 = jctcm2
      fctcom = cutcm
      fclrlv = clrlvl
      flcflg = lctyp
      flcdir = lcdir
      flcfin = lcfin
      flcvec = dirvec

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine pmdrst ()
C*       Saves current Advanced POCKET POKMOD parameters from rrdm.com
C*       Fortran common area.  It is used for Preview in the
C*       Advanced Pocket form.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pmdrst ()

      include 'rrdm.com'

      common/rrdmsv/ fctcom, fclrlv, flcvec, farcrd,
     x fslang, frmpds, fnumlv, fretds, fhretd, fvretd, fmaxst, fminst,
     x fgenfr, fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr, fhscal,
     x fmxang,fnramp, fentry, fnwrn, frtrct, fcltyp, fpdir, ftrans,
     x fperim, fisle, fspirl, fslift, fcornr, fjctcm, fctcm1, fctcm2,
     x flvdep, flcflg, flcdir, flcfin, fhldir

      integer*2 fnramp, fentry, fnwrn, frtrct, fcltyp, fpdir, ftrans
      integer*2 fspirl, fslift, fcornr, fjctcm, fctcm1, fctcm2, flvdep
      integer*2 fperim, fisle, flcflg, flcdir, flcfin, fhldir
      real*4 fslang, frmpds, farcrd, fhscal
      real*4 fnumlv,fretds, fmaxst, fminst, fmxang, fhretd, fvretd
      real*4 fgenfr, fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr
      real*8 fclrlv,flcvec
      character*80 fctcom

      nramps = fnramp
      entry = fentry
      hldir = fhldir
      nwrn = fnwrn
      rtract = frtrct
      cltyp = fcltyp
      pdir = fpdir
      ispirl = fspirl
      slift = fslift
      corner = fcornr
      arcrad = farcrd
      slwang = fslang
      transt = ftrans
      perim = fperim
      isle = fisle
      rmpdis = frmpds
      numlvl = fnumlv
      lvdep = flvdep
      retdis = fretds
      hrtdis = fhretd
      vrtdis = fvretd
      maxstp = fmaxst
      minstp = fminst
      genfr = fgenfr
      posfr = fposfr
      retrfr = fretfr
      entrfr = fentfr
      tranfr = ftrnfr
      finfr = ffinfr
      frsfr = ffrsfr
      maxang = fmxang
      jctcom = fjctcm
      jctcm1 = fctcm1
      jctcm2 = fctcm2
      cutcm = fctcom
      clrlvl = fclrlv
      lctyp = flcflg
      lcdir = flcdir
      lcfin = flcfin
      dirvec = flcvec

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine poksav ()
C*       Save current Advanced POCKET POKMOD parameters from Fortran
C*       common area.  It is used for Preview in the Advanced POCKET
C*       form.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine poksav (mxlp)
      integer*2 mxlp

      include 'com.com'
      include 'rrdm.com'
      common/pksav/ ifl2,ifl9,ifl10,ifl11,ifl12,ifl270,mxloop

      integer*2 ifl2,ifl9,ifl10,ifl11,ifl12,ifl270,mxloop

      ifl2 = ifl(2)
      ifl9 = ifl(9)
      ifl10 = ifl(10)
      ifl11 = ifl(11)
      ifl12 = ifl(12)
      ifl270 = ifl(270)
      mxloop = nloops
      if (mxlp .gt. 0) nloops = mxlp
      ifl(216) = 1

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokrst ()
C*       Reset current Advanced POCKET POKMOD parameters from Fortran
C*        common area.  It is used for Preview in the Lace POCKET form.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pokrst ()

      include 'com.com'
      include 'rrdm.com'
      common/pksav/ ifl2,ifl9,ifl10,ifl11,ifl12,ifl270,mxloop

      integer*2 ifl2,ifl9,ifl10,ifl11,ifl12,ifl270,mxloop

      ifl(2) = ifl2
      ifl(9) = ifl9
      ifl(10) = ifl10
      ifl(11) = ifl11
      ifl(12) = ifl12
      ifl(270) = ifl270
      nloops = mxloop
      ifl(216) = 0

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine pokfrm ()
C*       Loads Advanced Pocket form  parameters from rrdm.com
C*       Fortran common area.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine pokfrm (fclik,fdirp,fdiri,fttyp,ftdis,fetyp,feelm,
     x                   foffp,fltyp,flops,ffthk)

      include 'rrdm.com'

      integer*2 fclik,fdirp,fdiri,fttyp,fetyp,feelm,foffp,fltyp,flops
      real*4 ftdis,ffthk

      fclik = pclik
      fdirp = dirp
      fdiri = diri
      fttyp = ttyp
      fetyp = etyp
      if (lctyp .gt. 0) fetyp = sttyp
      feelm = eelm
      foffp = offprt
      fltyp = ltyp
      flops = lops
      ftdis = tdis
      ffthk = finthk

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine watfrm ()
C*       Loads Waterline Roughing form parameters from rrdm.com
C*       Fortran common area.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************

      subroutine watfrm (fclik,flayn,fbtyp,fttyp,fstock,fltyp,flops,
     x                   fbotp,ftopp,fstkp,fgap,ffrom,foffp,fpoff,fztyp,
     x                                           fzlst,fdeep,fstep)

      include 'rrdm.com'

      real*4 fbotp,ftopp,fstkp,fgap,fstep
      integer*2 fclik,flayn,fbtyp,fttyp,fstock,fltyp,flops,ffrom,foffp
      integer*2 fztyp,fzlst,fdeep

      fclik = wclik
      flayn = wlayn
      fbtyp = wbtyp
      fttyp = wttyp
      fstock = wstock
      fbotp = wbotp
      ftopp = wtopp
      fstkp = wstkp
      fltyp = wltyp
      flops = wlops
      fgap = wgap
      ffrom = wfrom
      foffp = offprt
      fpoff = offdis
      fztyp = wztyp
      fzlst = wzlst
      fdeep = wdeep
      fstep = wstep

      return
      end

C*********************************************************************
C*    E_SUBROUTINE     : subroutine wgtfinis (ffinis,fdup,fddn)
C*       Get 'FINISH,LEVEL' Waterline Roughing parameters from
C*       the common area rrdm.com.
C*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
      subroutine wgtfinis (ffinis,fdup,fddn)

      include 'rrdm.com'

      integer*2 ffinis
      real*4 fdup,fddn

      ffinis = wfinis
      fddn = adjdn
      fdup = adjup

      return
      end
