c
c***********************************************************************
c
c   FILE NAME:  lodsup
c   CONTAINS:
c               endseq  stlcod  tlncod  tlnoff  tlnon   tlwhen
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        lodsup.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:07
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  endseq (kfl)
c
c   FUNCTION:  This routine marks an end of sequence by performing the
c              following functions.
c
c                  1) Resets sequence deltas and times is requested.
c                  2) Internally turns off COOLNT and SPINDL.
c
c   INPUT:  kfl     I*4  D1  -  1 = Reset sequence variables.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine endseq (kfl)
c
      include 'post.inc'
c
      equivalence (SPNDCD,KPOSMP(3105)), (COOLCD,KPOSMP(3128))
      equivalence (IZIGON,KPOSMP(0370)), (IZIGAD,KPOSMP(0371))
c
      integer*4 COOLCD(4),SPNDCD(4),IZIGON,IZIGAD
c
      equivalence (ZIGDEP,POSMAP(0190)), (BRKPRM,POSMAP(1215))
      equivalence (MCHDLS,POSMAP(1221)), (ROTDLS,POSMAP(1233))
      equivalence (AXSDLS,POSMAP(1237)), (LINDLS,POSMAP(1594))
      equivalence (SPNDVL,POSMAP(3007)), (COOLVL,POSMAP(3294))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
      equivalence (SEQTIM,POSMAP(3558)), (ACCFED,POSMAP(4291))
      equivalence (SGMDLS,POSMAP(5489))
c
      real*8 COOLVL(4),SPNDVL(4),RPM,SFM,MCHDLS(3,4),ROTDLS(4),
     1       ZIGDEP,AXSDLS(10),LINDLS(6),SEQTIM,BRKPRM(5),ACCFED,
     2       SGMDLS(6,3)
c
      integer*4 kfl
c
      integer*4 i
c
c...Turn off COOLNT and SPINDL
c
      if (kfl .eq. 1) then
          call setcod (COOLCD(4),COOLVL(4))
          call setcod (SPNDCD(3),SPNDVL(3))
          RPM   = 0.
          SFM   = 0.
c
c...Reset delta sequences &
c...sequence machining time
c
          do 100 i=1,4,1
              MCHDLS(1,i) = 0.
              MCHDLS(2,i) = 0.
              MCHDLS(3,i) = 0.
  100     continue
c
          do 200 i=1,6,1
              LINDLS(i) = 0.
              SGMDLS(i,1) = 0.
              SGMDLS(i,2) = 0.
  200     continue
c
          do 300 i=1,4,1
              ROTDLS(i) = 0.
  300     continue
c
          do 400 i=1,10,1
              AXSDLS(i) = 0.
  400     continue
c
          SEQTIM = 0.
      endif
c
c...Reset BREAK machining time & delta
c
      BRKPRM(2) = 0.
      BRKPRM(3) = 0.
c
c...Turn Zigzag mode off
c
      IZIGON = 0
      IZIGAD = 2
      ZIGDEP = 0.
c
c...Reset previous acceleration feed rate
c
      ACCFED = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  stlcod (gtool)
c
c   FUNCTION:  This routine outputs a SELCTL code.
c
c   INPUT:  gtool   R*8  D1  -  Tool number to select.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine stlcod (gtool)
c
      include 'post.inc'
c
      equivalence (TOOLFL,KPOSMP(1804)), (TLCCD ,KPOSMP(1839))
      equivalence (SELGRP,KPOSMP(1873)), (TOFNXT,KPOSMP(1877))
c
      integer*4 TOOLFL(20),TLCCD(20),SELGRP,TOFNXT
c
      equivalence (CTOFRG,POSMAP(2401)), (TLCVL ,POSMAP(3963))
      equivalence (TLOFRG,POSMAP(3991)), (TSELTM,POSMAP(3992))
c
      real*8 TLCVL(20),CTOFRG,TLOFRG,TSELTM
c
      real*8 gtool
c
c...Output Pick-up tool sequence
c
      call codout (TLCCD(SELGRP),TLCVL(SELGRP))
      call tlncod (gtool,gtool,TLOFRG,CTOFRG,2)
      if (TOOLFL(11) .eq. 1) call clrbuf
      TOFNXT = 0
      TSELTM = 0.
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tlncod (gtc,gts,gto,gco,kfl)
c
c   FUNCTION:  This routine outputs a register that contains multiple
c              tool and offset values.
c
c   INPUT:  gtc     R*8  D1  -  Tool number to load.
c
c           gts     R*8  D1  -  Tool number to select.
c
c           gto     R*8  D1  -  Tool length offset register.
c
c           gco     R*8  D1  -  Cutter compensation offset register.
c
c           kfl     I*4  D1  -  1 = Main output is LOADTL, 2 = SELCTL,
c                               3 = Tool length offset, 4 = Cutter
c                               compensation offset.
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tlncod (gtc,gts,gto,gco,kfl)
c
      include 'post.inc'
c
      equivalence (TLDIG ,KPOSMP(1829)), (TLCCD ,KPOSMP(1839))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOSGN,KPOSMP(1869))
      equivalence (TLOFDR,KPOSMP(1876)), (CUTCCD,KPOSMP(3271))
c
      integer*4 TLDIG(2,5),TLCCD(20),TLOCD(8),TLOSGN,TLOFDR,CUTCCD(30)
c
      integer*4 kfl
c
      real*8 gtc,gts,gto,gco
c
      integer*4 imx
c
      real*8 tnum
c
c...LOADTL
c
      if (kfl .eq. 1 .and. TLDIG(1,2) .eq. 0) then
          call codout (TLCCD(4),gtc)
c
c...SELCTL
c
      else if (kfl .eq. 2 .and. TLDIG(1,1) .eq. 0) then
          call codout (TLCCD(3),gts)
c
c...TOOLNO/ADJUST
c
      else if (kfl .eq. 3 .and. TLDIG(1,3) .eq. 0) then
          tnum   = gto
          if (TLOSGN .eq. 1 .and. TLOFDR .eq. 2) tnum = 0. - tnum
          call codout (TLOCD(1),tnum)
c
c...CUTCOM
c
      else if (kfl .eq. 4 .and. TLDIG(1,4) .eq. 0) then
          tnum   = gco
          call codout (CUTCCD(4),gco)
c
c...Combination LOADTL
c
      else if (kfl .eq. 1) then
c
c......Get maximum number of digits in code
c
          imx    = TLDIG(2,2)
c
          if (TLOCD(1) .eq. TLCCD(4) .and. TLDIG(2,3) .ne. 0 .and.
     1        TLDIG(2,3) .gt. imx) imx = TLDIG(2,3)
          if (CUTCCD(4) .eq. TLCCD(4) .and. TLDIG(2,4) .ne. 0 .and.
     1        TLDIG(2,4) .gt. imx) imx = TLDIG(2,4)
c
c......Build code
c
          tnum   = gtc * 10**(imx-TLDIG(2,2))
          if (TLOCD(1) .eq. TLCCD(4) .and. TLDIG(1,3) .ne. 0)
     1            tnum = tnum + (gto * 10**(imx-TLDIG(2,3)))
          if (CUTCCD(4) .eq. TLCCD(4) .and. TLDIG(1,4) .ne. 0)
     1            tnum = tnum + (gco * 10**(imx-TLDIG(2,4)))
          call codout (TLCCD(4),tnum)
c
c...Combination SELCTL
c
      else if (kfl .eq. 2) then
c
c......Get maximum number of digits in code
c
          imx    = TLDIG(2,1)
c
          if (TLOCD(1) .eq. TLCCD(3) .and. TLDIG(2,3) .ne. 0 .and.
     1        TLDIG(2,3) .gt. imx) imx = TLDIG(2,3)
          if (CUTCCD(4) .eq. TLCCD(3) .and. TLDIG(2,4) .ne. 0 .and.
     1        TLDIG(2,4) .gt. imx) imx = TLDIG(2,4)
c
c......Build code
c
          tnum   = gtc * 10**(imx-TLDIG(2,1))
          if (TLOCD(1) .eq. TLCCD(3) .and. TLDIG(1,3) .ne. 0)
     1            tnum = tnum + (gto * 10**(imx-TLDIG(2,3)))
          if (CUTCCD(4) .eq. TLCCD(3) .and. TLDIG(1,4) .ne. 0)
     1            tnum = tnum + (gco * 10**(imx-TLDIG(2,4)))
          call codout (TLCCD(3),tnum)
c
c...Combination TOOLNO
c
      else if (kfl .eq. 3) then
c
c......Get maximum number of digits in code
c
          imx    = TLDIG(2,3)
c
          if (TLOCD(1) .eq. TLCCD(4) .and. TLDIG(2,2) .ne. 0 .and.
     1        TLDIG(2,2) .gt. imx) imx = TLDIG(2,2)
          if (TLOCD(1) .eq. CUTCCD(4) .and. TLDIG(2,4) .ne. 0 .and.
     1        TLDIG(2,4) .gt. imx) imx = TLDIG(2,4)
c
c......Build code
c
          tnum   = gto * 10**(imx-TLDIG(2,3))
          if (TLOCD(1) .eq. TLCCD(4) .and. TLDIG(1,2) .ne. 0)
     1            tnum = tnum + (gtc * 10**(imx-TLDIG(2,2)))
          if (TLOCD(1) .eq. CUTCCD(4) .and. TLDIG(1,4) .ne. 0)
     1            tnum = tnum + (gco * 10**(imx-TLDIG(2,4)))
          call codout (TLOCD(1),tnum)
c
c...Combination CUTCOM
c
      else if (kfl .eq. 4) then
c
c......Get maximum number of digits in code
c
          imx    = TLDIG(2,4)
c
          if (CUTCCD(4) .eq. TLCCD(4) .and. TLDIG(2,2) .ne. 0 .and.
     1        TLDIG(2,2) .gt. imx) imx = TLDIG(2,2)
          if (CUTCCD(4) .eq. TLOCD(1) .and. TLDIG(2,3) .ne. 0 .and.
     1        TLDIG(2,3) .gt. imx) imx = TLDIG(2,3)
c
c......Build code
c
          tnum   = gco * 10**(imx-TLDIG(2,4))
          if (CUTCCD(4) .eq. TLCCD(4) .and. TLDIG(1,2) .ne. 0)
     1            tnum = tnum + (gtc * 10**(imx-TLDIG(2,2)))
          if (CUTCCD(4) .eq. TLOCD(1) .and. TLDIG(1,3) .ne. 0)
     1            tnum = tnum + (gto * 10**(imx-TLDIG(2,3)))
          call codout (CUTCCD(4),tnum)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tlnoff
c
c   FUNCTION:  This routine outputs a tool length compensation off
c              block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tlnoff
c
      include 'post.inc'
c
      equivalence (TOOLFL,KPOSMP(1804))
      equivalence (TLDIG ,KPOSMP(1829)), (TLCCD ,KPOSMP(1839))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOBLK,KPOSMP(1868))
c
      integer*4 TLDIG(2,5),TLCCD(20),TLOCD(8),TLOBLK,TOOLFL(20)
c
      equivalence (CTOFRG,POSMAP(2401))
      equivalence (TLOVL ,POSMAP(3983)), (TLOBAS,POSMAP(3988))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
c
      real*8 TLOVL(5),TLOBAS,TLOFRG,LSTTN,CTOFRG
c
      integer*4 ierr
c
      character*80 msg
c
c...Output tool offset off code
c
      call codout (TLOCD(3),TLOVL(3))
c
c...Output tool register code
c
      TLOFRG = TLOBAS
      call tlncod (LSTTN,LSTTN,TLOFRG,CTOFRG,3)
c
c...Force tool offset block
c
      call pshblk (TLOBLK)
      if (TOOLFL(17) .eq. 1) call clrbuf
c
c...Output simulation block
c
      call simofs (-1,-1,0,0,msg,ierr)
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tlnon
c
c   FUNCTION:  This routine outputs a tool length compensation on block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tlnon
c
      include 'post.inc'
c
      equivalence (PLNCOD,KPOSMP(4222)), (TOOLFL,KPOSMP(1804))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOBLK,KPOSMP(1868))
      equivalence (TLOSGN,KPOSMP(1869)), (TLOFPL,KPOSMP(1875))
      equivalence (TLOFDR,KPOSMP(1876)), (TOFNXT,KPOSMP(1877))
c
      integer*4 PLNCOD(4),TOOLFL(20),TLOCD(8),TLOBLK,TLOSGN,TLOFPL,
     1          TLOFDR,TOFNXT
c
      equivalence (PLNCDV,POSMAP(2208)), (CTOFRG,POSMAP(2401))
      equivalence (TLOVL ,POSMAP(3983)), (TLOBAS,POSMAP(3988))
      equivalence (LSTTN ,POSMAP(3990)), (TLOFRG,POSMAP(3991))
c
      real*8 PLNCDV(3),TLOVL(5),TLOFRG,CTOFRG,LSTTN,TLOBAS
c
      integer*4 ipln(3),ireg,imod,ierr
c
      character*80 msg
c
      data ipln /3,2,1/
c
c...Output plane selection code
c
      if (TOOLFL(14) .eq. 1) call codout (PLNCOD(TLOFPL),
     1                                    PLNCDV(TLOFPL))
c
c...Output tool offset on registers
c
      call codout (TLOCD(2),TLOVL(2))
      if (TLOFDR .eq. 1) call codout (TLOCD(4),TLOVL(4))
      if (TLOFDR .eq. 2 .and. TLOSGN .ne. 1)
     1        call codout (TLOCD(5),TLOVL(5))
c
c...Output tool register code
c
      call tlncod (LSTTN,LSTTN,TLOFRG,CTOFRG,3)
c
c...Force tool offset block
c
      call pshblk (TLOBLK)
      if (TOOLFL(16) .eq. 1) call clrbuf
c
c...Output simulation block
c
      imod   = TLOFDR
      if (imod .eq. 0) imod = 1
      ireg   = TLOFRG - TLOBAS
      call simofs (-1,-1,imod,ireg,msg,ierr)
c
c...End of routine
c
 8000 TOFNXT = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tlwhen (kfl)
c
c   FUNCTION:  This routine determines when to output a select tool
c              sequence and tool offset register depending on which axes
c              move and how long the move takes.
c
c   INPUT:  kfl     I*4  D10 -  1 = This axis is waiting to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine tlwhen (kfl)
c
      include 'post.inc'
c
      equivalence (SPITAX,KPOSMP(1279)), (TOFNXT,KPOSMP(1877))
c
      integer*4 SPITAX,TOFNXT
c
      equivalence (FMVTIM,POSMAP(3553)), (TSELN ,POSMAP(3989))
      equivalence (TSELTM,POSMAP(3992))
c
      real*8 FMVTIM,TSELN,TSELTM
c
      integer*4 kfl(10)
c
      integer*4 isub(2,3)
c
      data isub /1,2,3,4,5,6/
c
c...Output buffered SELCTL
c
      if (TSELTM .ne. 0. .and. FMVTIM .ge. TSELTM) call stlcod (TSELN)
c
c...Output buffered tool offset register
c...10/27/92 changed to '4-SPITAX' instead of SPITAX
c
      if (TOFNXT .eq. 1 .and. (kfl(isub(1,4-SPITAX)) .eq. 1 .or.
     1    kfl(isub(2,4-SPITAX)) .eq. 1)) call tlnon
c
c...End of routine
c
 8000 return
      end
