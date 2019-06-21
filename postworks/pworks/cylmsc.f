c
c***********************************************************************
c
c   FILE NAME:  cylmsc
c   CONTAINS:
c               cylctl  cyloff  thrmot
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cylmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:06
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cylctl (kfl)
c
c   FUNCTION:  This is the controlling routine for Lathe Automatic and
c              Post generated cycles.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  1   = Automatic cycles.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylctl (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYCSV,KPOSMP(0291))
      equivalence (CYLCOD,KPOSMP(1921)), (IFITYP,KPOSMP(3150))
c
      integer*4 ICYCSW(5),CYLCOD(25),ICYCDO(15),ICYCSV(5),IFITYP
c
      equivalence (CYCPSV,POSMAP(2951)), (PFEED ,POSMAP(3540))
c
      real*8 CYCPSV(10),PFEED(4)
c
      integer*4 kfl
c
      integer*4 iman
c
c...Determine whether to output
c...Automatic or Manual cycles
c
      if (kfl .eq. 1) go to 100
      if (ICYCSW(1) .eq. 3) then
          ICYCSW(1) = 1
          ICYCSW(2) = 1
      endif
c
c......Automatic cycle
c......Make sure cycle is supported
c
      if (ICYCSW(1) .eq. 1) then
          iman   = 0
c
c.........Cycle not supported by control
c.........Cancel cycle and put in manual mode
c
          if (CYLCOD(ICYCDO(1)+4) .eq. 0) then
              call cyloff
              IFITYP = ICYCSV(2)
              if (IFITYP .ne. 5) then
                  PFEED(IFITYP) = CYCPSV(5)
              endif
              ICYCSW(1) = 3
          endif
      endif
c
c...Automatic cycles
c
      if (ICYCSW(1) .ne. 1) go to 200
  100 call cylaut (kfl)
      go to 8000
c
c...Manual cycles
c
  200 call cylman
c
c...End of routine
c
 8000 if (ICYCSW(2) .ne. 3) ICYCSW(2) = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cyloff
c
c   FUNCTION:  This routine cancels lathe cycles and threadcutting.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cyloff
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCSV,KPOSMP(0291))
      equivalence (CYLCOD,KPOSMP(1921)), (ICYLBK,KPOSMP(3080))
      equivalence (IFITYP,KPOSMP(3150)), (POSFED,KPOSMP(3209))
c
      integer*4 ICYCSW(5),ICYCSV(5),IFITYP,POSFED,CYLCOD(25),ICYLBK(20)
c
      equivalence (CYCPSV,POSMAP(2951)), (CYLCDV,POSMAP(2968))
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 CYCPSV(10),PFEED(4),CYLCDV(25)
c
      integer*4 ifrc
c
c...Reset saved settings
c
      if (ICYCSW(1) .ne. 0) then
          ICYCSV(3) = ICYCSW(1)
c
c......Feedrate
c
          ICYCSV(2) = IFITYP
          if (IFITYP .ne. 5) then
              CYCPSV(5) = PFEED(IFITYP)
          endif
          IFITYP = ICYCSV(1)
          PFEED(IFITYP) = CYCPSV(3)
          POSFED = 1
c
c....Output CYCLE/OFF block
c
          if (ICYCSW(1) .ne. 4) then
              call codout (CYLCOD(4),CYLCDV(4))
              call frcblk (ICYLBK(1),ifrc,1)
          endif
      endif
c
c...Turn cycles off
c
      ICYCSW(1) = 0
      ICYCSW(2) = 3
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  thrmot (glk,gli,kvar,gvar,kfl)
c
c   FUNCTION:  This routine outputs a single pass threadcutting move.
c
c   INPUT:  glk     R*8  D1  -  Z-axis thread lead value.
c
c           gli     R*8  D1  -  X-axis thread lead value.
c
c           kvar    I*4  D1  -  0 = Consant lead thread, 1 = Increasing
c                               variable lead thread, 2 = Decreasing
c                               variable lead thread.
c
c           gvar    R*8  D1  -  Variable lead value when 'kvar' is not
c                               equal to 0.
c
c           kfl     R*8  D1  -  1 = Modal threadcutting.  Do not cancel
c                               after this move.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine thrmot (glk,gli,kvar,gvar,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SIMACT,KPOSMP(0174)), (ICYCSW,KPOSMP(0271))
      equivalence (CYLCOD,KPOSMP(1921)), (CYLREG,KPOSMP(1946))
      equivalence (REGFRC,KPOSMP(0603)), (IFITYP,KPOSMP(3150))
      equivalence (IFOTYP,KPOSMP(3151)), (POSFED,KPOSMP(3209))
c
      integer*4 ICYCSW(5),IFITYP,CYLCOD(25),CYLREG(25),POSFED,IFOTYP,
     1          REGFRC(MAXFMT),SIMACT
c
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (CYLCDV,POSMAP(2968)), (PFEED ,POSMAP(3540))
c
      real*8 PFEED(4),AXSOUT(10),CYLCDV(25)
c
      integer*4 kvar,kfl
c
      real*8 glk,gli,gvar
c
      integer*4 inum,icnt,ifl(10)
c
      real*8 fed1,fed2
c
c...Output threading codes
c......Definition code
c
      inum   = 1
      if (kvar .eq. 1 .and. CYLCOD(2) .ne. 0) inum = 2
      if (kvar .eq. 2 .and. CYLCOD(3) .ne. 0) inum = 3
      if (CYLCOD(inum) .ne. 0. .and. ICYCSW(2) .eq. 1 .and.
     1    REGFRC(CYLCOD(inum)) .eq. 0) REGFRC(CYLCOD(inum)) = 1
      call codout (CYLCOD(inum),CYLCDV(inum))
c
c......Lead codes
c
      if (glk .ne. 0) then
          if (CYLREG(1) .ne. 0. .and. ICYCSW(2) .eq. 1 .and.
     1        REGFRC(CYLREG(1)) .eq. 0) REGFRC(CYLREG(1)) = 1
          call codout (CYLREG(1),glk)
      endif
c
      if (gli .ne. 0) then
          if (CYLREG(2) .ne. 0. .and. ICYCSW(2) .eq. 1 .and.
     1        REGFRC(CYLREG(2)) .eq. 0) REGFRC(CYLREG(2)) = 1
          call codout (CYLREG(2),gli)
      endif
c
c......Variable lead code
c
      if (kvar .eq. 1 .and. gvar .ne. 0) then
          if (CYLREG(3) .ne. 0. .and. ICYCSW(2) .eq. 1 .and.
     1        REGFRC(CYLREG(3)) .eq. 0) REGFRC(CYLREG(3)) = 1
          call codout (CYLREG(3),gvar)
      endif
c
      if (kvar .eq. 2 .and. gvar .ne. 0) then
          if (CYLREG(4) .ne. 0. .and. ICYCSW(2) .eq. 1 .and.
     1        REGFRC(CYLREG(4)) .eq. 0) REGFRC(CYLREG(4)) = 1
          call codout (CYLREG(4),gvar)
      endif
c
c...Set feedrate
c
      if (SIMACT .ne. 0) then
          IFITYP = 2
          IFOTYP = 2
      else
          IFITYP = 5
          IFOTYP = 5
      endif
      fed1   = glk
      if (fed1 .gt. 1.) fed1 = 1. / fed1
      fed2   = gli
      if (fed2 .gt. 1.) fed2 = 1. / fed2
      if (fed2 .ne. 0.) fed1 = fed1 + (fed2-fed1)/2.
      PFEED(2) = fed1
c
c...Output motion block
c
      if (SIMACT .eq. 0) POSFED = 2
      call whchax (AXSOUT,ifl,icnt)
      call motion (ifl,icnt)
c
c...Cancel threading cycle
c
      ICYCSW(2) = 0
      if (kfl .eq. 0) call cyloff
c
c...End of routine
c
 8000 return
      end
