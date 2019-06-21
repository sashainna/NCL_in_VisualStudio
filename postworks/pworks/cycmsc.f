c
c***********************************************************************
c
c   FILE NAME:  cycmsc
c   CONTAINS:
c               cycctl  cycoff  cyc2nd  cymjig  cycmax
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cycmsc.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:30:43
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cycctl (kfl)
c
c   FUNCTION:  This is the controlling routine for both Mill and lathe
c              Automatic and Post generated cycles.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  1-2 = Automatic cycles.
c                                  3   = Unidirectional move.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cycctl (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYCSV,KPOSMP(0291)), (XFMFL ,KPOSMP(0969))
      equivalence (SPITAX,KPOSMP(1279))
      equivalence (IFITYP,KPOSMP(3150)), (CYCPAX,KPOSMP(0299))
c
      integer*4 ICYCSW(5),ICYCFL(30),CYCCOD(20),ICYCDO(15),SPITAX,
     1          ICYCSV(5),IFITYP,CYCPAX,XFMFL(20)
c
      equivalence (ISEQ  ,POSMAP(1164))
      equivalence (MCHNUM,POSMAP(1287)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (CYCVR ,POSMAP(2926)), (RCYCDO,POSMAP(2931))
      equivalence (CYCPSV,POSMAP(2951)), (PFEED ,POSMAP(3540))
      equivalence (SPIVEC,POSMAP(3583))
      equivalence (SPICVC,POSMAP(3586)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),TLVEC(3),ROTANG(20,2),RCYCDO(20),AXSOUT(10),
     1       CYCVR(5),CYCPSV(10),PFEED(4),ISEQ,SPICVC(3),SPIVEC(3)
c
      integer*4 kfl
c
      integer*4 iman,is1,is2,ispt(3,3),isw(3),iaxfl(10),icnt
c
      real*8 rmch(3,4),rlin(6),raxs(10),ttv(3),rnum
c
      data ispt /1,2,3, 3,1,2, 2,3,1/
c
c...Clamp axes if necessary
c
      call whchax (AXSOUT,iaxfl,icnt)
      call clmput (AXSOUT,iaxfl)
c
c...Second time here
c...Return to appropriate routine
c
      if (kfl .eq. 1 .or. kfl .eq. 2) go to 100
      if (XFMFL(10) .eq. 1 .and. XFMFL(11) .eq. 1) then
          call ptxfr (SPIVEC,ttv,2)
          call cycmax (ttv,CYCPAX)
      else
          call pivvec (ROTANG,SPICVC)
          call cycmax (SPICVC,CYCPAX)
      endif
c
c...Output TMARK block
c
      if (ICYCDO(10) .ne. 0) then
          if (ICYCDO(10) .eq. 1) then
              call tmset (ISEQ,1)
          else
              ICYCDO(11) = ICYCDO(11) + 1
              if (ICYCDO(11) .gt. RCYCDO(16)) then
                  call tmset (RCYCDO(14),3)
                  ICYCDO(11) = 1
              endif
          endif
      endif
c
c...Perform jig move
c
      if (ICYCFL(2) .eq. 1 .and. CYCVR(1) .ne. 0. .or. kfl .eq. 3) then
          call cymjig (kfl)
          if (kfl .ne. 0) go to 8000
      endif
c
c...Determine whether to output
c...Automatic or Manual cycles
c
      if (ICYCSW(1) .eq. 3) then
          ICYCSW(1) = 1
          ICYCSW(2) = 1
      endif
c
c......Automatic cycle
c......Make sure non-cycle axes do not move
c......during cycle operation
c
      if (ICYCSW(1) .eq. 1) then
          iman   = 0
c
c.........Cycle not supported by control
c
          if (ICYCDO(1) .eq. 14 .or. CYCCOD(ICYCDO(1)+1) .eq. 0) then
              iman   = 1
c
c.........Output manual cycle if not XY plane and cycles only
c.........allowed in XY plane.
c
          else if (ICYCFL(3).eq.2 .and. CYCPAX.ne.1) then
              iman = 1
c
c.........Assign cycle axis pointers
c
          else if (XFMFL(10) .ne. 1) then
              is1    = ispt(1,CYCPAX)
              is2    = ispt(2,CYCPAX)
c
c.........Adjust tool end point
c.........at final depth to machine coordinates
c
              rnum   = RCYCDO(1)
              if (rnum .lt. 1.) rnum = 1.
              call gttvec (ttv,ROTANG(1,1))
              rmch(1,2) = MCHNUM(1,2) - rnum * ttv(1)
              rmch(2,2) = MCHNUM(2,2) - rnum * ttv(2)
              rmch(3,2) = MCHNUM(3,2) - rnum * ttv(3)
              call alladj (rmch,rlin,raxs,ROTANG,2,5)
c
c.........Determine which axes move
c
              call whchmv (AXSOUT,raxs,isw)
              if (isw(is1) .eq. 1 .or. isw(is2) .eq. 1) iman = 1
          endif
c
c.........3-axis move
c.........Cancel cycle and put in manual mode
c
          if (iman .eq. 1) then
              if (ICYCDO(1) .ne. 14) call cycoff (1)
              IFITYP = ICYCSV(2)
              if (IFITYP .ne. 5) then
                  PFEED(IFITYP) = CYCPSV(5)
              else
                  RCYCDO(2) = CYCPSV(5)
              endif
              ICYCSW(1) = 3
          endif
      endif
c
c...Automatic cycles
c
      if (ICYCSW(1) .ne. 1) go to 200
  100 call cymaut (kfl)
      go to 8000
c
c...Manual cycles
c
  200 call cymman (kfl)
c
c...End of routine
c
 8000 if (ICYCSW(2) .ne. 3) ICYCSW(2) = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycoff (kfl)
c
c   FUNCTION:  This routine outputs a cycle off block.
c
c   INPUT:  kfl     I*4  D1   -  1 = Output CYCLE/OFF block, 2 = Do not.
c                                3 = Only output CYCLE/OFF block, but
c                                do not internally cancel the cycle.
c                                Typically used with Macro style cycles.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cycoff (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (CYCCOD,KPOSMP(0211)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (ICYCSV,KPOSMP(0291))
      equivalence (MACCOD,KPOSMP(0801)), (ICYMTP,KPOSMP(0920))
      equivalence (MOTCOD,KPOSMP(1240))
      equivalence (ICYBLK,KPOSMP(3060)), (IFITYP,KPOSMP(3150))
c
      integer*4 CYCCOD(20),ICYBLK(20),ICYCFL(30),MOTCOD,ICYCSW(5),
     1          ICYCSV(5),IFITYP,ICYCDO(15),MACCOD(5),ICYMTP
c
      equivalence (MACCDV,POSMAP(1184))
      equivalence (MOTCDV,POSMAP(1284)), (CYCCDV,POSMAP(2901))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (RPMSAV,POSMAP(3305)), (SFMSAV,POSMAP(3306))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 CYCCDV(25),MOTCDV,CYCPSV(10),PFEED(4),RCYCDO(20),RPMSAV,
     1       SFMSAV,RPM,SFM,MACCDV(5)
c
      integer*4 kfl
c
      integer*4 ifrc
c
c...Output cycle off block
c
      if (ICYCFL(21) .eq. 1) then
          if (kfl .eq. 1 .or. kfl .eq. 3) then
              if (ICYCFL(30) .eq. 1 .and. ICYMTP .eq. 1)
     1            call codout (MACCOD(1),MACCDV(1))
              call codout (CYCCOD(1),CYCCDV(1))
              call frcblk (ICYBLK(1),ifrc,1)
              ICYMTP = 0
              if (kfl .eq. 3) go to 8000
          endif
c
c...Manual BORE9 cycle in effect
c...Restart spindle
c
      else if (ICYCSW(1) .ne. 0 .and. ICYCDO(1) .eq. 4) then
          RPM   = RPMSAV
          SFM   = SFMSAV
          call spndon
      endif
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
          else
              CYCPSV(5) = RCYCDO(2)
          endif
          IFITYP = ICYCSV(1)
          PFEED(IFITYP) = CYCPSV(3)
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
c   SUBROUTINE: cyc2nd (kfl)
c
c   FUNCTION:  This routine pops the last cycle position from the stack
c              (optional) and resets cycle parameters after the cycle
c              was cancelled internally for a positioning move (jig
c              move, etc.).
c
c   INPUT:  kfl     I*4  D1  -  1 = Pop cycle postion from stack.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cyc2nd (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCSV,KPOSMP(0291))
      equivalence (IFITYP,KPOSMP(3150))
c
      integer*4 ICYCSW(5),IFITYP,ICYCSV(5)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (PFEED ,POSMAP(3540)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       RCYCDO(20),PFEED(4),CYCPSV(10)
c
      integer*4 kfl
c
      integer*4 iaxsw(10),icnt
c
c...Pop previous cycle position from stack &
c...Reset cycle parameters
c
      if (kfl .eq. 1)
     1        call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iaxsw,icnt)
      call raprst
      IFITYP = ICYCSV(2)
      if (IFITYP .ne. 5) then
          PFEED(IFITYP) = CYCPSV(5)
      else
          RCYCDO(2) = CYCPSV(5)
      endif
      ICYCSW(1) = ICYCSV(3)
      ICYCSW(2) = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymjig (kfl)
c
c   FUNCTION:  This routine is used to position above a cycle hole
c              always in the same direction (jig move).
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  3   = A unidirectional move has been
c                                        generated and is ready for
c                                        output.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: kfl     I*4  D1  -  See Input.
c
c***********************************************************************
c
      subroutine cymjig (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (SPITAX,KPOSMP(1279))
c
      integer*4 ICYCFL(30),ICYCSW(5),SPITAX,ICYCDO(15)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (CYCVR ,POSMAP(2926))
      equivalence (RCYCDO,POSMAP(2931)), (CYCJVC,POSMAP(2961))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),TLVEC(3),STONUM(3,4),
     1       RCYCDO(20),AXSOUT(10),CYCVR(5),CYCJVC(3)
c
      integer*4 kfl
c
      integer*4 ierr
c
      real*8 rdis,pln(4)
c
c...2nd time here
c...Pop previous position from stack
c
      if (kfl .ne. 0) then
          call cyc2nd (1)
          kfl    = 0
          go to 8000
      endif
c
c...Get clearance distance for cycle
c......Using RTRCTO parameter
c
      if (ICYCFL(26) .eq. 1 .and. ICYCDO(8) .ge. 1 .and.
     1    (ICYCSW(21) .eq. 1 .or. (ICYCDO(1) .ne. 5 .and.
     2    ICYCDO(1) .ne. 13))) then
              rdis   = RCYCDO(11)
c
c......Clearance plane
c
      else if (ICYCFL(9) .eq. 2 .or. (ICYCFL(9) .eq. 1 .and.
     1         ICYCSW(3) .eq. 1)) then
          call plnvpt (TLVEC,MCHNUM(1,2),pln,ierr)
          call plndis (pln,STONUM(1,2),rdis)
          if (rdis .lt. RCYCDO(4)) rdis = RCYCDO(4)
c
c......Rapto plane
c
      else
          rdis   = RCYCDO(4)
      endif
c
c...Save current position
c
      call pshaxs (AXSOUT,TLVEC)
c
c...Define clearance position
c
      MCHNUM(1,2) = MCHNUM(1,2) + rdis   * TLVEC(1)
      MCHNUM(2,2) = MCHNUM(2,2) + rdis   * TLVEC(2)
      MCHNUM(3,2) = MCHNUM(3,2) + rdis   * TLVEC(3)
c
c...Adjust points to machine system
c
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,2,3)
c
c...Apply shift to points
c
      MCHNUM(1,4) = MCHNUM(1,4) - CYCJVC(1) * CYCVR(1)
      MCHNUM(2,4) = MCHNUM(2,4) - CYCJVC(2) * CYCVR(1)
      MCHNUM(3,4) = MCHNUM(3,4) - CYCJVC(3) * CYCVR(1)
c
c...Set up points for output
c
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
      call cycoff (1)
      call rapset (5,1)
      kfl    = 3
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycmax (gvec,kax)
c
c   FUNCTION:  This routine is used to define the major cycle axis.
c
c   INPUT:  gvec    R*8  D3  -  Spindel axis vector.
c
c   OUTPUT: kax     I*4  D1  -  Major cycle axis index (1 - XY plane,
c                               2 - ZX plane, 3 - YZ plane).
c
c***********************************************************************
c
      subroutine cycmax (gvec,kax)
c
      include 'post.inc'
c
      equivalence (IZSWIV,KPOSMP(1224))
c
      integer*4 IZSWIV
c
      real*8 gvec(3)
      integer*4 kax
c
      real*8 rvec(3)
c
c...Get major part of spindle vector
c
      call ptxfm (gvec,rvec,2)
      kax    = 1
      if (IZSWIV .ne. 1) then
          if (dabs(rvec(1)) .gt. dabs(rvec(3))) kax = 3
          if (dabs(rvec(2)) .gt. dabs(rvec(4-kax))) kax = 2
      endif
c
      return
      end
