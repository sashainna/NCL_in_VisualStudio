c
c***********************************************************************
c
c   FILE NAME:  cymman
c   CONTAINS:
c               cymman  cymmdo  cympek  cymshf  popcyc  pshcyc
c               cymcir
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cymman.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        08/11/16 , 10:33:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cymman (kfl)
c
c   FUNCTION:  This is the controlling routine for post generated Mill
c              cycles.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  Not used at this time.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymman (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCSW(5),ICYCFL(30),ICYCDO(15)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),TLVEC(3),STONUM(3,4),
     1       RCYCDO(20),CYCPSV(10),AXSOUT(10),VECSAV(3)
c
      integer*4 kfl
c
      integer*4 is(3),ids(2,3),ierr,iret
c
      real*8 rfin(3),rpos(3),rclr(3),pln(4),rnum,ang
c
      data ids /2,3, 1,3, 1,2/
c
c...Set up cycle positions
c......Assign major tool axis direction
c
      is(3) = 3
      if (abs(TLVEC(1)) .gt. abs(TLVEC(is(3)))) is(3) = 1
      if (abs(TLVEC(2)) .gt. abs(TLVEC(is(3)))) is(3) = 2
      is(1) = ids(1,is(3))
      is(2) = ids(2,is(3))
c
c......Final position
c
      rfin(1) = MCHNUM(1,2) - RCYCDO(1) * TLVEC(1)
      rfin(2) = MCHNUM(2,2) - RCYCDO(1) * TLVEC(2)
      rfin(3) = MCHNUM(3,2) - RCYCDO(1) * TLVEC(3)
c
c......Rapto position
c
      rpos(1) = MCHNUM(1,2) + RCYCDO(4) * TLVEC(1)
      rpos(2) = MCHNUM(2,2) + RCYCDO(4) * TLVEC(2)
      rpos(3) = MCHNUM(3,2) + RCYCDO(4) * TLVEC(3)
c
c......Clearance position
c

      if (ICYCSW(2) .ne. 0) then
c
c.........RTRCTO overrides clearance plane
c
          if (ICYCFL(26) .eq. 1 .and. ICYCDO(8) .ge. 1 .and.
     1        ICYCDO(1) .ne. 13) then
              CYCPSV(4) = RCYCDO(11)
c
c.........Normal clearance plane
c
          else
              call plnvpt (TLVEC,MCHNUM(1,2),pln,ierr)
              call plndis (pln,STONUM(1,2),rnum)
              if (rnum .lt. RCYCDO(4)) rnum = RCYCDO(4)
              call betvec (TLVEC,VECSAV,ang)
              if (ang .gt. .001) then
                  if (rnum .lt. RCYCDO(4) .and. ICYCFL(26) .eq. 1 .and.
     1                ICYCDO(8) .ge. 1) rnum = RCYCDO(4)
                  if (rnum .lt. RCYCDO(11)) rnum = RCYCDO(11)
                  if (CYCPSV(4) .eq. 0.) CYCPSV(4) = rnum
              else
                  CYCPSV(4) = rnum
              endif
          endif
      endif
c
c.........Define clearance position
c
      rclr(1) = MCHNUM(1,2) + CYCPSV(4) * TLVEC(1)
      rclr(2) = MCHNUM(2,2) + CYCPSV(4) * TLVEC(2)
      rclr(3) = MCHNUM(3,2) + CYCPSV(4) * TLVEC(3)
c
c......Determine retract plane
c
      iret   = 1
      if (ICYCFL(9) .eq. 2 .or.
     1    (ICYCFL(9) .eq. 1 .and. ICYCSW(3) .eq. 1) .or.
     2    (ICYCFL(26) .eq. 1 .and. ICYCDO(8) .ge. 1 .and.
     3     ICYCDO(1) .ne. 5 .and. ICYCDO(1) .ne. 13)) iret = 2
cc      if (ICYCDO(1) .eq. 14) iret = 2
c
c...Perform cycle
c
      call cymmdo (is,rfin,rpos,rclr,iret)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymmdo (ks,gfin,gpos,gclr,kret)
c
c   FUNCTION:  This routine performs all of the post-generated Mill
c              cycles.
c
c   INPUT:  ks      I*4  D4  -  Subscript pointer to MCHNUM set up for
c                               the active cycle plan.
c
c           gfin    R*8  D3  -  The linear axes position at the bottom
c                               of the hole.
c
c           gpos    R*8  D3  -  The linear axes position at the rapto
c                               plane.
c
c           gclr    R*8  D3  -  The linear axes position at the clear-
c                               ance plane.
c
c           kret    I*4  D1  -  1 = Retract to rapto plane, 2 = retract
c                               to clearance plane.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymmdo (ks,gfin,gpos,gclr,kret)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (SPNDIR,KPOSMP(3141))
c
      integer*4 ICYCSW(5),ICYCDO(15),SPNDIR,ICYCFL(30)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (RCYCDO,POSMAP(2931)), (ROTANG,POSMAP(5173))
c
      real*8 RCYCDO(20),MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),
     1       ROTANG(20,2)
c
      integer*4 ks(3),kret
c
      real*8 gfin(3),gpos(3),gclr(3)
c
      integer*4 nfret,ifret(4),jindex,inc,ifl,istk(5),ipt,iclw,icclw
c
      data nfret /4/, ifret /1,9,10,12/
c
c...Initialize routine
c
      ipt    = 0
      inc    = jindex(ifret,ICYCDO(1),nfret)
c
      if (SPNDIR .ne. 2) then
          iclw   = 11
          icclw  = 12
      else
          iclw   = 12
          icclw  = 11
      endif
c
c...RETRCT/ON
c...Position to retract plane
c
      if (kret .eq. 2) then
c         if (ICYCDO(1) .eq. 4 .or. ICYCDO(1) .eq. 11) then
          if (ICYCDO(1) .eq. 4) then
              call cymshf (istk,ipt,gclr,1,1)
          else
              call pshcyc (istk,ipt,1,gclr)
          endif
      endif
c
c...Perform shift for BORE9
c
      ifl   = 1
      if (ICYCDO(1) .eq. 4) then
          call pshcyc (istk,ipt,9,gpos)
          call cymshf (istk,ipt,gpos,1,2)
          ifl   = 2
      endif
c
c...Position above hole (rapto plane)
c
      call pshcyc (istk,ipt,ifl,gpos)
c
c...Start spindle for BORE9
c
      if (ICYCDO(1) .eq. 4) call pshcyc (istk,ipt,iclw,gpos)
c
c...Lock feedrate & spindle overrides for
c...REVERS & TAP
c
      if ((ICYCDO(1) .eq. 10 .or. ICYCDO(1) .eq. 12) .and.
     1    ICYCFL(16) .eq. 1) call pshcyc (istk,ipt,13,gfin)
c
c...Perform pecking cycles for
c...DEEP & THRU
c
      if (ICYCDO(1) .eq. 5 .or. ICYCDO(1) .eq. 13)
     1    call cympek (istk,ipt,gpos,gclr,TLVEC)
c
c...Perform helical motion for
c...CYCLE/CIRCUL
c
      if (ICYCDO(1) .eq. 14) then
          call cymcir (istk,ipt,gfin,gpos,TLVEC)
          if (ICYCDO(5) .eq. 1) goto 8000
c
c...Feed in to final depth
c
      else
          call pshcyc (istk,ipt,3,gfin)
      endif
c
c...Reverse spindle for
c...REVERS, TAP
c
      if (ICYCDO(1) .eq. 10 .or. ICYCDO(1) .eq. 12)
     1        call pshcyc (istk,ipt,icclw,gfin)
c
c...Perform optional dwell
c
      if (ICYCDO(5) .ne. 0 .and. RCYCDO(5) .ne. 0.)
     1    call pshcyc (istk,ipt,6,gfin)
c
c...Stop spindle for
c...BORE, BORE7, BORE8
c
      if (ICYCDO(1) .eq. 1 .or. ICYCDO(1) .eq. 2 .or. ICYCDO(1) .eq. 3)
     1        call pshcyc (istk,ipt,8,gfin)
c
c...Shift tool for
c...BORE9, SHIFT
c
      if (ICYCDO(1) .eq. 4 .or. ICYCDO(1) .eq. 11) then
          call pshcyc (istk,ipt,9,gfin)
          call cymshf (istk,ipt,gfin,1,2)
      endif
c
c...Stop program for BORE7
c
      if (ICYCDO(1) .eq. 2) call pshcyc (istk,ipt,10,gfin)
c
c...CYCLE/MILL
c...End of cycle
c
      if (ICYCDO(1) .eq. 8) go to 8000
c
c...Retract tool to rapto plane
c
      if (kret .eq. 1 .or. inc .ne. 0) then
          if (ICYCDO(1) .eq. 2) then
              call popcyc (istk,ipt)
              MCHNUM(1,2) = gpos(1)
              MCHNUM(2,2) = gpos(2)
              MCHNUM(3,2) = gpos(3)
              call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
              call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
          else
              if (ICYCDO(1) .eq. 4 .or. ICYCDO(1) .eq. 11)
     1                call cymshf (istk,ipt,gpos,1,2)
              ifl    = 3
              if (inc .eq. 0) ifl = 2
              call pshcyc (istk,ipt,ifl,gpos)
          endif
      endif
c
c...Retract tool to clearance plane
c
      if (kret .eq. 2) then
          if (ICYCDO(1) .eq. 2) then
              call popcyc (istk,ipt)
              MCHNUM(1,2) = gclr(1)
              MCHNUM(2,2) = gclr(2)
              MCHNUM(3,2) = gclr(3)
              call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
              call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
          else
              if (ICYCDO(1) .eq. 4 .or. ICYCDO(1) .eq. 11)
     1                call cymshf (istk,ipt,gclr,1,2)
              call pshcyc (istk,ipt,2,gclr)
          endif
      endif
c
c...Start spindle for
c...BORE, BORE7, BORE8, SHIFT, TAP, REVERS
c
      if (ICYCDO(1) .eq. 1 .or. ICYCDO(1) .eq. 2 .or.
     1    ICYCDO(1) .eq. 3 .or. ICYCDO(1) .eq. 11 .or.
     2    ICYCDO(1) .eq. 12 .or. ICYCDO(1) .eq. 10)
     3        call pshcyc (istk,ipt,iclw,gfin)
c
c...Unlock feedrate & spindle overrides for
c...REVERS & TAP
c
      if ((ICYCDO(1) .eq. 10 .or. ICYCDO(1) .eq. 12) .and.
     1    ICYCFL(16) .eq. 1) call pshcyc (istk,ipt,14,gfin)
c
c...End of routine
c
 8000 call popcyc (istk,ipt)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cympek (kstk,kpt,gpos,gret,gvec)
c
c   FUNCTION:  This routine performs the pecking portion of the DEEP and
c              THRU cycles.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack that stores the type
c                               of block to output.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.
c
c           gpos    R*8  D3  -  Current tool end point position.  This
c                               is usually the rapto plane position.
c
c           gret    R*8  D3  -  Retract position.
c
c           gvec    R*8  D3  -  Tool axis vector for cycle operation.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cympek (kstk,kpt,gpos,gret,gvec)
c
      include 'post.inc'
c
      equivalence (ICYCDO,KPOSMP(0276)), (MCHOPT,KPOSMP(0308))
c
      integer*4 ICYCDO(15),MCHOPT(20)
c
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20)
c
      integer*4 kstk(5),kpt
c
      real*8 gpos(3),gret(3),gvec(3)
c
      real*8 rpt(3),rmch(3),rplng,pdif,rdep,fdep,pck1,pck2,rlst,rdis,
     1       tvec(3),rret(3),dr
c
c...Initialize routine
c
      if (ICYCDO(6) .eq. 0 .or. RCYCDO(7) .eq. 0.) go to 8000
c
c......Define pecking depths
c
      pck1   = dabs(RCYCDO(7))
      pck2   = dabs(RCYCDO(8))
      if (ICYCDO(6) .eq. 1 .or. pck2 .eq. 0.) then
          pck2   = pck1
          pdif   = 0.
          rlst   = 0.
      else
          pdif   = pck1   - pck2
          rlst   = pck2   * .2
      endif
c
c......Define final depth
c
      rdep   = 0.
      fdep   = RCYCDO(1) + RCYCDO(4)
      if (fdep .ge. 0.) then
          tvec(1) = gvec(1)
          tvec(2) = gvec(2)
          tvec(3) = gvec(3)
      else
          fdep   = dabs(fdep)
          tvec(1) = gvec(1) * (-1.)
          tvec(2) = gvec(2) * (-1.)
          tvec(3) = gvec(3) * (-1.)
      endif
c
c......Store current position
c
      rpt(1) = gpos(1)
      rpt(2) = gpos(2)
      rpt(3) = gpos(3)
c
c...Retract plane for THRU
c
      if (ICYCDO(1) .eq. 13) then
          if (ICYCDO(8) .eq. 0) then
              rret(1) = gpos(1)
              rret(2) = gpos(2)
              rret(3) = gpos(3)
          else
              rdis   = RCYCDO(11) - RCYCDO(4)
              rret(1) = gpos(1) + rdis * tvec(1)
              rret(2) = gpos(2) + rdis * tvec(2)
              rret(3) = gpos(3) + rdis * tvec(3)
          endif
      endif
c
c......Store plunge distance
c
      if (ICYCDO(8) .le. 1) then
          rplng  = .1
          if (MCHOPT(2) .eq. 2) rplng = 2.5
      else
          rplng  = RCYCDO(12)
      endif
c
c...Perform pecking cycle
c
  100 rdis   = pck1 - (rdep / fdep) * pdif
      if (rdep+rdis .ge. fdep .or. rdep+pck2+rlst .ge. fdep) go to 8000
      rdep   = rdep   + rdis
c
c......Feed in 'peck'
c
      rpt(1) = rpt(1) - rdis * tvec(1)
      rpt(2) = rpt(2) - rdis * tvec(2)
      rpt(3) = rpt(3) - rdis * tvec(3)
      call pshcyc (kstk,kpt,3,rpt)
      dr     = dsqrt ((rret(1)-rpt(1))**2 +
     -                (rret(2)-rpt(2))**2 +
     -                (rret(3)-rpt(3))**2)
c
c......Perform dwell
c
      if (ICYCDO(5) .ne. 0 .and. RCYCDO(5) .gt. 0.)
     1        call pshcyc (kstk,kpt,6,rpt)
c
c.........CYCLE/THRU retract & plunge logic
c
      if (ICYCDO(1) .eq. 13) then
          call pshcyc (kstk,kpt,2,rret)
          if (ICYCDO(5) .eq. 2 .and. RCYCDO(6) .gt. 0.)
     1            call pshcyc (kstk,kpt,7,rpt)
c
c...........Plunge if retracted over plunge level
c
          if (dr .gt. rplng) then
              rmch(1) = rpt(1) + rplng * tvec(1)
              rmch(2) = rpt(2) + rplng * tvec(2)
              rmch(3) = rpt(3) + rplng * tvec(3)
              call pshcyc (kstk,kpt,2,rmch)
          end if
c
c.........CYCLE/DEEP retract logic
c
      else if (ICYCDO(8) .ne. 0 .and. RCYCDO(11) .gt. 0.) then
          rmch(1) = rpt(1) + RCYCDO(11) * tvec(1)
          rmch(2) = rpt(2) + RCYCDO(11) * tvec(2)
          rmch(3) = rpt(3) + RCYCDO(11) * tvec(3)
          call pshcyc (kstk,kpt,2,rmch)
          if (ICYCDO(5) .eq. 2 .and. RCYCDO(6) .gt. 0.)
     1            call pshcyc (kstk,kpt,7,rpt)
      endif
c
c......Perform next peck
c
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymshf (kstk,kpt,gpos,kdir,kpos)
c
c   FUNCTION:  This routine outputs the shifted point for the BORE9 and
c              shift cycles.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack that stores the type
c                               of block to output.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.
c
c           gpos    R*8  D3  -  Current tool end point position.  The
c                               shift amounts will be applied to this
c                               location.
c
c           kdir    I*4  D1  -  1 = Apply the shift in the direction of
c                               the cycle OFFSET values.  -1 = Apply the
c                               shift in the opposite direction.
c
c           kpos    I*4  D1  -  1 = Position in rapid mode, 2 = Using
c                               rapid feedrate, 3 = Using programmed
c                               feedrate.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymshf (kstk,kpt,gpos,kdir,kpos)
c
      include 'post.inc'
c
      equivalence (CYCPAX,KPOSMP(0299))
c
      integer*4 CYCPAX
c
      equivalence (RCYCDO,POSMAP(2931)), (SPIVEC,POSMAP(3583))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 ROTANG(20,2),RCYCDO(20),SPIVEC(3)
c
      integer*4 kstk(5),kpt,kdir,kpos
c
      real*8 gpos(3)
c
      integer*4 ispt(3,3),is(3)
c
      real*8 rmch(3,4),raxs(10),rlin(6),rvec(3)
c
      data ispt /1,2,3, 3,1,2, 2,3,1/
c
c...Get indices based on major tool axis
c
      is(1) = ispt(1,CYCPAX)
      is(2) = ispt(2,CYCPAX)
      is(3) = ispt(3,CYCPAX)
c
c...Adjust points to machine system
c
      rmch(1,2) = gpos(1)
      rmch(2,2) = gpos(2)
      rmch(3,2) = gpos(3)
      call alladj (rmch,rlin,raxs,ROTANG,2,2)
c
c...Apply shift amounts
c
      rmch(is(1),3) = rmch(is(1),3) + RCYCDO(9) * kdir
      rmch(is(2),3) = rmch(is(2),3) + RCYCDO(10) * kdir
c
c...Backadjust shifted points
c
      call alladr (raxs,rlin,rmch,ROTANG,rvec,2,2)
c
c...Output shifted point
c
      call pshcyc (kstk,kpt,kpos,rmch(1,2))
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: popcyc (kstk,kpt)
c
c   FUNCTION:  This routine clears out the manual cycle stack.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine popcyc (kstk,kpt)
c
      integer*4 kstk(5),kpt
c
      real*8 rnum(3)
c
c...Clean up manual cycle stack
c
      call pshcyc (kstk,kpt,-1,rnum)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: pshcyc (kstk,kpt,kfl,gpos)
c
c   FUNCTION:  This routine controls the actual output of all blocks
c              generated by a post genterated cycle.  It puts blocks on
c              the stack to satisfy lookahead routines and takes them
c              off the stack to output them.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack that stores the type
c                               of block to output.  Currenty only 1
c                               block is stored on the stack at a time.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.  Should be set to 0
c                               for initial call.
c
c           kfl     I*4  D1  -  Type of block to output and can have the
c                               following values:
c
c                                  -1 = An input block is not given.
c                                       Output the next stack block
c                                       only.
c
c                                   1 = Position in rapid mode.
c                                   2 = Position using rapid feedrate.
c                                   3 = Position using cycle feedrate.
c                                   4 = Not used.
c                                   5 = Not used.
c                                   6 = DWELL 1.
c                                   7 = DWELL 2.
c                                   8 = Spindle stop.
c                                   9 = Spindle orient.
c                                  10 = STOP.
c                                  11 = Spindle CLW.
c                                  12 = Spindle CCLW.
c                                  13 = Disable feed overrides.
c                                  14 = Enable feed overrides.
c
c           gpos    R*8  D3  -  Tool end point position to output when
c                               'kfl' is less than 6.
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pshcyc (kstk,kpt,kfl,gpos)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (MCHOPT,KPOSMP(0308))
      equivalence (STOPCD,KPOSMP(1076))
      equivalence (SPNDCD,KPOSMP(3105)), (SPNOCD,KPOSMP(3121))
      equivalence (SPNDIR,KPOSMP(3141)), (IFITYP,KPOSMP(3150))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (FEDOCD,KPOSMP(3217)), (ICY2FL,KPOSMP(4004))
c
      integer*4 IFITYP,SPNDCD(4),ITYPE,ISUBT,MXCL,SPNDIR,STOPCD,
     1          MCHOPT(20),SPNOCD(2),FEDOCD(2),ICY2FL(20),IRPALT,IRPTAX
c
      equivalence (STOPVL,POSMAP(1169))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (RCYCDO,POSMAP(2931)), (FEDOVL,POSMAP(3001))
      equivalence (SPNDVL,POSMAP(3007)), (SPNOVL,POSMAP(3022))
      equivalence (RPMSAV,POSMAP(3305)), (SFMSAV,POSMAP(3306))
      equivalence (RPM   ,POSMAP(3307)), (SFM   ,POSMAP(3308))
      equivalence (PFEED ,POSMAP(3540)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),TLVEC(3),RCYCDO(20),
     1       PFEED(4),AXSOUT(10),SPNDVL(4),RPM,SFM,RPMSAV,SFMSAV,
     2       STOPVL,SPNOVL(2),FEDOVL(2)
c
      integer*4 kstk(5),kpt,kfl
c
      real*8 gpos(3)
c
      integer*4 ifl(10),icnt,its,ims,iss,ifsv,inc1,inc2
c
      real*8 rmch(3,4),rlin(6),raxs(10),rfsv
c
      if (kpt .eq. 0 .and. kfl .eq. -1) go to 8000
c
c...Remove previous position
c...from stack
c
      if (kpt .ne. 0) then
          if (kstk(kpt) .le. 5) then
              call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,ifl,icnt)
          else
              its    = ITYPE
              iss    = ISUBT
              ims    = MXCL
              call popcmd
              ITYPE  = its
              ISUBT  = iss
              MXCL   = ims
          endif
      endif
c
c...Push current position
c...onto lookahead stack
c
      if (kfl .eq. -1) go to 500
      if (kfl .le. 5) then
          rmch(1,2) = gpos(1)
          rmch(2,2) = gpos(2)
          rmch(3,2) = gpos(3)
          call alladj (rmch,rlin,raxs,ROTANG,1,5)
          call pshaxs (raxs,TLVEC)
c
c...Push the post command
c...onto lookahead stack
c...(Actually place the STOP command
c... onto the lookahead stack)
c
      else
          its    = ITYPE
          iss    = ISUBT
          ims    = MXCL
          ITYPE  = 2000
          ISUBT  = 2
          MXCL   = 0
          call pshcmd
          ITYPE  = its
          ISUBT  = iss
          MXCL   = ims
      endif
c
c...First time here
c...Don't output previous cycle block
c
      if (kpt .eq. 0) then
          kpt    = 1
          kstk(kpt) = kfl
          go to 8000
      endif
c
c...Output previous cycle block
c......Previous position
c
  500 if (kstk(kpt) .le. 5) then
          if (icnt .ne. 0) then
              ifsv   = IFITYP
              rfsv   = PFEED(1)
c
c........Move in rapid mode
c
              if (kstk(kpt) .eq. 1) then
                  inc1   = ICY2FL(4)
                  inc2   = ICY2FL(5)
                  if (inc1 .eq. 1) then
                      inc1   = IRPALT
                      inc2   = IRPTAX
                  endif
                  call rapset (inc1,inc2)
c
c........Move at cycle rapid rate
c
              else if (kstk(kpt) .eq. 2) then
                  IFITYP = 1
                  PFEED(1) = RCYCDO(3)
c
c........Move at programmed feed rate
c...........TPI mode
c
              else if (IFITYP .eq. 5) then
                  IFITYP = 1
                  if ((MCHOPT(2) .eq. 1 .and. RCYCDO(2) .gt. 1.)
     1                .or. (MCHOPT(2) .eq. 2 .and.
     2                RCYCDO(2) .gt. 25.4)) then
                      PFEED(1) = RPM   / RCYCDO(2)
                  else
                      PFEED(1) = RPM   * RCYCDO(2)
                  endif
              endif
c
c........Perform cycle move
c
              call motion (ifl,icnt)
              if (kstk(kpt) .eq. 1) call raprst
              IFITYP = ifsv
              PFEED(1) = rfsv
          endif
c
c......Post command
c
      else
c
c.........DWELL 1 block
c
          if (kstk(kpt) .eq. 6) then
              call dlyout (RCYCDO(5),1)
c
c.........DWELL 2 block
c
          else if (kstk(kpt) .eq. 7) then
              call dlyout (RCYCDO(6),1)
c
c.........Spindle stop
c
          else if (kstk(kpt) .eq. 8) then
              RPM    = 0.
              SFM    = 0.
              call codout (SPNDCD(3),SPNDVL(3))
              call clrbuf
c
c.........Spindle orient
c
          else if (kstk(kpt) .eq. 9) then
              RPM    = 0.
              SFM    = 0.
              call codout (SPNDCD(4),SPNDVL(4))
              call clrbuf
c
c.........STOP
c
          else if (kstk(kpt) .eq. 10) then
              call codout (STOPCD,STOPVL)
              call clrbuf
c
c.........Spindle CLW
c
          else if (kstk(kpt) .eq. 11) then
              RPM   = RPMSAV
              SFM   = SFMSAV
              SPNDIR = 1
              call spndon
c
c.........Spindle CCLW
c
          else if (kstk(kpt) .eq. 12) then
              RPM   = RPMSAV
              SFM   = SFMSAV
              SPNDIR = 2
              call spndon
c
c.........Disable feed overrides
c
          else if (kstk(kpt) .eq. 13) then
              call codout (FEDOCD(1),FEDOVL(1))
              call codout (SPNOCD(1),SPNOVL(1))
c
c.........Enable feed overrides
c
          else if (kstk(kpt) .eq. 14) then
              call codout (FEDOCD(2),FEDOVL(2))
              call codout (SPNOCD(2),SPNOVL(2))
          endif
      endif
c
c...Push current cycle block
c...onto local stack
c
      if (kfl .eq. -1) then
          kpt   = kpt   - 1
      else
          kstk(kpt) = kfl
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cymcir (kstk,kpt,gfin,gpos.gvec)
c
c   FUNCTION:  This routine calculates and outputs the helical blocks
c              for the CYCLE/CIRCUL canned cycle.
c
c   INPUT:  kstk    I*4  D5  -  Local cycle stack that stores the type
c                               of block to output.
c
c           kpt     I*4  D1  -  Pointer to 'kstk'.
c
c           gfin    R*8  D3  -  The linear axes position at the bottom
c                               of the hole.
c
c           gpos    R*8  D3  -  The linear axes position at the top of
c                               the part.
c
c           gvec    R*8  D3  -  Tool axis vector for cycle operation.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymcir (kstk,kpt,gfin,gpos,gvec)
c
      include 'post.inc'
c
      equivalence (SIMACT,KPOSMP(0174))
      equivalence (ICYCDO,KPOSMP(0276)), (ICIPRM,KPOSMP(1230))
      equivalence (ISCIRC,KPOSMP(1238)), (ICRPFL,KPOSMP(1370))
      equivalence (ICRBRK,KPOSMP(1372)), (ICIRSW,KPOSMP(1391))
      equivalence (IHELIX,KPOSMP(1392)), (ICIDLT,KPOSMP(1741))
      equivalence (IFEDSW,KPOSMP(3104)), (IFITYP,KPOSMP(3150))
c
      integer*4 ICYCDO(15),ICIPRM(8),ICIDLT,ISCIRC,IHELIX,ICIRSW,ICRPFL,
     1          ICRBRK,SIMACT,IFITYP,IFEDSW
c
      equivalence (RAD   ,POSMAP(0002)), (RCIPRM,POSMAP(2049))
      equivalence (CIRTOL,POSMAP(2201))
      equivalence (HELANG,POSMAP(2213)), (RCYCDO,POSMAP(2931))
      equivalence (PFEED ,POSMAP(3540))
      equivalence (XFMMAT,POSMAP(4025)), (ROTANG,POSMAP(5173))
c
      real*8 RAD,CIRTOL(5),RCYCDO(20),HELANG(2),ROTANG(20,2),RCIPRM(25),
     1       XFMMAT(12),PFEED(4)
c
      integer*4 kstk(5),kpt
c
      real*8 gfin(3),gpos(3),gvec(3),kfed
c
      integer*4 nramps,iclw,icw,ihx,icb,ierr
c
      logical isidmx
c
      real*8 rramps,rmch(3,4),rlin(6),raxs(10),rfin(3),rpos(3),rvec(3),
     1       svec(3),evec(3),tv(3),pvec(3),mx(12),rang(2)
c
c...Save flags
c
      icw    = ICIRSW
      ihx    = IHELIX
      icb    = ICRBRK
c
c...Adjust points to machine system
c
      rmch(1,2) = gfin(1)
      rmch(2,2) = gfin(2)
      rmch(3,2) = gfin(3)
      call alladj (rmch,rlin,raxs,ROTANG,2,2)
      rfin(1) = rmch(1,3)
      rfin(2) = rmch(2,3)
      rfin(3) = rmch(3,3)
c
      rmch(1,2) = gpos(1) - RCYCDO(4)*gvec(1)
      rmch(2,2) = gpos(2) - RCYCDO(4)*gvec(2)
      rmch(3,2) = gpos(3) - RCYCDO(4)*gvec(3)
      call alladj (rmch,rlin,raxs,ROTANG,2,2)
      rpos(1) = rmch(1,3)
      rpos(2) = rmch(2,3)
      rpos(3) = rmch(3,3)
c
c...Calculate circle flags
c......Determine machining plane
c
      call pivvec (ROTANG(1,1),pvec)
      call ptxfm (pvec,pvec,2)
      if (pvec(1) .ge. .999999) then
          ICIRSW = ICRPFL
          ICIPRM(1) = 2
          ICIPRM(2) = 3
          ICIPRM(3) = 1
      else if (pvec(2) .ge. .999999) then
          ICIRSW = ICRPFL
          ICIPRM(1) = 3
          ICIPRM(2) = 1
          ICIPRM(3) = 2
      else if (pvec(3) .ge. .999999) then
          ICIPRM(1) = 1
          ICIPRM(2) = 2
          ICIPRM(3) = 3
      else
          ICIRSW = 2
          ICRBRK = 3
          ICIPRM(1) = 0
          ICIPRM(2) = 0
          ICIPRM(3) = 0
      endif
c
c......Quadrant, Direction, Helix Macro flag
c
      ICIPRM(4) = 1
      ICIPRM(5) = 2 - ICYCDO(6)
      ICIPRM(6) = 0
c
c...Setup circle parameters
c......Center point
c
      call ptxfm (rpos,RCIPRM(1),1)
c
c......Circle radius
c
      if (ICYCDO(7) .eq. 0) then ! IN
          RCIPRM(4) = RCYCDO(8) - RCYCDO(5)/2
      else
          RCIPRM(4) = RCYCDO(8)
      endif
      if (RCIPRM(4) .lt. CIRTOL(1)) go to 8000
c
c......Starting  and ending angles
c
      RCIPRM(5) = RCYCDO(6)
      iclw   = 1
      if (ICIPRM(5) .eq. 1)  iclw = -1
      rramps = RCYCDO(1)/RCYCDO(7)
      nramps = rramps
      RCIPRM(6) = RCIPRM(5) + iclw*360.0*(rramps-nramps)
      RCIPRM(7) = 360.*rramps
c
c......Get starting and ending vectors
c
      svec(1) = dcos(RCIPRM(5)/RAD)
      svec(2) = dsin(RCIPRM(5)/RAD)
      svec(3) = 0.
      evec(1) = dcos(RCIPRM(6)/RAD)
      evec(2) = dsin(RCIPRM(6)/RAD)
      evec(3) = 0.
c
c......3-D helical
c
      if (ICIPRM(1) .eq. 0) then
c
c.........Rotate starting vector
c
          if (SIMACT .eq. 1 .and. .not. isidmx(XFMMAT)) then
              call mxtomx (XFMMAT,mx)
          else
              call gtpola (gvec,rang,mx)
          endif
          call ptmatb (svec,tv,mx,2)
          call ptxfm (tv,RCIPRM(14),2)
c
c.........Starting vector already has starting angle in it
c.........So zero out the starting angle
c
          RCIPRM(6) = RCIPRM(6) - RCIPRM(5)
          RCIPRM(5) = 0.
c
c.........Starting location
c
          RCIPRM(11) = RCIPRM(1) + RCIPRM(4)*RCIPRM(14)
          RCIPRM(12) = RCIPRM(2) + RCIPRM(4)*RCIPRM(15)
          RCIPRM(13) = RCIPRM(3) + RCIPRM(4)*RCIPRM(16)
          call ptxfr (RCIPRM(11),rmch(1,3),1)
c
c.........Ending location
c
          call ptmatb (evec,tv,mx,2)
          call ptxfm (tv,tv,2)
          RCIPRM(8) = RCIPRM(1) + RCIPRM(4)*tv(1)
          RCIPRM(9) = RCIPRM(2) + RCIPRM(4)*tv(2)
          RCIPRM(10) = RCIPRM(3) + RCIPRM(4)*tv(3)
c
c.........Circular plane
c
          call ptxfm (rfin,rfin,1)
          call ptxfm (gvec,pvec,2)
          call plnvpt (pvec,rfin,RCIPRM(17),ierr)
          if (ierr .ne. 0) go to 8000
c
c......2-D helical
c
      else
c
c.........Starting location
c
          RCIPRM(11) = RCIPRM(ICIPRM(1)) + RCIPRM(4)*svec(1)
          RCIPRM(12) = RCIPRM(ICIPRM(2)) + RCIPRM(4)*svec(2)
          RCIPRM(13) = RCIPRM(ICIPRM(3))
          rmch(ICIPRM(1),3) = RCIPRM(11)
          rmch(ICIPRM(2),3) = RCIPRM(12)
          rmch(ICIPRM(3),3) = RCIPRM(13)
          call ptxfr (rmch(1,3),rmch(1,3),1)
c
c.........Ending location
c
          RCIPRM(8) = RCIPRM(ICIPRM(1)) + RCIPRM(4)*evec(1)
          RCIPRM(9) = RCIPRM(ICIPRM(2)) + RCIPRM(4)*evec(2)
          RCIPRM(10) = RCIPRM(ICIPRM(3))
      endif
c
c...Output starting point of helix
c
      call alladr (raxs,rlin,rmch,ROTANG,rvec,2,2)
      call pshcyc (kstk,kpt,3,rmch(1,2))
c
c...Flush cycle stack
c
      call popcyc (kstk,kpt)
c
c...Calculate linear distance per degree
c
      HELANG(1) = RCYCDO(7) / 360.
c
c...Calculate circular deltas
c
      IHELIX = 1
      ISCIRC = 1
      if (ICIDLT .eq. 1) call cirhl1 (RCIPRM,ICIPRM)
c
c...Output helical motion
c
      kfed = 0
      PFEED(1) = RCYCDO(2)
      IFEDSW = 1
      call cirout (RCIPRM,ICIPRM)
c
c...Perform extra pass around bottom of circle
c
      ISCIRC = 1
      IHELIX = 0
      RCIPRM(7) = 360.
      if (ICIPRM(1) .eq. 0) then
          ICIPRM(4) = 1
          RCIPRM(6) = 0.
          RCIPRM(5) = RCIPRM(6)
          RCIPRM(11) = RCIPRM(8)
          RCIPRM(12) = RCIPRM(9)
          RCIPRM(13) = RCIPRM(10)
          RCIPRM(14) = tv(1)
          RCIPRM(15) = tv(2)
          RCIPRM(16) = tv(3)
      else
          RCIPRM(5) = RCIPRM(6)
          RCIPRM(11) = RCIPRM(7+ICIPRM(1))
          RCIPRM(12) = RCIPRM(7+ICIPRM(2))
      endif
      if (ICIDLT .eq. 1) then
          call tmpdrs
          call cirhl1 (RCIPRM,ICIPRM)
      endif
      call cirout (RCIPRM,ICIPRM)
c
c...retracting tool or not would
c...position at center of hole
c
c      if (ICYCDO(5) .eq. 0) then
          rfin(1) = gfin(1)  ! + RCYCDO(4) * gvec(1)
          rfin(2) = gfin(2)  ! + RCYCDO(4) * gvec(2)
          rfin(3) = gfin(3)  ! + RCYCDO(4) * gvec(3)
          call pshcyc (kstk,kpt,3,rfin)
c      endif
c
c...Restore flags
c
      ICIRSW = icw
      ICRBRK = icb
      IHELIX = ihx
      ISCIRC = 0
c
c...End of routine
c
 8000 return
      end
