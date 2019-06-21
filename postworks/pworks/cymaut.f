c
c***********************************************************************
c
c   FILE NAME:  cymaut
c   CONTAINS:
c               cymaut  cymout  cymblk  cymcal  cymclk  cymcps  cympos
c               cymdep  cymret  cymrpl  cymtim  cymrtc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cymaut.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:37:28
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cymaut (kfl)
c
c   FUNCTION:  This is the controlling routine for automatic Mill
c              cycles.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  1 = Manual retract generated.
c                                  2 = Postion at top of hole prior to
c                                      CYCLE/DEEP.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymaut (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (ICYCSV,KPOSMP(0291))
      equivalence (MCHOPT,KPOSMP(0308)), (CYCMAX,KPOSMP(0299))
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (SPITAX,KPOSMP(1279)), (IFITYP,KPOSMP(3150))
      equivalence (IFOTYP,KPOSMP(3151)), (IFDOUT,KPOSMP(3158))
      equivalence (LTMODE,KPOSMP(4125))
c
      integer*4 ICYCFL(30),ICYCSW(5),SPITAX,IFITYP,IFOTYP,IFDOUT(4),
     1          ICYCDO(15),ICYCSV(5),MCHOPT(20),CYCMAX,CYCREG(20),
     2          LTMODE,XFMFL(20)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (VECSAV,POSMAP(1372))
      equivalence (AXSSTO,POSMAP(1425)), (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
      equivalence (RPM   ,POSMAP(3307)), (PFEED ,POSMAP(3540))
      equivalence (FEED  ,POSMAP(3547)), (OFEED ,POSMAP(3560))
      equivalence (SPIVEC,POSMAP(3583)), (SPICVC,POSMAP(3586))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSSTO(10),ROTANG(20,2),TLVEC(3),
     1       SPIVEC(3),STONUM(3,4),RCYCDO(20),PFEED(4),FEED(4),OFEED(6),
     2       RPM,CYCPSV(10),AXSOUT(10),SPICVC(3),VECSAV(3)
c
      integer*4 kfl
c
      integer*4 ispt(3,3),is(4),isav,idid, iax(10),i,ireg,inum
c
      data ispt /1,2,3, 3,1,2, 2,3,1/
      data iax /0,0,0,0,0,0,0,0,0,0/
c
      real*8 rsgn,rfin(3),rpos(3),rsav,rnum1,rnum2,rmch(3),stnum(3),
     1       vsav(3)
c
c...Second time here
c...Pop previous position from stack
c
      idid   = 0
      if (kfl .ne. 0) then
          call cyc2nd (1)
c
c......When kfl=1
c......The transformation block has already been output
c......No need to adjust the Axis again
c
          if (kfl .eq. 2 .and. XFMFL(10) .eq. 1) then
              call axsxfm (AXSOUT,AXSOUT)
              call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
          endif
          ICYCSW(4) = 0
          idid   = kfl
          kfl    = 0
      endif
c
c...Set up cycle positions
c......Assign major tool axis direction
c
      is(1) = ispt(1,CYCMAX)
      is(2) = ispt(2,CYCMAX)
      is(3) = ispt(3,CYCMAX)
      if (XFMFL(11) .eq. 1) then
          call ptxfm (TLVEC,rpos,2)
      else
          call ptxfm (SPICVC,rpos,2)
      endif
      rsgn   = rpos(is(3)) / dabs(rpos(is(3)))
      is(4) = rsgn
c
c......Final position
c
      call ptxfm (MCHNUM(1,4),rmch,1)
      rfin(is(1)) = rmch(is(1))
      rfin(is(2)) = rmch(is(2))
      rfin(is(3)) = rmch(is(3)) - RCYCDO(1)*rsgn
c
c......Top of part &
c......Rapto plane
c
      rpos(1) = rmch(is(3))
      rpos(2) = rmch(is(3)) + RCYCDO(4)*rsgn
c
c......Clearance plane
c
      call ptxfm (STONUM(1,4),stnum,1)
      if (idid .eq. 2) stnum(is(3)) = CYCPSV(4)
      if (ICYCSW(2) .eq. 1) CYCPSV(4) = stnum(is(3))
c
c......Retract plane
c.........CYCLE/THRU w/no retract
c
      if (ICYCFL(25) .eq. 2 .and. ICYCDO(1) .eq. 13) then
          rpos(3) = rfin(is(3))
c
c.........Using RTRCTO parameter
c
      else if (ICYCFL(26) .eq. 1 .and. ICYCDO(8) .ge. 1) then
          rpos(3) = rpos(1) + RCYCDO(11)*rsgn
c
c.........Using RETRCT statment
c
      else if (ICYCFL(9) .eq. 1) then
          if (ICYCSW(3) .eq. 1) then
              rpos(3) = CYCPSV(4)
          else
              rpos(3) = rpos(2)
          endif
c
c.........Retract to clearance plane
c
      else if (ICYCFL(9) .eq. 2) then
          rpos(3) = CYCPSV(4)
c
c.........Retract to rapto plane
c
      else
          rpos(3) = rpos(2)
      endif
c
c...Output retract block when
c...Rapto plane is higher than
c...previous retract plane
c......Cannot determine retract plane
c......in the middle of a Xform block change
c......due to current Xform not being calculated yet
c
c
      if (ICYCFL(10) .eq. 1 .and. idid .eq. 0 .and.
     1    XFMFL(10) .eq. 0) then
          call gmotrg (is(3),ireg)
          call codint (ireg,rpos(2),rnum1,inum)
          call codint (ireg,stnum(is(3)),rnum2,inum)
          if (rnum1*rsgn .gt. rnum2*rsgn) then
              call cymret (is,rpos,kfl)
              if (kfl .ne. 0) go to 8000
          endif
      endif
c
c...Position tool prior to CYCLE/DEEP
c...or when Automatic XFORM block is being output
c
      if (((ICYCFL(24) .eq. 1 .and. ICYCDO(1) .eq. 5 .and.
     1    ICYCSW(2) .eq. 1) .or. XFMFL(10) .eq. 1) .and. idid .ne. 2)
     2        then
          if (XFMFL(10) .eq. 1)
     1        call axsxfr (AXSOUT,AXSOUT)
          call pshaxs (AXSOUT,TLVEC)
          call cycoff (1)
          rmch(is(3)) = stnum(is(3))
          call ptxfr (rmch,MCHNUM(1,4),1)
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
          call rapset (1,2)
          kfl    = 2
          go to 8000
      endif
c
c...Calculate cutting feedrate
c
      if (IFITYP .eq. 5) then
          isav   = IFITYP
          rsav   = PFEED(1)
          IFITYP = 1
          if ((MCHOPT(2) .eq. 1 .and. RCYCDO(2) .gt. 1.) .or.
     1        (MCHOPT(2) .eq. 2 .and. RCYCDO(2) .gt. 25.4)) then
              PFEED(1) = RPM    / RCYCDO(2)
          else
              PFEED(1) = RPM    * RCYCDO(2)
          endif
          call fedctl(iax)
          IFITYP = isav
          PFEED(1) = rsav
c
c...Make sure TPI feed rates are output
c...Vadim  -  3/9/95
c
          if (CYCREG(3) .eq. 0.) then
              IFOTYP = IFDOUT(2)
          else
              IFOTYP = IFITYP
          endif
      else
          call fedctl(iax)
          IFOTYP = IFDOUT(IFITYP)
      endif
      OFEED(1) = FEED(1)
      OFEED(2) = FEED(1)
      OFEED(3) = FEED(1)
      OFEED(4) = 0.
      OFEED(5) = FEED(1)
      OFEED(6) = FEED(2)
c
c...Calculate machining time
c
      call cymtim (is,rpos)
c
c...Output cycle block
c
      call cymout (is,rfin,rpos)
c
c...Store current position
c
      stnum(is(1)) = rfin(is(1))
      stnum(is(2)) = rfin(is(2))
      stnum(is(3)) = rpos(3)
      call ptxfr (stnum,STONUM(1,4),1)
      call alladj (STONUM,LINAXS,AXSSTO,ROTANG,4,5)
      if (LTMODE .ne. 0) then
          vsav(1) = TLVEC(1)
          vsav(2) = TLVEC(2)
          vsav(3) = TLVEC(3)
      endif
      call setpos
c
c...Restore tool axis if in LATHE/MILL mode
c...Otherwise setpos adjusts it for XY-ZX xfrom
c...Which is not correct
c
      if (LTMODE .ne. 0) then
          do 500 i=1,3,1
              VECSAV(i) = vsav(i)
              TLVEC(i) = vsav(i)
  500     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymout (ks,gfin,gpos)
c
c   FUNCTION:  This routine determines which codes need to be output on
c              a milling canned cycle block and calls a routine to out-
c              put them.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymout (ks,gfin,gpos)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (CYCREG,KPOSMP(0231)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (INCR  ,KPOSMP(1226))
      equivalence (PLCYCD,KPOSMP(0296)), (CYCPAX,KPOSMP(0299))
      equivalence (ICYMTP,KPOSMP(0920)), (DELYFL,KPOSMP(3356))
c
      integer*4 ICYCFL(30),CYCCOD(20),CYCREG(20),ICYCSW(5),ICYCDO(15),
     1          INCR,DELYFL(5),PLCYCD(3),CYCPAX,ICYMTP
c
      equivalence (STONUM,POSMAP(1387)), (CYPCDV,POSMAP(2998))
      equivalence (CYCCDV,POSMAP(2901)), (CYCVR ,POSMAP(2926))
      equivalence (RCYCDO,POSMAP(2931)), (CYCPSV,POSMAP(2951))
c
      real*8 CYCCDV(25),RCYCDO(20),CYCPSV(10),STONUM(3,4),CYCVR(5),
     -       CYPCDV(3)
c
      integer*4 ks(4)
c
      real*8 gfin(3),gpos(3)
c
      integer*4 is1,is2,ifl(20),i,inum,ireg
c
      real*8 rv(20),rnum
c
c...Clear output flags
c
      do 100 i=1,20,1
          ifl(i) = 0
  100 continue
      call gmotrg (ks(3),ireg)
c
c.........Force cycle code when
c.........Z level changes
c
      if (ICYCFL(15) .eq. 1) then
          call codint (ireg,gfin(ks(3)),rnum,inum)
          rnum   = dabs(rnum-CYCPSV(1))
          if (rnum .gt. .00009) then
              if (ICYCFL(30) .eq. 1 .and. ICYCSW(2) .eq. 0 .and.
     1            ICYMTP .eq. 1) call cycoff (3)
              ICYCSW(2) = 1
          endif
      endif
c
c...Cycle definition code
c
      ifl(1) = ICYCDO(1) + 1
      ifl(2) = CYCCOD(ifl(1))
      rv(2) = CYCCDV(ifl(1))
      call regtyp (ifl(2),rv(2))
c
c...Cycle plane code
c
      if (ICYCFL(3) .eq. 1) then
          ifl(19) = PLCYCD(CYCPAX)
          rv(19) = CYPCDV(CYCPAX)
          call regtyp (ifl(19),rv(19))
      end if
c
c...Cycle position
c
      ifl(3) = 1
c
c...Final depth
c
      rv(4) = gfin(ks(3))
      ifl(4) = 1
c
c...Rapto position
c
      call cymrpl (ks,gpos,rv(5),ifl(5))
c
c......Top-of-part
c
      if (CYCREG(15) .ne. 0) then
          if (ICYCFL(30) .eq. 1) then
              call codint (ireg,gpos(1),rnum,inum)
          else
              call codint (CYCREG(12),gpos(1),rnum,inum)
          endif
          if (rnum .ne. CYCPSV(7) .or. ICYCSW(2) .eq. 1 .or.
     1        ICYCFL(30) .eq. 1) ifl(18) = 1
          CYCPSV(7) = rnum
          rv(18) = gpos(1)
          if (ICYCFL(7) .eq. 3 .or. (ICYCFL(7) .eq. 1 .and.
     1        INCR .eq. 2)) rv(18) = gpos(1) - CYCPSV(4)
      endif
c
c......RTRCTO (1st)
c
      if ((ICYCDO(8) .ne. 0 .or. (ICYCFL(28) .eq. 1 .and.
     1    ICYCFL(9) .eq. 1 .and. ICYCSW(3) .eq. 1) .or.
     2    ICYCFL(28) .eq. 3)  .and. CYCREG(12) .ne. 0) then
          call cymrtc (ks,gpos,rv(15),ifl(15))
c
c.....Temporary fix vp 13-jun-94 wait for Bob
c         if (gpos(3) .gt. gpos(2)) ifl(15) = 1
          if ((gpos(3)-gpos(2))*ks(4) .gt. 0.d0 .or.
     1        ICYCFL(28) .eq. 3 .or. ICYCDO(8) .ne. 0) ifl(15) = 1
          CYCPSV(6) = rv(15)
      endif
c
c...Feedrate
c
      ifl(6) = 1
c
c...Determine the cycle parameter
c...codes that need to be output
c
      if (ICYCSW(2) .eq. 1 .or. ICYCFL(23) .eq. 1) then
c
c......DWELL
c
          if (ICYCDO(5) .ne. 0) then
              if (ICYCDO(5) .ge. 1 .and. RCYCDO(5) .ne. 0. .and.
     1            CYCREG(4) .ne. 0) then
                  ifl(7) = 1
                  if (DELYFL(2) .eq. 1) then
                      rv(7) = RCYCDO(5) * 1000.
                  else if (DELYFL(2) .eq. 2) then
                      rv(7) = RCYCDO(5)
                  else
                      rv(7) = RCYCDO(5) / 60.
                  endif
              endif
c
              if (ICYCDO(5) .eq. 2 .and. RCYCDO(6) .ne. 0 .and.
     1            CYCREG(5) .ne. 0) then
                  ifl(8) = 1
                  if (DELYFL(2) .eq. 1) then
                      rv(8) = RCYCDO(6) * 1000.
                  else if (DELYFL(2) .eq. 2) then
                      rv(8) = RCYCDO(6)
                  else
                      rv(8) = RCYCDO(6) / 60.
                  endif
              endif
          endif
c
c......STEP
c
          if (ICYCDO(6) .ne. 0) then
              if (RCYCDO(7) .ne. 0. .and. CYCREG(6) .ne. 0) then
                  ifl(9) = 1
                  rv(9) = RCYCDO(7)
                  if (ICYCFL(12) .eq. 2) then
                      rv(9) = dabs(rv(9))
                  else if (ICYCFL(12) .eq. 3) then
                      if (rv(9) .gt. 0.) rv(9) = 0. - rv(9)
                  else if (ICYCFL(12) .eq. 4) then
                      rv(9) = dint(dabs(gpos(2)-gfin(ks(3)))/rv(9)) + 1
                  endif
              endif
c
              if (ICYCDO(6) .eq. 2 .and. RCYCDO(8) .ne. 0 .and.
     1            CYCREG(7) .ne. 0) then
                  ifl(10) = 1
                  rv(10) = RCYCDO(8)
                  if (ICYCFL(12) .eq. 2) rv(10) = dabs(rv(10))
                  if (ICYCFL(12) .eq. 3 .and. rv(10) .gt. 0.)
     1                    rv(10) = 0. - rv(10)
              endif
          endif
c
c......OFFSET
c
          if (ICYCDO(7) .eq. 1) then
              if (RCYCDO(9) .ne. 0. .and. CYCREG(8) .ne. 0) then
                  ifl(11) = 1
                  rv(11) = RCYCDO(9)
              endif
c
          else if (ICYCDO(7) .eq. 2) then
              is1    = ks(1) + 11
              is2    = ks(2) + 11
              if (ICYCFL(19) .eq. 1) then
                  is1    = 12
                  is2    = 13
              endif
              if (RCYCDO(9) .ne. 0. .and. CYCREG(is1-3) .ne. 0) then
                  ifl(is1) = 1
                  rv(is1) = RCYCDO(9)
              endif
c
              if (RCYCDO(10) .ne. 0 .and. CYCREG(is2-3) .ne. 0) then
                  ifl(is2) = 1
                  rv(is2) = RCYCDO(10)
              endif
          endif
c
c......RTRCTO (2nd)
c
          if (ICYCDO(8) .eq. 2 .and. RCYCDO(12) .ne. 0 .and.
     1        CYCREG(13) .ne. 0) then
              ifl(16) = 1
              rv(16) = RCYCDO(12)
          endif
c
c......REPEAT
c
          if (ICYCDO(9) .ne. 0) then
              if (RCYCDO(13) .ne. 0. .and. CYCREG(14) .ne. 0) then
                  ifl(17) = 1
                  rv(17) = RCYCDO(13)
              endif
          endif
      endif
c
c...Output cycle block
c
      if (ICYCFL(30) .eq. 1) then
          call cymcal (ks,ifl,rv,gfin,gpos)
      else
          call cymblk (ks,ifl,rv,gfin,gpos)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymblk (ks,kfl,gval,gfin,gpos)
c
c   FUNCTION:  This routine outputs a milling canned cycle block.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           kfl     I*4  D20  -  Contains an array of codes that need
c                                to be output in this block.  (1) =
c                                Which cycle, (2) = Cycle definition
c                                code, (3) = Cycle position, (4) = Fedto
c                                depth, (5) = Rapto plane, (6) = Feed
c                                rate, (7) = DWELL1, (8) = DWELL2, (9) =
c                                STEP1, (10) = STEP2, (11) = OFFSET1,
c                                (12) = OFFSETx, (13) = OFFSETy, (14) =
c                                OFFSETz, (15) = RTRCTO1, (16) = RETRCT2
c                                (17) = REPEAT, (18) = Top-of-part,
c                                (19) = Cycle plane code.
c
c           gval    R*8  D20  -  Values for 'kfl'.
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: kfl     I*4  D18  -  'kfl' will reset to 0 all codes that
c                                have been output.
c
c***********************************************************************
c
      subroutine cymblk (ks,kfl,gval,gfin,gpos)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (ICYCSW,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (USRBRG,KPOSMP(2600)), (MUSRBK,KPOSMP(3022))
      equivalence (NCUSRB,KPOSMP(3025)), (ICYBLK,KPOSMP(3060))
      equivalence (IFOTYP,KPOSMP(3151)), (ICY2FL,KPOSMP(4004))
c
      integer*4 USRBRG(20,20),MUSRBK,NCUSRB(20),ICYCFL(30),CYCREG(20),
     1          ICYCSW(5),REGFRC(MAXFMT),ICYBLK(20),IFOTYP,ICY2FL(20)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
      equivalence (USRBVL,POSMAP(2501)), (RCYCDO,POSMAP(2931))
c
      real*8 DUMMY,REGSTO(MAXFMT),USRBVL(20,20),RCYCDO(20)
c
      integer*4 ks(4),kfl(20)
c
      real*8 gval(20),gfin(3),gpos(3)
c
      integer*4 ist,iblk,ilast,ien,i,ncd,icd(20),idid,idep,ipos,ifl,
     1          ireg,ifed,jreg,inum,j, iax(10)
c
      real*8 rvl(20),rnum
c
      data iax /0,0, 0,0 ,0,0, 0,0, 0,0/
c
c...Initialize routine
c
      ist    = 1
      iblk   = ICYBLK(kfl(1))
      ilast  = 0
c
c...Find next end of block
c...in Cycle user defined block
c
      if (iblk .le. 0 .or. iblk .gt. MUSRBK) then
          ilast  = 1
          go to 400
      endif
  200 do 300 ien=ist,NCUSRB(iblk),1
          if (USRBRG(ien,iblk) .eq. -3) go to 400
  300 continue
      ien    = NCUSRB(iblk)
  400 if (ien .lt. ist) ilast = 1
      if (ilast .eq. 1) ien = ist
      if ((ist .eq. NCUSRB(iblk) .and. USRBRG(ien,iblk) .eq. -3)
     1        .and. iblk .ne. 0) go to 1100
c
c...Output the required cycle codes in this block
c
      ifl    = 0
      idid   = 0
      ifed   = 0
      ipos   = 0
      idep   = 0
      ncd    = 0
      do 700 i=ist,ien,1
          if (ilast .eq. 0) ireg   = USRBRG(i,iblk)
c
c......Machining plane code in cycles
c
          if (ireg .eq. kfl(19) .or.
     1        (ilast .eq. 1 .and. kfl(19) .ne. 0))  then
              if (ilast .eq. 1) ireg = kfl(19)
              call cmpcod (ireg,gval(19),0.d0,0,inum)
              if (inum .eq. 1) ifl = 1
              idid   = 1
              kfl(19) = 0
              call codout (ireg,gval(19))
              if (ilast .eq. 0) go to 700
          endif
c
c......Cycle code
c
          if (ireg .eq. kfl(2) .or. (ilast .eq. 1 .and. kfl(2) .ne. 0))
     1            then
              if (ilast .eq. 1) ireg = kfl(2)
              if (ICYCSW(2) .eq. 1 .and. ICYCFL(20) .eq. 1 .and.
     1            REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
              call cmpcod (ireg,gval(2),0.d0,0,inum)
              if (inum .eq. 1) ifl = 1
              idid   = 1
              kfl(2) = 0
              call codout (ireg,gval(2))
              if (ilast .eq. 0) go to 700
          endif
c
c......Feedrate code
c
          if (ireg .eq. -14 .or. (ilast .eq. 1 .and. kfl(6) .ne. 0))
     1            then
              idid   = 1
              if (kfl(6) .eq. 1) then
                  ifed   = 1
                  ifl    = 1
                  kfl(6) = 0
              endif
              if (ilast .eq. 0) go to 700
          endif
c
c......Position code
c
          if (ireg .eq. -4 .or. (ilast .eq. 1 .and. kfl(3) .ne. 0))
     1                 then
              idid   = 1
              if (kfl(3) .eq. 1) then
                  ipos   = 1
                  ifl    = 1
                  kfl(3) = 0
              endif
              if (ilast .eq. 0) go to 700
          endif
c
c......Final depth code
c
          if (ireg .eq. -8 .or. (ilast .eq. 1 .and. kfl(4) .ne. 0))
     1                 then
              idid   = 1
              if (kfl(4) .eq. 1) then
                  idep   = 1
                  ifl    = 1
                  kfl(4) = 0
              endif
              if (ilast .eq. 0) go to 700
          endif
c
c......Cycle parameter codes
c
          do 500 j=2,15,1
              if (j .eq. 3) go to 500
              jreg   = CYCREG(j)
              call regtyp (jreg,gval(j))
              if (ilast .eq. 1) ireg = jreg
              if (ireg .ne. jreg) go to 500
              idid   = 1
              if (kfl(j+3) .eq. 1) then
                  if ((ICYCSW(2) .eq. 1 .or. (j .ne. 2 .and. j .ne. 12))
     1                .and. REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
                  call codout (ireg,gval(j+3))
                  ifl    = 1
                  kfl(j+3) = 0
              endif
              if (ilast .eq. 0) go to 700
  500     continue
c
c......Unsupported code
c......Buffer it for output
c
          if (ilast .eq. 1) go to 700
          ncd    = ncd    + 1
          icd(ncd) = ireg
          rvl(ncd) = USRBVL(i,iblk)
  700 continue
c
c......Output unsupported codes
c
      if (ncd .ne. 0 .and. (ifl .eq. 1 .or. idid .eq. 0)) then
          do 1000 i=1,ncd,1
              call regtyp (icd(i),rvl(i))
              if (REGFRC(icd(i)) .eq. 0 .and. ICYCSW(2) .eq. 1)
     1                REGFRC(icd(i)) = 4
              if (icd(i) .gt. 0) then
                  if (rvl(i) .eq. DUMMY) rvl(i) = REGSTO(icd(i))
                  call codout (icd(i),rvl(i))
              endif
 1000     continue
      endif
c
c......Output feedrate
c
      if (ifed .eq. 1 .and. ICYCFL(14) .ne. 2) then
          if (IFOTYP .eq. 5 .and. CYCREG(3) .ne. 0) then
              ireg   = CYCREG(3)
              if (ICYCSW(2) .eq. 1 .and. ICYCFL(5) .eq. 1.and.
     1            REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
              rnum   = RCYCDO(2)
              if (kfl(1) .eq. 11 .and. ICY2FL(3) .eq. 1)
     1            rnum = -RCYCDO(2)
              call codout (CYCREG(3),rnum)
          else
              ireg   = -14
              call regtyp (ireg,RCYCDO(2))
              if (ICYCSW(2) .eq. 1 .and. ICYCFL(5) .eq. 1.and.
     1            REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
c
c...added parameter, WNT need examctly parameters
c...Yurong
c
              call frnout(iax)
          endif
      endif
c
c......Output final depth
c
      if (idep .eq. 1) call cymdep (ks,gfin,gpos,0,ireg,rnum)
c
c......Output cycle position
c
      if (ipos .eq. 1) call cympos (ks,gfin,gpos)
c
c......Clear output buffer
c
 1100 if (ilast .eq. 1) call clrbuf
c
c......Go get next block
c
      if (ilast .eq. 1) go to 8000
      if (USRBRG(ien,iblk) .eq. -3) call clrbuf
      ist    = ien    + 1
      go to 200
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymcal (ks,kfl,gval,gfin,gpos)
c
c   FUNCTION:  This routine outputs a milling canned cycle block when
c              cycles are output as Macro calls.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           kfl     I*4  D20  -  Contains an array of codes that need
c                                to be output in this block.  (1) =
c                                Which cycle, (2) = Cycle definition
c                                code, (3) = Cycle position, (4) = Fedto
c                                depth, (5) = Rapto plane, (6) = Feed
c                                rate, (7) = DWELL1, (8) = DWELL2, (9) =
c                                STEP1, (10) = STEP2, (11) = OFFSET1,
c                                (12) = OFFSETx, (13) = OFFSETy, (14) =
c                                OFFSETz, (15) = RTRCTO1, (16) = RETRCT2
c                                (17) = REPEAT, (18) = Top-of-part,
c                                (19) = Cycle plane code.
c
c           gval    R*8  D20  -  Values for 'kfl'.
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: kfl     I*4  D18  -  'kfl' will reset to 0 all codes that
c                                have been output.
c
c***********************************************************************
c
      subroutine cymcal (ks,kfl,gval,gfin,gpos)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (ICYCSW,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (MACCOD,KPOSMP(0801)), (MACPSW,KPOSMP(0859))
      equivalence (MACBLK,KPOSMP(0889)), (CYCPSW,KPOSMP(0890))
      equivalence (ICYMTP,KPOSMP(0920))
      equivalence (ICYBLK,KPOSMP(3060)), (IFOTYP,KPOSMP(3151))
      equivalence (ICY2FL,KPOSMP(4004))
c
      integer*4 ICYCFL(30),CYCREG(20),ICYCSW(5),REGFRC(MAXFMT),
     1          ICYBLK(20),IFOTYP,MACCOD(5),MACBLK,MACPSW(30),ICYMTP,
     2          CYCPSW(30),ICY2FL(20)
c
      equivalence (MACCDV,POSMAP(1184)), (RCYCDO,POSMAP(2931))
      equivalence (MACPRM,POSMAP(4300)), (CYCPRM,POSMAP(4330))
c
      real*8 RCYCDO(20),MACCDV(5),MACPRM(30),CYCPRM(30)
c
      integer*4 ks(4),kfl(20)
c
      real*8 gval(20),gfin(3),gpos(3),rnum
c
      integer*4 i,j,ireg,ictype,ifsav,iax(10)
c
      data iax /0,0, 0,0 ,0,0, 0,0, 0,0/
c
c...Initialize routine
c
      MACBLK = ICYBLK(kfl(1))
      do 100 i=1,30,1
          MACPSW(i) = 0
  100 continue
c
c...Machining plane code in cycles
c
      call codout (kfl(19),gval(19))
c
c...Output feedrate prior to cycle
c
      if (kfl(6) .ne. 0 .and. ICYCFL(14) .eq. 3) then
          if (IFOTYP .ne. 5 .or. CYCREG(3) .eq. 0) then
              ireg   = -14
              call regtyp (ireg,RCYCDO(2))
              if (ICYCSW(2) .eq. 1 .and. ICYCFL(5) .eq. 1.and.
     1            REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
              call frnout (iax)
              kfl(6) = 0
          endif
      endif
c
c...Output initial block
c
      call clrbuf
c
c...Determine type of Macro call to output
c...either Modal or One Shot
c
      if (ICYCSW(2) .eq. 1) then
          call cymclk (ks,gpos,ictype)
      else
          ictype = ICYMTP
      endif
c
c...Output initial position for
c...One Shot cycle
c
      if (ictype .eq. 2) then
          ifsav  = IFOTYP
          call cymcps (ks,kfl,gfin,gpos)
          ICYCSW(2) = 1
          IFOTYP = ifsav
c
c...Output Macro call
c
      else
          if (ICYMTP .eq. 2) ICYCSW(2) = 1
          if (ICYCSW(2) .eq. 1) call codout (MACCOD(1),MACCDV(1))
      endif
      ICYMTP = ictype
c
c...Output Cycle code
c
      if ((ICYCSW(2) .eq. 1 .or. ictype .eq. 2) .and.
     1    ICYCFL(20) .eq. 1 .and. REGFRC(kfl(2)) .eq. 0)
     2        REGFRC(kfl(2)) = 1
      call codout (kfl(2),gval(2))
c
c...Final depth code
c
      if (kfl(4) .eq. 1) then
          call cymdep (ks,gfin,gpos,1,ireg,rnum)
          call codout (ireg,rnum)
      endif
c
c...Cycle parameter codes
c
      do 500 j=2,15,1
          if (j .eq. 3) go to 500
          ireg   = CYCREG(j) + 1000
          if (kfl(j+3) .eq. 1) then
              call codout (ireg,gval(j+3))
              kfl(j+3) = 0
          endif
  500 continue
c
c...Output feedrate as parameter
c
      if (kfl(6) .ne. 0 .and. ICYCFL(14) .eq. 1) then
          if (IFOTYP .eq. 5 .and. CYCREG(3) .ne. 0) then
              rnum   = RCYCDO(2)
              if (kfl(1) .eq. 11 .and. ICY2FL(3) .eq. 1)
     1            rnum = -RCYCDO(2)
              ireg   = CYCREG(3) + 1000
              call codout (ireg,rnum)
              kfl(6) = 0
          endif
      endif
c
c...Output cycle parameter values
c
      do 600 i=1,30,1
          if (CYCPSW(i) .eq. 1) then
              MACPRM(i) = CYCPRM(i)
              MACPSW(i) = CYCPSW(i)
          endif
  600 continue
c
c...Output the Cycle Macro Call block
c
      call clrbuf
c
c...Output position codes for
c...Modal cycles
c
      if (ictype .eq. 1) call cymcps (ks,kfl,gfin,gpos)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymclk (ks,gpos,ktype)
c
c   FUNCTION:  This routine attempts to determine if the next point in a
c              cycle sequence will cause a full cycle block to be output.
c              It is used to determine if a single shot cycle Macro call
c              should be output or a modal cycle Macro call.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c   OUTPUT: ktype   I*4  D1   -  1 = The next point does not require a
c                                full cycle block.  2 = The next block
c                                either requires a full cycle block or
c                                there are no more points in the cycle.
c
c***********************************************************************
c
      subroutine cymclk (ks,gpos,ktype)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (NPT   ,KPOSMP(0059))
c
      integer*4 ITYPE,ISUBT,NPT
c
      equivalence (CLPT  ,POSMAP(0491)), (VECSAV,POSMAP(1372))
      equivalence (ROTBAS,POSMAP(1435))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 CLPT(240),ROTBAS(4),VECSAV(3),ROTANG(20,2),ROTSTO(20,2)
c
      integer*4 ktype,ks(4)
c
      real*8 gpos(3)
c
      integer*4 isw,ierr,ireg,inum
c
      real*8 rmch(3,4),tv(3),laxs(6),raxs(10),rang(20,2),rnum1,rnum2,
     1       rs(20,2),rpos(3)
c
c...Get next clfile record
c
      isw    = 0
  100 call clsamp (isw)
c
c...End of cycle sequence
c
      if (ITYPE .eq. 14000 .or. (ITYPE .eq. 2000 .and.
     1    (ISUBT .eq. 5 .or. ISUBT .eq. 1054))) then
          ktype  = 2
          go to 8000
c
c...Motion record
c...Determine if Z-level has changed
c
      else if (ITYPE .eq. 5000) then
c
c......Adjust points to rotaries
c
          rmch(1,2) = CLPT(1)
          rmch(2,2) = CLPT(2)
          rmch(3,2) = CLPT(3)
          if (NPT .eq. 3) then
             tv(1) = VECSAV(1)
             tv(2) = VECSAV(2)
             tv(3) = VECSAV(3)
          else
             tv(1) = CLPT(4)
             tv(2) = CLPT(5)
             tv(3) = CLPT(6)
          endif
          call cpyrot (ROTSTO,rs)
          call cpyrot (ROTANG,ROTSTO)
          call tlaxis (rmch,tv,laxs,raxs,rang,ROTBAS,0,0,ierr)
          call cpyrot (rs,ROTSTO)
c
c......Compare Z-level of next point
c......to current point, if different
c......then next block requires a
c......full cycle block
c
          call ptxfm (rmch(1,4),rpos,1)
          call gmotrg (ks(3),ireg)
          call codint (ireg,rpos(ks(3)),rnum1,inum)
          call codint (ireg,gpos(1),rnum2,inum)
          ktype  = 1
          if (rnum1 .ne. rnum2) ktype = 2
          go to 8000
c
c...Unrecognized clfile record type
c
      else
          go to 100
      endif
c
c...End of routine
c
 8000 call clsamp (-1)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymcps (ks,kfl,gfin,gpos)
c
c   FUNCTION:  This routine outputs the positioning block for cycles
c              that are output as Macro calls.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           kfl     I*4  D20  -  (6) = Feed rate.
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymcps (ks,kfl,gfin,gpos)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (ICYCSW,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (NRAPCD,KPOSMP(3190)), (RAPCD ,KPOSMP(3191))
c
      integer*4 ICYCFL(30),ICYCSW(5),REGFRC(MAXFMT),NRAPCD,RAPCD(5)
c
      equivalence (RCYCDO,POSMAP(2931)), (RAPVL ,POSMAP(3577))
c
      real*8 RCYCDO(20),RAPVL(5)
c
      integer*4 ks(4),kfl(20)
c
      real*8 gfin(3),gpos(3)
c
      integer*4 i,ireg,iax(10)
c
      data iax /0,0, 0,0 ,0,0, 0,0, 0,0/
c
c...Output position codes
c
      do 100 i=1,NRAPCD,1
          call codout (RAPCD(i),RAPVL(i))
  100 continue
c
c...Output feedrate
c
      if (kfl(6) .ne. 0 .and. ICYCFL(14) .eq. 1) then
          ireg   = -14
          call regtyp (ireg,RCYCDO(2))
          if (ICYCSW(2) .eq. 1 .and. ICYCFL(5) .eq. 1.and.
     1        REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
          call frnout (iax)
      endif
c
c...Output cycle position
c
      call cympos (ks,gfin,gpos)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cympos (ks,gfin,gpos)
c
c   FUNCTION:  This routine outputs the axes positions within a milling
c              canned cycle block.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: none.
c
c
c***********************************************************************
c
      subroutine cympos (ks,gfin,gpos)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (ICYCSW,KPOSMP(0271))
c
      integer*4 ICYCFL(30),ICYCSW(5)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3)
c
      integer*4 ks(4)
c
      real*8 gpos(3),gfin(3)
c
      integer*4 ifl(10),isw(2,3),icnt
c
      data isw /1,2, 3,4, 5,6/
c
c...Set up current position
c
      call ptxfr (gfin,MCHNUM(1,4),1)
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
c
c...Determine which axes to output
c
      ifl(1) = 0
      ifl(2) = 0
      ifl(3) = 0
      if (ICYCSW(2) .eq. 1) then
          if (ICYCFL(13) .eq. 1 .or. ICYCFL(29) .eq. 1) then
              ifl(ks(1)) = 1
              ifl(ks(2)) = 1
          endif
c          if (kfin .eq. 1) ifl(ks(3)) = 1
          call frcmot (2,4,ifl)
c
c...Force linear axes output
c...10/26/92 added cycle option 8.5.3
c
      else if (ICYCFL(29) .eq. 1) then
          ifl(ks(1)) = 1
          ifl(ks(2)) = 1
          call frcmot (2,4,ifl)
      endif
      call whchax (AXSOUT,ifl,icnt)
c
c...Don't output final depth
c
      if (ifl(isw(1,ks(3))) .eq. 1) then
          ifl(isw(1,ks(3))) = 0
          icnt   = icnt   - 1
      endif
      if (ifl(isw(2,ks(3))) .eq. 1) then
          ifl(isw(2,ks(3))) = 0
          icnt   = icnt   - 1
      endif
c
c...Output motion block
c
      if (icnt .gt. 0) call motout (AXSOUT,ifl,icnt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cymdep (ks,gfin,gpos,kmac,kreg,gval)
c
c   FUNCTION:  This routine outputs the final depth within a milling
c              canned cycle block.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gfin    R*8  D3   -  The linear axes position at the bottom
c                                of the hole.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c           kmac    I*4  D1   -  1 = The cycle block is being formatted
c                                as a Macro call (Siemens 840D).
c
c   OUTPUT: kreg    I*4  D1   -  Macro argument register for final depth
c                                when 'kmac' = 1.
c
c           gval    R*8  D1   -  Final depth value when 'kmac' = 1.
c
c***********************************************************************
c
      subroutine cymdep (ks,gfin,gpos,kmac,kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (ICYCSW,KPOSMP(0271))
      equivalence (CYCREG,KPOSMP(0231)), (MOTREG,KPOSMP(0381))
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (MOTFLG,KPOSMP(1073)), (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205))
      equivalence (INCR  ,KPOSMP(1226)), (ACTBLN,KPOSMP(1336))
      equivalence (MLNFRC,KPOSMP(1373))
c
      integer*4 ICYCFL(30),ICYCSW(5),CYCREG(20),INCR,MOTFLG,MOTREG(24),
     1          ACTLIN(3),REGFRC(MAXFMT),ACTBLN(3),NUMLIN(3),MLNFRC
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (CYCVR ,POSMAP(2926))
      equivalence (CYCPSV,POSMAP(2951)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       CYCPSV(5),CYCVR(5)
c
      integer*4 ks(4),kmac,kreg
c
      real*8 gpos(3),gfin(3),gval
c
      integer*4 isw(2,3),isub(2,3),imod,ireg,inc,inum,ipt
c
      real*8 rval,rpos(2)
c
      data isw /1,2, 3,4, 5,6/
      data isub /1,3, 5,7, 9,11/
c
c...Set current position
c
      call ptxfr (gfin,MCHNUM(1,4),1)
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
c
c...Set up final depth value
c
      ipt    = ACTLIN(ks(3))
      imod   = INCR
      if (ICYCFL(4) .ne. 1) imod = ICYCFL(4) - 1
c
c......Loop required when both
c......Primary and Secondary axes are active
c
  100 ireg    = CYCREG(1)
      inc    = isub(ipt,ks(3))
      rval   = AXSOUT(isw(ipt,ks(3)))
      rpos(ipt) = rval
      if (ireg .eq. 0 .or. ICYCFL(30) .eq. 1) ireg = MOTREG(inc)
      call codint (ireg,rpos(ipt),rpos(ipt),inum)
c
c.........Incremental mode
c
       if (imod .eq. 2) then
           if (ireg .eq. MOTREG(inc)) ireg = MOTREG(inc+1)
           if (ICYCFL(6) .eq. 1) then
               call setcod (MOTREG(inc),gpos(2))
           else if (ICYCFL(6) .eq. 2) then
               call setcod (MOTREG(inc),CYCPSV(4))
           else
               call setcod (MOTREG(inc),gpos(1))
           endif
           call increg (MOTREG(inc),rpos(ipt),rval)
           rval   = rval   + CYCVR(2) * ks(4)
       endif
c
c.........Return the final depth value
c.........for Macro Cycle call
c
       if (kmac .eq. 1) then
           kreg   = CYCREG(1) + imod - 1 + 1000
           gval   = rval
           if (imod .eq. 2) gval = dabs(gval)
           go to 8000
       endif
c
c.........Output this axis
c
       if ((ICYCSW(2) .eq. 1 .or. MLNFRC .eq. 1) .and.
     1     REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
       if (rpos(ipt) .ne. CYCPSV(1) .or. REGFRC(ireg) .gt. 0) then
           MOTFLG = 1
           call codout (ireg,rval)
       endif
c
c......End of loop
c
      if (ACTBLN(ks(3)) .eq. 1 .and. ipt .eq. ACTLIN(ks(3)) .and.
     1    NUMLIN(ks(3)) .eq. 2) then
          ipt    = 1
          if (ACTLIN(ks(3)) .eq. 1) ipt = 2
          go to 100
      endif
c
c...End of routine
c
 8000 CYCPSV(1) = rpos(ACTLIN(ks(3)))
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cymret (ks,gpos,kfl)
c
c   FUNCTION:  This routine outputs the final depth within a milling
c              canned cycle block.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: kfl     I*4  D1   -  Set to 1 when the tool should be
c                                retracted using normal motion logic.
c
c***********************************************************************
c
      subroutine cymret (ks,gpos,kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYCSW,KPOSMP(0271)), (CYCREG,KPOSMP(0231))
c
      integer*4 ICYCFL(30),CYCCOD(20),ICYCSW(5),CYCREG(20)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (CYCCDV,POSMAP(2901))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       CYCCDV(25),ROTSTO(20,2),STONUM(3,4)
c
      integer*4 ks(4),kfl
c
      real*8 gpos(3)
c
      integer*4 ifl
c
      real*8 rnum,rmch(3),stnum(3)
c
c...Turn off cycle and retract to
c...New rapto plane
c
      if (ICYCFL(11) .eq. 1) then
          call pshaxs (AXSOUT,TLVEC)
          call cycoff (1)
          call ptxfm (STONUM(1,4),stnum,1)
          rmch(ks(1)) = stnum(ks(1))
          rmch(ks(2)) = stnum(ks(2))
          rmch(ks(3)) = gpos(2)
          call ptxfr (rmch,MCHNUM(1,4),1)
          call cpyrot (ROTSTO,ROTANG)
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
          call rapset (1,2)
          ICYCSW(4) = 1
          kfl    = 1
c
c...Output Cycle off with
c...New retract plane
c
      else
          call codout (CYCCOD(1),CYCCDV(1))
          call cymrpl (ks,gpos,rnum,ifl)
          call codout (CYCREG(2),rnum)
          call clrbuf
C          ICYCSW(2) = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymrpl (ks,gpos,gval,kfl)
c
c   FUNCTION:  This routine calculates the output value for the current
c              cycle rapto plane location and determines if it needs to
c              be output.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: gval    R*8  D1   -  Value to output for rapto plane code.
c
c           kfl     I*4  D1   -  1 = Rapto plane needs to be output.
c
c***********************************************************************
c
      subroutine cymrpl (ks,gpos,gval,kfl)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (CYCREG,KPOSMP(0231)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205))
      equivalence (INCR  ,KPOSMP(1226)), (ICY2FL,KPOSMP(4004))
c
      integer*4 ICYCFL(30),CYCREG(20),ICYCSW(5),ICYCDO(15),INCR,
     -          ICY2FL(20),NUMLIN(3),ACTLIN(3)
c
      equivalence (CYCVR ,POSMAP(2926)), (CYCPSV,POSMAP(2951))
c
      real*8 CYCPSV(10),CYCVR(5)
c
      integer*4 ks(4),kfl
c
      real*8 gpos(3),gval
c
      integer*4 inum,inc,imod,ireg
c
      real*8 rnum,rp(3),ap(6)
c
c...Determine Incremental or Absolute mode
c
      imod   = 1
      if (ICYCFL(8) .eq. 3 .or. (ICYCFL(8) .eq. 1 .and. INCR .eq. 2))
     1        imod = 2
      if (ICYCFL(8) .eq. 4) imod = 3
c
c...Calculate Rapto plane using assigned
c...method for linear axis
c
      rnum = gpos(2)
      if (NUMLIN(ks(3)) .eq. 2 .and. ICY2FL(1) .ne. 1 .and. imod .eq. 1)
     1        then
          rp(1) = 0.
          rp(2) = 0.
          rp(3) = 0.
          rp(ks(3)) = rnum
          call linclc (rp,ap)
          inc    = (ks(3)-1) * 2 + ACTLIN(ks(3))
          rnum   = ap(inc)
      endif
c
c...Determine if rapto plane should be output
c
      if (ICYCFL(30) .eq. 1) then
          call gmotrg (ks(3),ireg)
          call codint (ireg,rnum,gval,inum)
      else
          call codint (CYCREG(2),rnum,gval,inum)
      endif
      if (ICYCDO(4) .eq. 1 .and. (gval .ne. CYCPSV(2) .or.
     1    ICYCSW(2) .eq. 1 .or. ICYCFL(30) .eq. 1)) kfl = 1
      CYCPSV(2) = gval
c
c...Incremental mode
c
      if (imod .eq. 2) gval = gval - CYCPSV(4)
      if (imod .eq. 3) gval = gval - gpos(1)
      gval   = gval   - CYCVR(2) * ks(4)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymtim (ks,gpos)
c
c   FUNCTION:  This routine estimates the maching time and distances for
c              milling canned cycles.
c
c   INPUT:  ks      I*4  D3   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cymtim (ks,gpos)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (ICYTIM,KPOSMP(0251))
      equivalence (ICYCDO,KPOSMP(0276)), (MCHOPT,KPOSMP(0308))
      equivalence (LTHDIA,KPOSMP(1228)), (ICYCSW,KPOSMP(0271))
      equivalence (ITMDLT,KPOSMP(1740)), (ICYDLT,KPOSMP(1742))
      equivalence (IFITYP,KPOSMP(3150))
      equivalence (IFOTYP,KPOSMP(3151)), (IFDOUT,KPOSMP(3158))
c
      integer*4 ICYCFL(30),ICYCDO(15),ICYTIM(20),MCHOPT(20),
     1          IFOTYP,LTHDIA(2),ICYCSW(5),IFDOUT(4),IFITYP,
     2          ICYDLT,ITMDLT
c
      equivalence (CUTTER,POSMAP(0744)), (MCHNUM,POSMAP(1287))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387))
      equivalence (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (RCYCDO,POSMAP(2931))
      equivalence (FEDRNG,POSMAP(3512)), (MOVTIM,POSMAP(3546))
      equivalence (FEED  ,POSMAP(3547)), (FMVTIM,POSMAP(3553))
      equivalence (RAPLMT,POSMAP(3567)), (ROTSTO,POSMAP(5123))
c
      real*8 MCHNUM(3,4),STONUM(3,4),RCYCDO(20),FEED(4),RAPLMT(10),
     1       MOVTIM,CUTTER(7),AXSOUT(10),FEDRNG(2,8),TLVEC(3),LINSTO(6),
     2       ROTSTO(20,2),AXSSTO(10),FMVTIM
c
      integer*4 ks(4),mks(3)
c
      real*8 gpos(3)
c
      integer*4 inc,npck,inum,iary(10),i,j
c
      real*8 rtim,rrap,rtix,rtiy,rplg,rdep,rret,rshf,fed,tmch(3,4),
     1       tlin(6),trot(20,2),taxs(10),tmst(3,4),trst(20,2),tlst(6),
     2       tast(10),rmch(3),stnum(3),rtmp(3),qaxs(10),qsto(10),qtim,
     3       mtim
c
c...Calculate current RPM/SFM
c
      call sfmctl (AXSOUT,CUTTER(1),LTHDIA(2))
c
c...Reset travel distances
c...And store point coming from
c
      if (ICYDLT .eq. 1) call tmpdrs
      call cpyrot (ROTSTO,trst)
      do 200 i=1,4,1
          do 100 j=1,3,1
              tmch(j,i) = MCHNUM(j,i)
              tmst(j,i) = STONUM(j,i)
  100     continue
  200 continue
      do 250 i=1,6,1
          tlst(i) = LINSTO(i)
  250 continue
      do 280 i=1,10,1
          tast(i) = AXSSTO(i)
          qaxs(i) = 0.
          qsto(i) = 0.
  280 continue
      do i=1,2
        do j=1,20,1
          trot(j,i) = 0.0d0
        enddo
      enddo
c
c...Check machine limits
c
      call lmtchk (AXSOUT,inum,iary,1)
c
c...Calculate machining times
c...Set distances traveled first
c
      call ptxfm (MCHNUM(1,4),rmch,1)
      call ptxfm (STONUM(1,4),stnum,1)
      call axssub (ks,mks,2)
      inc    = ICYTIM(ICYCDO(1))
      rtim   = 0.
      mtim   = 0.
      rrap   = RCYCDO(3)
      if (rrap .eq. 0. .or. rrap .ge. 10000.) rrap = RAPLMT(mks(3))
      rplg   = stnum(ks(3)) - gpos(2)
      rdep   = dabs(RCYCDO(1)+RCYCDO(4))
      rret   = gpos(3)-gpos(2)
c
c...Positioning time
c
      qaxs(mks(1)) = rmch(ks(1)) - stnum(ks(1))
      qaxs(mks(2)) = rmch(ks(2)) - stnum(ks(2))
      rtix   = dabs(qaxs(ks(1)*2-1)) / RAPLMT(mks(1))
      rtiy   = dabs(qaxs(ks(2)*2-1)) / RAPLMT(mks(2))
      rtim   = rtix
      if (rtix .lt. rtiy) rtim = rtiy
      call acltim (qsto,qaxs,rtim,0)
      qaxs(ks(1)*2-1) = 0.
      qaxs(ks(2)*2-1) = 0.
c
c......Positioning distances
c
      if (ICYDLT .eq. 1) then
          tmch(ks(3),4) = STONUM(ks(3),4)
          call alladj (tmch,tlin,taxs,trot,4,5)
          call alladr (taxs,tlin,tmch,trot,TLVEC,3,1)
          call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,1)
cc          if (ICYCSW(2) .ne. 0) then
              call ptxfm (tmch(1,4),rtmp,1)
              if (rret .gt. 0) then
                  rtmp(ks(3)) = gpos(3)
              else
                  rtmp(ks(3)) = gpos(2)
              endif
              call ptxfr (rtmp,tmch(1,4),1)
              call alladj (tmch,tlin,taxs,trot,4,5)
              call alladr (taxs,tlin,tmch,trot,TLVEC,3,1)
              call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,1)
cc          endif
      endif
c
c...10/26/92 add plunge time if clearance plane above
c...rapto plane othervise add time only for the first cycle
c
      qtim   = 0.
      if (rplg .gt. 0.) then
          qtim = rplg / RAPLMT(mks(3))
      else if (ICYCSW(2) .eq. 1) then
          qtim = dabs(rplg) / RAPLMT(mks(3))
      endif
      mtim   = mtim   + qtim
      qaxs(mks(3)) = rplg
      call acltim (qsto,qaxs,qtim,0)
      qaxs(mks(3)) = 0.
      rtim   = rtim   + qtim
c
c......Feed in / Feed out
c......BORE
c......BORE7
c......REAM
c......REVERS
c......TAP
c
      fed = FEED(1)
      if (inc .eq. 1 .or. inc .eq. 2 .or. inc .eq. 9 .or.
     1    inc .eq. 10 .or. inc .eq. 12) then
          qtim   = ((rdep / fed) * 2.)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rdep * 2.
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
          if (ICYCDO(5) .ne. 0) then
              rtim = rtim + (RCYCDO(5) / 60.)
              mtim = mtim + (RCYCDO(5) / 60.)
          endif
c
c......Feed in / Rapid out
c......BORE8
c......BORE9
c......DRILL
c......FACE
c......SHIFT
c
      else if (inc .eq. 3 .or. inc .eq. 4 .or. inc .eq. 6 .or.
     1         inc .eq. 7 .or. inc .eq. 11) then
          qtim   = (rdep / fed)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rdep
          call acltim (qsto,qaxs,qtim,0)
          rtim   = rtim   + qtim
          qtim   = (rdep / rrap)
          mtim   = mtim   + qtim
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
          if (ICYCDO(5) .ne. 0) then
              rtim = rtim + (RCYCDO(5) / 60.)
              mtim = mtim + (RCYCDO(5) / 60.)
          endif
c
c.........BORE9, SHIFT Offset
c
          if ((inc .eq. 4 .or. inc .eq. 11) .and.
     1        ICYCDO(7) .ne. 0) then
              rshf   = RCYCDO(9)
              if (ICYCDO(7) .eq. 2)
     1                rshf = dsqrt(RCYCDO(9)**2 + RCYCDO(10)**2)
              npck   = 3
              if (inc .eq. 11) npck = 2
              qtim   = ((rshf / rrap) * npck)
              mtim   = mtim   + qtim
              qaxs(mks(1)) = rshf * npck
              call acltim (qsto,qaxs,qtim,0)
              qaxs(mks(1)) = 0.
              rtim   = rtim   + qtim
              if (ICYCDO(5) .ne. 0) then
                  rtim = rtim + (RCYCDO(5) / 60.)
                  mtim = mtim + (RCYCDO(5) / 60.)
              endif
          endif
c
c......Pecking cycles
c......DEEP
c......THRU
c
      else if (inc .eq. 5 .or. inc .eq. 13) then
c
c.........Calculate number of pecks
c
          rshf   = RCYCDO(7)
          if (ICYCDO(6) .eq. 2 .and. ICYCFL(18) .eq. 1)
     1        rshf = (RCYCDO(7) + RCYCDO(8)) / 2.
          if (rshf .ne. 0.) rshf   = (rdep / rshf) + 1.
          npck   = rshf
          if (ICYCFL(17) .ne. 1 .or. npck .lt. 1) npck = 1
c
c.........Calculate machining times
c
          qtim   = (rdep / fed)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rdep * npck
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
          qtim   = (rdep / rrap)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rdep
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
          if (inc .eq. 5 .and. ICYCDO(8) .ne. 0) then
              qtim = ((RCYCDO(11) / rrap) * npck)
              mtim   = mtim   + qtim
              qaxs(mks(3)) = RCYCDO(11) * npck
              call acltim (qsto,qaxs,qtim,0)
              qaxs(mks(3)) = 0.
              rtim   = rtim   + qtim
              qtim = ((RCYCDO(11) / fed) * npck)
              mtim   = mtim   + qtim
              qaxs(mks(3)) = RCYCDO(11) * npck
              call acltim (qsto,qaxs,qtim,0)
              qaxs(mks(3)) = 0.
              rtim   = rtim   + qtim
          else if (inc .eq. 13) then
              if (ICYCDO(8) .lt. 2) then
                  rshf   = .1
                  if (MCHOPT(2) .eq. 2) rshf   = 2.54
              else
                  rshf   = RCYCDO(12)
              endif
              qtim = ((rdep / rrap) * npck)
              mtim   = mtim   + qtim
              qaxs(mks(3)) = rdep * npck
              call acltim (qsto,qaxs,qtim,0)
              qaxs(mks(3)) = 0.
              rtim   = rtim   + qtim
              qtim = ((rshf / fed) * npck)
              mtim   = mtim   + qtim
              qaxs(mks(3)) = rshf * npck
              call acltim (qsto,qaxs,qtim,0)
              qaxs(mks(3)) = 0.
              rtim   = rtim   + qtim
          endif
          if (ICYCDO(5) .ne. 0) then
              rtim = rtim + ((RCYCDO(5) / 60.) * npck)
              mtim = mtim + ((RCYCDO(5) / 60.) * npck)
          endif
          if (ICYCDO(5) .eq. 2) then
              rtim = rtim + ((RCYCDO(6) / 60.) * npck)
              mtim = mtim + ((RCYCDO(6) / 60.) * npck)
          endif
c
c......Feed in
c......MILL
c
      else if (inc .eq. 8) then
          qtim   = (rdep / fed)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rdep
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
          if (ICYCDO(5) .ne. 0) then
              rtim = rtim + (RCYCDO(5) / 60.)
              mtim = mtim + (RCYCDO(5) / 60.)
          endif
      endif
c
c......Check machine limits
c......At hole bottom
c
      call ptxfm (tmch(1,4),rtmp,1)
      rtmp(ks(3)) = gpos(2) - rdep
      call ptxfr (rtmp,tmch(1,4),1)
      call alladj (tmch,tlin,taxs,trot,4,5)
      call alladr (taxs,tlin,tmch,trot,TLVEC,3,1)
      call lmtchk (taxs,inum,iary,1)
c
c......Calculate postioning deltas
c
      if (ICYDLT .eq. 1) then
          call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,1)
          if (inc .ne. 8) then
              call ptxfm (tmch(1,4),rtmp,1)
              if (rret .gt. 0) then
                  rtmp(ks(3)) = gpos(3)
              else
                  rtmp(ks(3)) = gpos(2)
              endif
              call ptxfr (rtmp,tmch(1,4),1)
              call alladj (tmch,tlin,taxs,trot,4,5)
              call alladr (taxs,tlin,tmch,trot,TLVEC,3,1)
              call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,0)
          endif
      endif
c
c...Retract time
c
      if (rret .gt. 0.) then
          qtim   = (rret / rrap)
          mtim   = mtim   + qtim
          qaxs(mks(3)) = rret
          call acltim (qsto,qaxs,qtim,0)
          qaxs(mks(3)) = 0.
          rtim   = rtim   + qtim
      endif
c
c...Calculate machining distances
c
      ITMDLT = ICYDLT
      call mchdis (1)
      ITMDLT = 2
c
c...Accumulate machining time
c
      MOVTIM = rtim
      FMVTIM = mtim
      call addtim (rtim)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cymrtc (ks,gpos,gval,kfl)
c
c   FUNCTION:  This routine calculates the output value for the current
c              cycle rapto plane location and determines if it needs to
c              be output.
c
c   INPUT:  ks      I*4  D4   -  Subscript pointer to MCHNUM set up for
c                                the active cycle plan.  'ks(4)' con-
c                                tains the major tool axis direction
c                                (1 or -1).
c
c           gpos    R*8  D3   -  (1) = Cycle axis top of part, (2) =
c                                Rapto plane, (3) = Retract plane.
c
c   OUTPUT: gval    R*8  D1   -  Value to output for retract plane code.
c
c           kfl     I*4  D1   -  1 = Retract plane needs to be output.
c
c***********************************************************************
c
      subroutine cymrtc (ks,gpos,gval)
c
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181))
      equivalence (CYCREG,KPOSMP(0231)), (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205))
      equivalence (INCR  ,KPOSMP(1226)), (ICY2FL,KPOSMP(4004))
c
      integer*4 ICYCFL(30),CYCREG(20),ICYCSW(5),ICYCDO(15),INCR,
     -          ICY2FL(20),NUMLIN(3),ACTLIN(3)
c
      equivalence (CYCVR ,POSMAP(2926)), (CYCPSV,POSMAP(2951))
c
      real*8 CYCPSV(10),CYCVR(5)
c
      integer*4 ks(4)
c
      real*8 gpos(3),gval
c
      integer*4 inc,imod
c
      real*8 rnum,rp(3),ap(6)
c
c...Determine Incremental or Absolute mode
c
      imod   = ICYCFL(27)
c
c...Calculate Retract plane using assigned
c...method for linear axis
c
      rnum   = gpos(3)
      if (NUMLIN(ks(3)) .eq. 2 .and. ICY2FL(2) .ne. 1 .and. imod .eq. 1)
     1        then
          rp(1) = 0.
          rp(2) = 0.
          rp(3) = 0.
          rp(ks(3)) = rnum
          call linclc (rp,ap)
          inc    = (ks(3)-1) * 2 + ACTLIN(ks(3))
          rnum   = ap(inc)
      endif
c
c...RTRCTO code
c
      if (imod .eq. 1) then
          gval = rnum
      else if (imod .eq. 2) then
          gval = gpos(3) - gpos(2) + CYCVR(2)
      else if (imod .eq. 3) then
          gval = gpos(3) - gpos(1)
      endif
c
      return
      end
