c
c***********************************************************************
c
c   FILE NAME:  cylaut
c   CONTAINS:
c               cylaut  cylout  cylblk  cylpos  cyltim
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cylaut.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:05
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: cylaut (kfl)
c
c   FUNCTION:  This is the controlling routine for automatic Lathe
c              cycles.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or with one of the following values:
c
c                                  1 = Manual retract generated.
c                                  3 = Postion at taper for THREAD,STEP
c                                      & TURN,STEP.
c
c                               When 'kfl' is non-zero then this routine
c                               should be called again immediately after
c                               the returned motion block is output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylaut (kfl)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271))
      equivalence (ICYCDO,KPOSMP(0276)), (INCR  ,KPOSMP(1226))
      equivalence (ICYLFL,KPOSMP(1901)), (IFITYP,KPOSMP(3150))
      equivalence (IFOTYP,KPOSMP(3151)), (IFDOUT,KPOSMP(3158))
c
      integer*4 IFITYP,IFOTYP,IFDOUT(4),ICYCDO(15),ICYLFL(20),ICYCSW(5),
     1          INCR
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931)), (PFEED ,POSMAP(3540))
      equivalence (FEED  ,POSMAP(3547)), (OFEED ,POSMAP(3560))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),AXSSTO(10),STONUM(3,4),RCYCDO(20),PFEED(4),
     1       FEED(4),OFEED(6),LINAXS(6),AXSOUT(10),TLVEC(3),ROTANG(20,2)
c
      integer*4 kfl
c
      integer*4 i,idid,isav, iax(10)
c
      real*8 rfin(3),rpos(10)
      data iax /0,0,0,0,0,0,0,0,0,0/
c
c...Second time here
c...Pop previous position from stack
c
      idid   = 0
      if (kfl .ne. 0) then
          call cyc2nd (1)
          ICYCSW(4) = 0
          kfl    = 0
          idid   = 1
      endif
c
c...Save current position
c
      do 100 i=1,10,1
          rpos(i) = AXSSTO(i)
  100 continue
c
c...Set up final cycle position
c
      rfin(1) = MCHNUM(1,4)
      rfin(2) = MCHNUM(2,4)
      rfin(3) = MCHNUM(3,4)
c
c...Position tool prior to THREAD,STEP & TURN,STEP
c
      if (((ICYCDO(1) .eq. 10 .and. ICYLFL(9) .eq. 1) .or.
     1    (ICYCDO(1) .eq. 14 .and. ICYLFL(10) .eq. 1)) .and.
     2    idid .eq. 0) then
          call pshaxs (AXSOUT,TLVEC)
          call cyloff
          MCHNUM(1,2) = MCHNUM(1,2) + RCYCDO(9)
          MCHNUM(2,2) = STONUM(2,2)
          MCHNUM(3,2) = STONUM(3,2)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,1,5)
          call rapset (1,2)
          kfl    = 1
          go to 8000
      endif
c
c...Calculate cutting feedrate
c
      if (ICYCDO(6) .ne. 0) then
          IFITYP = 5
          IFOTYP = 5
          PFEED(2) = dsqrt(RCYCDO(6)**2 + RCYCDO(7)**2)
          call fedctl(iax)
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
      call cyltim
c
c...Output cycle block
c
      isav   = INCR
      IF (ICYLFL(2) .ne. 1) INCR = ICYLFL(2) - 1
      call cylout (rfin)
c
c...Set current position
c
      do 500 i=1,10,1
          AXSSTO(i) = rpos(i)
  500 continue
      call setpos
c
c...Cancel cycle
c
      INCR   = isav
      if (ICYLFL(11) .eq. 1) call cyloff
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylout (gfin)
c
c   FUNCTION:  This routine determines which codes need to be output on
c              a lathe canned cycle block and calls a routine to out-
c              put them.
c
c   INPUT:  gfin    R*8  D3   -  The linear axes position at the end
c                                of the cycle.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylout (gfin)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
      equivalence (ICYLFL,KPOSMP(1901)), (CYLCOD,KPOSMP(1921))
      equivalence (CYLREG,KPOSMP(1946))
c
      integer*4 ICYLFL(20),CYLCOD(25),CYLREG(25),ICYCSW(5),ICYCDO(15)
c
      equivalence (RCYCDO,POSMAP(2931)), (CYLCDV,POSMAP(2968))
c
      real*8 CYLCDV(25),RCYCDO(20)
c
      real*8 gfin(3)
c
      integer*4 ifl(20),i
c
      real*8 rv(20)
c
c...Clear output flags
c
      do 100 i=1,18,1
          ifl(i) = 0
  100 continue
c
c...Cycle definition code
c
      ifl(1) = ICYCDO(1) + 4
      ifl(2) = CYLCOD(ifl(1))
      rv(2) = CYLCDV(ifl(1))
      call regtyp (ifl(2),rv(2))
c
c...Cycle position
c
      ifl(3) = 1
c
c...Cycle depth
c
      ifl(19) = 1
c
c...Feedrate
c
      ifl(5) = 1
c
c...Determine the cycle parameter
c...codes that need to be output
c
      if (ICYCSW(2) .eq. 1 .or. ICYLFL(6) .eq. 1) then
c
c......FEDTO
c
          if (ICYCDO(4) .ne. 0) then
              if (RCYCDO(3) .ne. 0. .and. CYLREG(7) .ne. 0) then
                  ifl(4) = 1
                  rv(4) = RCYCDO(3)
              endif
          endif
c
c......RAPTO
c
          if (ICYCDO(7) .ne. 0) then
              if (RCYCDO(8) .ne. 0. .and. CYLREG(10) .ne. 0) then
                  ifl(6) = 1
                  rv(6) = RCYCDO(8)
              endif
          endif
c
c......STEP
c
          if (ICYCDO(8) .ne. 0) then
              if (RCYCDO(9) .ne. 0. .and. CYLREG(14) .ne. 0) then
                  ifl(7) = 1
                  rv(7) = RCYCDO(9)
                  if (ICYLFL(3) .eq. 2) then
                      rv(7) = dabs(rv(7))
                  else if (ICYLFL(3) .eq. 3) then
                      if (rv(7) .gt. 0.) rv(7) = 0. - rv(7)
                  endif
              endif
c
              if (ICYCDO(8) .eq. 2 .and. RCYCDO(10) .ne. 0. .and.
     1            CYLREG(15) .ne. 0) then
                  ifl(8) = 1
                  rv(8) = RCYCDO(10)
                  if (ICYLFL(3) .eq. 2) then
                      rv(8) = dabs(rv(8))
                  else if (ICYLFL(3) .eq. 3) then
                      if (rv(8) .gt. 0.) rv(8) = 0. - rv(8)
                  endif
              endif
          endif
c
c......OFFSET
c
          if (ICYCDO(9) .ne. 0) then
              if (RCYCDO(11) .ne. 0. .and. CYLREG(8) .ne. 0) then
                  ifl(9) = 1
                  rv(9) = RCYCDO(11)
              endif
c
              if (ICYCDO(9) .eq. 2 .and. RCYCDO(12) .ne. 0. .and.
     1            CYLREG(9) .ne. 0) then
                  ifl(10) = 1
                  rv(10) = RCYCDO(12)
              endif
          endif
c
c......RTRCTO
c
          if (ICYCDO(10) .ne. 0) then
              if (RCYCDO(13) .ne. 0. .and. CYLREG(12) .ne. 0) then
                  ifl(11) = 1
                  rv(11) = RCYCDO(13)
              endif
c
              if (ICYCDO(10) .eq. 2 .and. RCYCDO(14) .ne. 0. .and.
     1            CYLREG(13) .ne. 0) then
                  ifl(12) = 1
                  rv(12) = RCYCDO(14)
              endif
          endif
c
c......TOOL
c
          if (ICYCDO(11) .ne. 0) then
              if (RCYCDO(15) .ne. 0. .and. CYLREG(16) .ne. 0) then
                  ifl(13) = 1
                  rv(13) = RCYCDO(15)
              endif
c
              if (ICYCDO(11) .eq. 2 .and. RCYCDO(16) .ne. 0. .and.
     1            CYLREG(17) .ne. 0) then
                  ifl(14) = 1
                  rv(14) = RCYCDO(12)
              endif
          endif
c
c......REPEAT
c
          if (ICYCDO(12) .ne. 0) then
              if (RCYCDO(17) .ne. 0 .and. CYLREG(11) .ne. 0) then
                  ifl(15) = 1
                  rv(15) = RCYCDO(17)
              endif
          endif
c
c......CHAMFER
c......ON / OFF
c
          if (ICYCDO(13) .ne. 0) then
              ifl(16) = 1
              CYLREG(20) = CYLCOD(23+ICYCDO(13))
              rv(16) = CYLCDV(23+ICYCDO(13))
          endif
c
c......TPI
c
          if (ICYCDO(6) .ne. 0) then
              if (RCYCDO(6) .ne. 0. .and. CYLREG(1) .ne. 0) then
                  ifl(17) = 1
                  rv(17) = RCYCDO(6)
              endif
c
              if (ICYCDO(6) .eq. 2 .and. RCYCDO(7) .ne. 0. .and.
     1            CYLREG(2) .ne. 0) then
                  ifl(18) = 1
                  rv(18) = RCYCDO(7)
              endif
          endif
      endif
c
c...Output cycle block
c
      call cylblk (ifl,rv,gfin)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cylblk (kfl,gval,gfin)
c
c   FUNCTION:  This routine outputs a lathe canned cycle block.
c
c   INPUT:  kfl     I*4  D19  -  Contains an array of codes that need
c                                to be output in this block.  (1) =
c                                Which cycle, (2) = Cycle definition
c                                code, (3) = Cycle position, (4) =
c                                FEDTO, (5) = Feed rate, (6) = RAPTO,
c                                (7) = STEP1, (8) = STEP2, (9) =
c                                OFFSET1, (10) = OFFSET2, (11) =
c                                RTRCTO1, (12) = RTRCTO2, (13) = TOOL1,
c                                (14) = TOOL2, (15) = REPEAT, (16) =
c                                CHAMFER, (17) = LEADK, (18) = LEADI,
c                                (19) = Final depth.
c
c           gval    R*8  D18  -  Values for 'kfl'.
c
c           gfin    R*8  D3   -  The linear axes position at the end of
c                                the cycle.
c
c   OUTPUT: kfl     I*4  D18  -  'kfl' will reset to 0 all codes that
c                                have been output.
c
c***********************************************************************
c
      subroutine cylblk (kfl,gval,gfin)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (REGFRC,KPOSMP(0603))
      equivalence (ICYLFL,KPOSMP(1901)), (CYLREG,KPOSMP(1946))
      equivalence (USRBRG,KPOSMP(2600)), (MUSRBK,KPOSMP(3022))
      equivalence (NCUSRB,KPOSMP(3025)), (ICYLBK,KPOSMP(3080))
      equivalence (IFOTYP,KPOSMP(3151))
c
      integer*4 USRBRG(20,20),MUSRBK,NCUSRB(20),ICYLFL(20),CYLREG(25),
     1          ICYCSW(5),REGFRC(MAXFMT),ICYLBK(20),IFOTYP
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
      equivalence (USRBVL,POSMAP(2501)), (RCYCDO,POSMAP(2931))
c
      real*8 DUMMY,REGSTO(MAXFMT),USRBVL(20,20),RCYCDO(20)
c
      integer*4 kfl(20)
c
      real*8 gval(20),gfin(3)
c
      integer*4 ist,iblk,ilast,ien,i,ncd,icd(20),idid,inc,ipos,ifl,
     1          ireg,ifed,inum,isub(13),jreg,idep,iout,j, iax(10)
c
      real*8 rvl(20)
c
      data isub /7,0,10,14,15,8,9,12,13,16,17,11,20/
      data iax /0,0, 0,0, 0,0, 0,0, 0,0/
c
c...Initialize routine
c
      ist    = 1
      iblk   = ICYLBK(kfl(1)-3)
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
      if (ist .eq. NCUSRB(iblk) .and. USRBRG(ien,iblk) .eq. -3)
     1        go to 1100
c
c...Output the required cycle codes in this block
c
      ifl    = 0
      idep   = 0
      idid   = 0
      ifed   = 0
      iout   = 0
      ipos   = 0
      ncd    = 0
      do 700 i=ist,ien,1
          if (ilast .eq. 0) ireg   = USRBRG(i,iblk)
c
c......Cycle code
c
          if (ireg .eq. kfl(2) .or. (ilast .eq. 1 .and. kfl(2) .ne. 0))
     1            then
              if (ilast .eq. 1) ireg = kfl(2)
              if (ICYCSW(2) .eq. 1 .and. ICYLFL(4) .eq. 1 .and.
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
          if (ireg .eq. -14 .or. (ilast .eq. 1 .and. kfl(5) .ne. 0))
     1            then
              idid   = 1
              if (kfl(5) .eq. 1) then
                  ifed   = 1
                  ifl    = 1
                  kfl(5) = 0
              endif
              if (ilast .eq. 0) go to 700
          endif
c
c......Position code
c
          if (ireg .eq. -8 .or. (ilast .eq. 1 .and. kfl(3) .ne. 0))
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
          if (ireg .eq. -4 .or. (ilast .eq. 1 .and. kfl(19) .ne. 0))
     1                 then
              idid   = 1
              if (kfl(19) .eq. 1) then
                  idep   = 1
                  ifl    = 1
                  kfl(19) = 0
              endif
              if (ilast .eq. 0) go to 700
          endif
c
c......Cycle parameter codes
c
          do 500 j=4,16,1
              if (j .eq. 5) go to 500
              inc    = isub(j-3)
              jreg   = CYLREG(inc)
              call regtyp (jreg,gval(j))
              if (ilast .eq. 1) ireg = jreg
              if (ireg .ne. jreg) go to 500
              idid   = 1
              if (kfl(j) .eq. 1) then
                  if (REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
                  call codout (ireg,gval(j))
                  ifl    = 1
                  kfl(j) = 0
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
      if (ifed .eq. 1) then
c
c.........TPI
c
          if (IFOTYP .eq. 5) then
              if (kfl(17) .eq. 1) then
                  ireg   = CYLREG(1)
                  if ((ICYCSW(2) .eq. 1 .or. ICYLFL(6) .eq. 1) .and.
     1                REGFRC(ireg) .eq. 0)
     2                    REGFRC(ireg) = 1
                  call codout (ireg,RCYCDO(6))
              endif
c
              if (kfl(18) .eq. 1) then
                  ireg   = CYLREG(2)
                  if ((ICYCSW(2) .eq. 1 .or. ICYLFL(6) .eq. 1) .and.
     1                REGFRC(ireg) .eq. 0)
     2                    REGFRC(ireg) = 1
                  call codout (ireg,RCYCDO(7))
              endif
c
c.........IPM
c
          else
              ireg   = -14
              call regtyp (ireg,RCYCDO(2))
              if (ICYCSW(2) .eq. 1 .and. REGFRC(ireg) .eq. 0)
     1            REGFRC(ireg) = 1
              call frnout(iax)
          endif
      endif
c
c......Output cycle position
c
      if (ipos .eq. 1 .or. idep .eq. 1)
     1        call cylpos (gfin,ipos,idep,iout)
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
c   SUBROUTINE:  cylpos (gfin,kpos,kdep,kfl)
c
c   FUNCTION:  This routine outputs the final position within a lathe
c              canned cycle block.
c
c   INPUT:  gfin    R*8  D3   -  The linear axes position at the end of
c                                the cycle.
c
c           kpos    I*4  D1   -  1 = Output final length (Z-axis).
c
c           kdep    I*4  D1   -  1 = Output final depth (X-axis).
c
c           kfl     I*4  D1   -  Should be set to 0 on first call and
c                                not modified thereafter (used to output
c                                motion preparatory codes).
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cylpos (gfin,kpos,kdep,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (MOTREG,KPOSMP(0381))
      equivalence (REGFRC,KPOSMP(0603)), (MOTFLG,KPOSMP(1073))
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (ICYLFL,KPOSMP(1901)), (CYLREG,KPOSMP(1946))
c
      integer*4 ICYLFL(20),ICYCSW(5),REGFRC(MAXFMT),CYLREG(25),
     1          MOTREG(24),INCR,MOTFLG
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (CYCPSV,POSMAP(2951))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       STONUM(3,4),CYCPSV(10)
c
      integer*4 kdep,kpos,kfl
c
      real*8 gfin(3)
c
      integer*4 inum,ireg, numw, iax(10)
      data iax /0,0,0,0,0,0,0,0,0,0/
c
      real*8 rpos,rval
      numw = 0
c
c...Set up current position
c
      if (kdep .eq. 1) then
          MCHNUM(1,4) = gfin(1)
      else
          MCHNUM(1,4) = STONUM(1,4)
      endif
      MCHNUM(2,4) = gfin(2)
      if (kpos .eq. 1) then
          MCHNUM(3,4) = gfin(3)
      else
          MCHNUM(3,4) = STONUM(3,4)
      endif
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
c
c...Output final length
c
      if (kpos .eq. 1) then
          ireg   = CYLREG(5)
          rval   = AXSOUT(5)
          rpos   = rval
          if (ireg .eq. 0) ireg = MOTREG(9)
          call codint (ireg,rpos,rpos,inum)
          if (INCR .eq. 2) then
              if (ireg .eq. MOTREG(9)) ireg = MOTREG(10)
              call increg (MOTREG(9),rpos,rval)
          endif
          if (ICYCSW(2) .eq. 1 .and. ICYLFL(5) .eq. 1 .and.
     1        REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
          if (rpos .ne. CYCPSV(1) .or. REGFRC(ireg) .gt. 0) then
              MOTFLG = 1
c...we have to pass exactly same parameters as function delared
c...Yurong changed 10/15/97
c
c              if (kfl .eq. 0) call motgcd
              if (kfl .eq. 0) call motgcd(iax, numw)
              call codout (ireg,rval)
              kfl   = 1
          endif
          CYCPSV(1) = rpos
      endif
c
c...Output final depth
c
      if (kdep .eq. 1) then
          ireg   = CYLREG(6)
          rval   = AXSOUT(1)
          rpos   = rval
          if (ireg .eq. 0) ireg = MOTREG(1)
          call codint (ireg,rpos,rpos,inum)
          if (INCR .eq. 2) then
              if (ireg .eq. MOTREG(1)) ireg = MOTREG(2)
              call increg (MOTREG(1),rpos,rval)
          endif
          if (ICYCSW(2) .eq. 1 .and. ICYLFL(5) .eq. 1 .and.
     1        REGFRC(ireg) .eq. 0) REGFRC(ireg) = 1
          if (rpos .ne. CYCPSV(2) .or. REGFRC(ireg) .gt. 0) then
              MOTFLG = 1
c...we have to pass exactly same parameters as function delared
c...Yurong changed 10/15/97
c
c             if (kfl .eq. 0) call motgcd
              if (kfl .eq. 0) call motgcd(iax, numw)
              call codout (ireg,rval)
              kfl   = 1
          endif
          CYCPSV(2) = rpos
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cyltim
c
c   FUNCTION:  This routine estimates the maching time and distances for
c              lathe canned cycles.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cyltim
c
      include 'post.inc'
c
      equivalence (ICYCDO,KPOSMP(0276)), (LTHDIA,KPOSMP(1228))
      equivalence (ICYLTM,KPOSMP(1971))
c
      integer*4 ICYCDO(15),ICYLTM(20),LTHDIA(2)
c
      equivalence (CUTTER,POSMAP(0744)), (MCHNUM,POSMAP(1287))
      equivalence (AXSOUT,POSMAP(1340)), (STONUM,POSMAP(1387))
      equivalence (RCYCDO,POSMAP(2931)), (RPM   ,POSMAP(3307))
      equivalence (MOVTIM,POSMAP(3546)), (FEED  ,POSMAP(3547))
      equivalence (FMVTIM,POSMAP(3553)), (RAPLMT,POSMAP(3567))
c
      real*8 MCHNUM(3,4),STONUM(3,4),RCYCDO(20),FEED(4),RAPLMT(10),
     1       MOVTIM,CUTTER(7),AXSOUT(10),RPM,FMVTIM
c
      integer*4 inc,npck,inum,iary(10),irep,i
c
      real*8 rtim,rrap,rplg,rdep,rret,rshf,rfed,rpfed,rnum,rmch(2),
     1       rsto(2),rdep1,rnum1,rpck,rpck1,qaxs(10),qsto(10),qtim,mtim
c
c...Initialize routine
c
      do 100 i=1,10,1
          qaxs(i) = 0.
          qsto(i) = 0.
  100 continue
c
c...Calculate current RPM/SFM
c
      call sfmctl (AXSOUT,CUTTER(1),LTHDIA(2))
c
c...Calculate machining distances
c
      call mchdis (1)
c
c...Check machine limits
c
      call lmtchk (AXSOUT,inum,iary,1)
c
c...Calculate machining times
c
      inc    = ICYLTM(ICYCDO(1))
      rtim   = 0.
      mtim   = 0.
      rrap   = RCYCDO(5)
      if (rrap .eq. 0. .or. rrap .ge. 10000.) rrap = RAPLMT(1)
c
c......Set up FROM/TO positions
c
      rsto(2) = STONUM(3,4)
      rmch(2) = MCHNUM(3,4)
      if (LTHDIA(2) .eq. 2) then
          rsto(1) = STONUM(1,4) / 2.
          rmch(1) = MCHNUM(1,4) / 2.
      else
          rsto(1) = STONUM(1,4)
          rmch(1) = MCHNUM(1,4)
      endif
c
c......DEEP
c......DRILL
c......THRU
c
      if (inc .eq. 1 .or. inc .eq. 2 .or. inc .eq. 6) then
c
c.........Calculate depths
c
          rdep   = dsqrt((rmch(1)-rsto(1))**2 + (rmch(2)-rsto(2))**2)
          rplg   = 0.
          if (ICYCDO(7) .ne. 0) then
              rplg   = RCYCDO(8)
              rdep   = rdep   - rplg
          endif
c
c.........Calculate number of pecks
c
          if (ICYCDO(8) .eq. 0 .or. RCYCDO(9) .eq. 0.) then
              npck   = 1
          else
              rshf   = 0.
              if (ICYCDO(8) .eq. 1) then
                  rshf   = RCYCDO(9)
              else if (ICYCDO(8) .eq. 2) then
                  rshf = (RCYCDO(9) + RCYCDO(10)) / 2.
              endif
              rshf   = (rdep / rshf) + 1.
              npck   = rshf
              if (npck .lt. 1) npck = 1
          endif
c
c.........Calculate machining times
c
          qtim   = (rplg / RAPLMT(1))
          mtim   = mtim   + qtim
          qaxs(3) = rplg
          call acltim (qsto,qaxs,qtim,0)
          rtim   = rtim   + qtim
          qtim   = (rdep / FEED(1))
          mtim   = mtim   + qtim
          qaxs(3) = rdep
          call acltim (qsto,qaxs,qtim,0)
          rtim   = rtim   + qtim
          qtim   = (rdep / rrap)
          mtim   = mtim   + qtim
          call acltim (qsto,qaxs,qtim,0)
          rtim   = rtim   + qtim
          qaxs(3) = 0.
          if (ICYCDO(10) .ne. 0) then
              qtim = ((RCYCDO(13) / rrap) * npck)
              mtim   = mtim   + qtim
              qaxs(3) = RCYCDO(13) * npck
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
              qtim = ((RCYCDO(13) / FEED(1)) * npck)
              mtim   = mtim   + qtim
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
              qaxs(3) = 0.
          endif
c
c......FACE
c......ROUGH
c......THREAD
c......TURN
c
      else if (inc .eq. 3 .or. inc .eq. 4 .or. inc .eq. 5 .or.
     1         inc .eq. 7) then
c
c.........Calculate depths
c
          if (inc .eq. 3) then
              rdep   = dabs(rmch(2)-rsto(2))
              rret   = dabs(rmch(1)-rsto(1))
          else if (inc .eq. 5) then
              rdep   = dabs(RCYCDO(11))
              rret   = dabs(rmch(1)-rsto(1))
          else
              rdep   = dabs(rmch(1)-rsto(1))
              rret   = dabs(rmch(2)-rsto(2))
          endif
          rdep1  = rdep
          if (ICYCDO(4) .ne. 0) then
              rdep   = rdep   + RCYCDO(3)
              rret   = dsqrt(rret**2 + RCYCDO(3)**2)
          endif
          rplg   = 0.
          if (ICYCDO(7) .ne. 0 .and. inc .ne. 5) then
              rplg   = RCYCDO(8)
              rdep   = rdep   - rplg
          endif
c
c.........Calculate number of pecks
c
          rnum   = rdep
          rnum1  = rdep1
          if (ICYCDO(8) .eq. 0 .or. RCYCDO(9) .eq. 0.) then
              npck   = 1
              rpck   = rnum
              rpck1  = rnum1
          else
              if (ICYCDO(8) .eq. 1) then
                  rpck   = dabs(RCYCDO(9))
              else if (ICYCDO(8) .eq. 2) then
                  rpck   = (dabs(RCYCDO(9)) + dabs(RCYCDO(10))) / 2.
              endif
              rpck1  = rpck
              rshf   = ((rnum+rnum1)/2. / rpck) + .5
              npck   = rshf
              if (npck .lt. 1) npck = 1
          endif
c
c.........Calculate feedrates
c
          rpfed  = FEED(1)
          rfed   = FEED(1)
          if (inc .eq. 5) then
              rpfed  = rrap
              rfed   = RCYCDO(6)
              if (ICYCDO(6) .eq. 2)
     1            rfed = dsqrt(RCYCDO(6)**2 + RCYCDO(7)**2)
              rfed   = rfed   * RPM
              if (rfed .le. 0.) rfed = FEED(1)
          endif
c
c.........Calculate machining times
c
          irep   = 1
          if (ICYCDO(12) .ne. 0) irep = RCYCDO(17)
          irep   = irep   - 1
          do 200 i=1,npck,1
              qtim   = (rplg / RAPLMT(1))
              mtim   = mtim   + qtim
              qaxs(1) = rplg
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
c
              qtim   = (rpck*i / rpfed)
              mtim   = mtim   + qtim
              qaxs(1) = 0.
              qaxs(3) = rpck  * i
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
c
              qtim   = (rret / rfed)
              mtim   = mtim   + qtim
              qaxs(1) = rret
              qaxs(3) = 0.
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
c
              qtim   = (rpck1*i / rrap)
              mtim   = mtim   + qtim
              qaxs(1) = 0.
              qaxs(3) = rpck  * i
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
c
              qtim   = (rret / RAPLMT(1))
              mtim   = mtim   + qtim
              qaxs(1) = rret
              qaxs(3) = 0
              call acltim (qsto,qaxs,qtim,0)
              rtim   = rtim   + qtim
              qaxs(1) = 0.
  200     continue
c
          if (irep .gt. 0) then
              do 250 i=1,irep,1
                  qtim   = (rplg / RAPLMT(1))
                  mtim   = mtim   + qtim
                  qaxs(1) = rplg
                  call acltim (qsto,qaxs,qtim,0)
                  rtim   = rtim   + qtim
c
                  qtim   = (rdep / rfed)
                  mtim   = mtim   + qtim
                  qaxs(1) = 0.
                  qaxs(3) = rdep
                  call acltim (qsto,qaxs,qtim,0)
                  rtim   = rtim   + qtim
c
                  qtim   = (rret / rfed)
                  mtim   = mtim   + qtim
                  qaxs(1) = rret
                  qaxs(3) = 0.
                  call acltim (qsto,qaxs,qtim,0)
                  rtim   = rtim   + qtim
c
                  qtim   = (rdep1 / rpfed)
                  mtim   = mtim   + qtim
                  qaxs(1) = 0.
                  qaxs(3) = rdep1
                  call acltim (qsto,qaxs,qtim,0)
                  rtim   = rtim   + qtim
c
                  qtim   = (rret / RAPLMT(1))
                  mtim   = mtim   + qtim
                  qaxs(1) = rret
                  qaxs(3) = 0.
                  call acltim (qsto,qaxs,qtim,0)
                  rtim   = rtim   + qtim
                  qaxs(1) = 0.
  250         continue
          endif
      endif
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
