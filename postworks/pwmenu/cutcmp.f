c
c***********************************************************************
c
c   FILE NAME:  cutcmp.for
c   CONTAINS:
c               cutcmp  cutsup  ctmctl  cutcod  cutfix
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cutcmp.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 09:19:05
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cutcmp (kfl,cmsg,kerr)
c
c   FUNCTION:  Cutcom/Fixture offsets menu handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.  If kfl is greater than or equal
c                               to the current level, then the user re-
c                               quested direct access to a menu/prompt.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine cutcmp (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ifl,igo,iwlk
c
c...Specific level was requested
c...Go directly to that level
c
      ifl    = kfl
      iwlk   = IWALK
      if (kfl .ge. NLEVL) go to 200
      if (IWALK .eq. 1) then
          NLEVL  = NLEVL  + 1
          go to 1000
      endif
c
c...Display Cutter/Fixture compensation menu
c
      SMLEVL(NLEVL) = -1
      PRESEL(NLEVL) = -1
  100 MLEVL(NLEVL) = 0
      call menu (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (MOTIF .eq. 1) go to 8000
c
c...Go to appropriate section
c
  200 igo    = MLEVL(NLEVL) + 1
      NLEVL  = NLEVL  + 1
      if (kfl .lt. NLEVL) MLEVL(NLEVL) = 0
      go to (1000,1100,1200,1300,1400,8000), igo
c
c...Walk through Cutcom/Fixture offsets
c
 1000 IWALK  = 1
c
c...Cutcom support
c
 1100 MLEVL(NLEVL-1) = 1
      call cutsup (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cutcom control
c
 1200 MLEVL(NLEVL-1) = 2
      call ctmctl (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cutcom codes
c
 1300 MLEVL(NLEVL-1) = 3
      call cutcod (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Fixture offsets
c
 1400 MLEVL(NLEVL-1) = 4
      call cutfix (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...End of sub-section
c...Reset Level structure
c
 7000 NLEVL  = NLEVL  - 1
      if (ifl .ge. NLEVL .or. iwlk .eq. 1) go to 8000
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutsup (kfl,cmsg,kerr)
c
c   FUNCTION:  Cutter compensation support prompt handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine cutsup (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (CUTCFL,KPOSMP(3251))
c
      integer*4 MACHTP,CUTCFL(20)
c
      equivalence (CUTCVR,POSMAP(2402))
c
      real*8 CUTCVR(8)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inum,inc,is(5)
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
c
      data is /14,2,17,16,3/
c
      data rmn /0./, rmx /99999999./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,210,210,210,210,210,610,610), MLEVL(NLEVL)
c
c...Cutter compensation output format
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = CUTCFL(1)
      nc     = -1
      call prmint (inum,nc,1,0,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CUTCFL(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Cutter compensation features
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          if (MACHTP .eq. 2 .and. (is(inc) .eq. 2 .or.
     1        is(inc) .eq. 17)) iwrn = 1
          if (is(inc) .eq. 16 .and. CUTCFL(1) .ne. 1)
     1        iwrn = 1
          if (is(inc) .eq. 14 .and. (CUTCFL(1) .lt. 1 .or.
     1        CUTCFL(1) .gt. 3)) iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          iwrn   = 0
          inum   = CUTCFL(is(inc))
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCFL(is(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
  295 continue
c
c...Cutter compensation features
c
  600 ist    = 7
  610 inc    = ist    - 7
      do 690 i=ist,8,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          if (inc .eq. 2 .and. (CUTCFL(1) .eq. 0 .or. CUTCFL(1) .eq. 4))
     1            then
              if (kfl .lt. NLEVL) go to 695
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          iwrn   = 0
          rnum   = CUTCVR(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCVR(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
  695 continue
c
c...Up arrow
c...Go to previous prompt
c
      go to 8000
 7000 MLEVL(NLEVL) = PRMSTK(PRMSP)
      PRMSP  = PRMSP  - 1
      go to 50
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ctmctl (kfl,cmsg,kerr)
c
c   FUNCTION:  Cutter compensation control prompt handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine ctmctl (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251))
c
      integer*4 CUTCFL(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inum,inc,isub(7)
c
      data iyesno /13,12/
c
      data isub /4,5,6,7,8,9,10/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110), MLEVL(NLEVL)
c
c...Miscellaneous CUTCOM flags
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,7,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn = 0
          if (i .eq. 3 .and. CUTCFL(1) .ne. 3) iwrn = 1
          if (i .ge. 4 .and. i .le. 5 .and. CUTCFL(1) .ne.  2 .and.
     1        CUTCFL(1) .ne. 3) iwrn = 1
          if (i .eq. 5 .and. CUTCFL(7) .ne. 1) iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          iwrn   = 0
          inum   = CUTCFL(isub(inc))
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCFL(isub(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Up arrow
c...Go to previous prompt
c
      go to 8000
 7000 MLEVL(NLEVL) = PRMSTK(PRMSP)
      PRMSP  = PRMSP  - 1
      go to 50
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutcod (kfl,cmsg,kerr)
c
c   FUNCTION:  Tool change codes prompt handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine cutcod (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (TLDIG ,KPOSMP(1829))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
c
      integer*4 CUTCFL(20),CUTCCD(30),MACHTP,TLDIG(2,5)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (CUTCVR,POSMAP(2402)), (CUTCVL,POSMAP(2410))
c
      real*8 CUTCVR(8),CUTCVL(30),DUMMY
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum,iary(2),ifl
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
c
      data rmn /-99999999./, rmx /99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,500,610,610,610,610,610,610,610,610,610,
     1       610,610,1710,1710,1710,1710,1710), MLEVL(NLEVL)
c
c...Misc tool change codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,4,1
          inc    = inc    + 1
          iwrn   = 0
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCCD(inc)
          rnum   = CUTCVL(inc)
          nc     = 1
  120     call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Get digits within
c......register for tool codes
c
              if (inc .eq. 4) then
                  call tolcod (rnum,iary,iwrn)
                  if (iwrn .eq. 1) then
                      call errmsg ('INVRSP',1,2)
                      if (IMSCAN .eq. 2) go to 8000
                      go to 120
                  endif
                  TLDIG(1,4) = iary(1)
                  TLDIG(2,4) = iary(2)
              endif
              CUTCCD(inc) = inum
              CUTCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Base number for cutcom offsets
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      rnum   = CUTCVR(3)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CUTCVR(3) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...More cutcom codes
c
  600 ist    = 6
  610 inc    = ist    - 2
      do 690 i=ist,16,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .eq. 5 .and. CUTCFL(1) .ne. 3) iwrn = 1
          if (inc .ge. 6 .and. inc .le. 8  .and. CUTCFL(1) .ne. 1 .and.
     1        CUTCFL(1) .ne. 2 .and. CUTCFL(1) .ne. 3 .and.
     2        CUTCFL(17) .ne. 1) iwrn = 1
          if (inc .ge. 9 .and. inc .le. 11 .and. CUTCFL(1) .ne. 4)
     1            iwrn = 1
          if (inc .ge. 12 .and. inc .le. 15 .and. CUTCFL(3) .ne. 1)
     1            iwrn = 1
          if (inc .eq. 7 .and. MACHTP .eq. 2) iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCCD(inc)
          rnum   = CUTCVL(inc)
          ifl    = 1
          if ((inc .ge. 6 .and. inc .le. 9) .or.inc .eq. 15) then
              rnum   = DUMMY
              ifl    = 2
          endif
          nc     = 1
          call prmcod (inum,rnum,nc,1,ifl,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCCD(inc) = inum
              CUTCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
c
c...More cutcom codes
c
 1700 ist    = 17
 1710 inc    = ist    + 6
      if (CUTCFL(17) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1790 i=ist,21,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCCD(inc)
          if (inc .eq. 24) then
              rnum = CUTCVL(inc)
              ifl    = 1
          else
              rnum   = DUMMY
              ifl    = 2
          endif
          nc     = 1
          call prmcod (inum,rnum,nc,1,ifl,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCCD(inc) = inum
              CUTCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1790 continue
 1795 continue
c
c...Up arrow
c...Go to previous prompt
c
      go to 8000
 7000 MLEVL(NLEVL) = PRMSTK(PRMSP)
      PRMSP  = PRMSP  - 1
      go to 50
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutfix (kfl,cmsg,kerr)
c
c   FUNCTION:  Fixture offsets prompt handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine cutfix (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (MUSRBK,KPOSMP(3022))
      equivalence (CUTCFL,KPOSMP(3251)), (CUTCCD,KPOSMP(3271))
c
      integer*4 MACHTP,MUSRBK,CUTCFL(20),CUTCCD(30)
c
      equivalence (DUMMY ,POSMAP(0003)), (CUTCVR,POSMAP(2402))
      equivalence (CUTCVL,POSMAP(2410))
c
      real*8 DUMMY,CUTCVL(30),CUTCVR(8)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
      data rmn /-99999999./, rmx /99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,310,310,310,310,700,810,810,810,1110,1110),
     1           MLEVL(NLEVL)
c
c...Fixture offset register
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = CUTCCD(16)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CUTCCD(16) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Base number for fixture offsets
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      rnum   = CUTCVR(4)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CUTCVR(4) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Fixture offset codes
c
  300 ist    = 3
  310 inc    = ist    + 13
      do 390 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCCD(inc)
          rnum   = CUTCVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCCD(inc) = inum
              CUTCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  390 continue
c
c...Output MINUS registers as negatives
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = CUTCFL(11)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CUTCFL(11) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Fixture offset axis codes
c
  800 ist    = 8
  810 inc    = ist    + 12
      do 890 i=ist,10,1
          inc    = inc    + 1
          if (inc .eq. 22 .and. MACHTP .eq. 2) then
              if (kfl .lt. NLEVL) go to 890
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCCD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  890 continue
c
c...Output fixture offset codes in block by themselves
c
 1100 ist    = 11
 1110 inc    = ist    + 0
      do 1190 i=ist,12,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CUTCFL(inc)
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CUTCFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
c
c...Up arrow
c...Go to previous prompt
c
      go to 8000
 7000 MLEVL(NLEVL) = PRMSTK(PRMSP)
      PRMSP  = PRMSP  - 1
      go to 50
c
c...End of routine
c
 8000 return
      end
