c
c***********************************************************************
c
c   FILE NAME:  strgcf.for
c   CONTAINS:
c               strgcf  sgfcon  sgfreg  sgfblk  sgfpos  sgfhom  sgflmt
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        strgcf.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  strgcf (kfl,cmsg,kerr)
c
c   FUNCTION:  Stringer Configuration menu handling routine.
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
      subroutine strgcf (kfl,cmsg,kerr)
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
      kerr   = 0
      ifl    = kfl
      iwlk   = IWALK
      if (kfl .ge. NLEVL) go to 200
      if (IWALK .eq. 1) then
          NLEVL  = NLEVL  + 1
          go to 1000
      endif
c
c...Display Stringer Configuration menu
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
      go to (1000,1100,1200,1300,1400,1500,1600), igo
c
c...Walk through Stringer Configuration
c
 1000 IWALK  = 1
c
c...Setup Stringer machine
c
 1100 MLEVL(NLEVL-1) = 1
      call sgfcon (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Axes register description
c
 1200 MLEVL(NLEVL-1) = 2
      call sgfreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...User defined blocks
c
 1300 MLEVL(NLEVL-1) = 3
      call sgfblk (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Current axes position
c
 1400 MLEVL(NLEVL-1) = 4
      call sgfpos (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Home position & Clearance plane
c
 1500 MLEVL(NLEVL-1) = 5
      call sgfhom (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Machine Limits
c
 1600 MLEVL(NLEVL-1) = 6
      call sgflmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
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
c   SUBROUTINE:  sgfcon (kfl)
c
c   FUNCTION:  Stringer Machine configuration prompt handling routine.
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
      subroutine sgfcon (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SGMACT,KPOSMP(0996))
c
      integer*4 SGMACT
c
      equivalence (SGMOFS,POSMAP(5099))
c
      real*8 SGMOFS(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,i,ist,inc
c
      real*8 rnum,rmn,rmx
c
      data rmn /-99999999./, rmx /99999999./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210), ist
c
c...Get active head at beginning of program
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = SGMACT
      nc     = 1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SGMACT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get A-axis offset for Head 2
c
  200 ist    = 2
  210 do 290 i=ist,3,1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inc    = inc    + 1
          rnum   = SGMOFS(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMOFS(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
c   SUBROUTINE:  sgfreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Axes registers prompt handling routine.
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
      subroutine sgfreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SGMREG,KPOSMP(0951))
c
      integer*4 SGMREG(6,3)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 iwrn,inum,ilod,ist,i,nc
c
      real*8 rnum
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210), ist
c
c...Get Head to define axes for
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      ISGLST = ISGINC
      inum   = ISGINC + 1
      nc     = -1
      call prmint (inum,nc,1,2,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISGINC = inum   - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get registers for axes
c
  200 ist    = 2
c
  210 do 290 i=ist,7,1
c
c......Verify that this machine has this axis
c
          if (ISGLST .ne. 1 .and. i .gt. 4) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......Get user's input
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SGMREG(i-1,ISGLST)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMREG(i-1,ISGLST) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
c   SUBROUTINE:  sgfblk (kfl,cmsg,kerr)
c
c   FUNCTION:  User defined stringer blocks and codes prompt handling
c              routine.
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
      subroutine sgfblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SGMBLK,KPOSMP(1008)), (SGMCOD,KPOSMP(1018))
      equivalence (MUSRBK,KPOSMP(3022))
c
      integer*4 SGMBLK(10),MUSRBK,SGMCOD(10)
c
      equivalence (SGMCDV,POSMAP(5552))
c
      real*8 SGMCDV(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,inum,i,inc
c
      real*8 rnum
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,610,610,610,610,610,610,610),
     1    MLEVL(NLEVL)
c
c...User defined stringer blocks
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,5,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SGMBLK(inc)
          nc     = 1
          call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMBLK(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Miscellaneous stringer codes
c
  600 ist    = 6
  610 inc    = ist    - 6
      do 690 i=ist,12,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SGMCOD(inc)
          rnum   = SGMCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMCOD(inc) = inum
              SGMCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
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
c   SUBROUTINE:  sgfpos (kfl,cmsg,kerr)
c
c   FUNCTION:  Current Axes position prompt handling routine.
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
      subroutine sgfpos (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (SGMSTO,POSMAP(5101))
c
      real*8 SGMSTO(6,3)
c
      integer*4 nc,iwrn,ilod,i,ist,inum
c
      real*8 rnum,rmn,rmx
c
      data rmn /-99999999/, rmx /99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210), ist
c
c...Get Head to define axes for
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      ISGLST = ISGINC
      inum   = ISGINC + 1
      nc     = -1
      call prmint (inum,nc,1,2,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISGINC = inum   - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Axes positions
c
  200 ist     = 2
  210 do 290 i=ist,7,1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (ISGLST .ne. 1 .and. i .gt. 4) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          rnum   = SGMSTO(i-1,ISGLST)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMSTO(i-1,ISGLST) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
c   SUBROUTINE:  sgfhom (kfl,cmsg,kerr)
c
c   FUNCTION:  Home position prompt handling routine.
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
      subroutine sgfhom (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (SGMHOM,POSMAP(5119))
c
      real*8 SGMHOM(6,3)
c
      integer*4 nc,iwrn,ilod,i,ist,inum
c
      real*8 rnum,rmn,rmx
c
      data rmn /-99999999/, rmx /99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210), ist
c
c...Get Head to define axes for
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      ISGLST = ISGINC
      inum   = ISGINC + 1
      nc     = -1
      call prmint (inum,nc,1,2,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISGINC = inum   - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Axes positions
c
  200 ist     = 2
  210 do 290 i=ist,7,1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (ISGLST .ne. 1 .and. i .gt. 4) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          rnum   = SGMHOM(i-1,ISGLST)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMHOM(i-1,ISGLST) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
c   SUBROUTINE:  sgflmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Machine Limits prompt handling routine.
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
      subroutine sgflmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (SGMLIM,POSMAP(5137))
c
      real*8 SGMLIM(2,6,3)
c
      integer*4 nc,iwrn,ilod,i,ist,inum
c
      real*8 rnum(2),rmn(2),rmx(2)
c
      data rmn /-99999999,-99999999/, rmx /99999999,99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210), ist
c
c...Get Head to define axes for
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      ISGLST = ISGINC
      inum   = ISGINC + 1
      nc     = -1
      call prmint (inum,nc,1,2,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISGINC = inum   - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Axes limits
c
  200 ist     = 2
  210 do 290 i=ist,7,1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (ISGLST .ne. 1 .and. i .gt. 4) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          rnum(1) = SGMLIM(1,i-1,ISGLST)
          rnum(2) = SGMLIM(2,i-1,ISGLST)
          nc     = 2
          call prmrel (rnum,nc,2,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SGMLIM(1,i-1,ISGLST) = rnum(1)
              SGMLIM(2,i-1,ISGLST) = rnum(2)
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
