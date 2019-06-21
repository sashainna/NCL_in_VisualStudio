c
c***********************************************************************
c
c   FILE NAME:  tlchg.for
c   CONTAINS:
c               tlchg   tlcsup  tlclod  tlccod  tlcoff  tolcod
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tlchg.f , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:04:16
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  tlchg (kfl,cmsg,kerr)
c
c   FUNCTION:  Tool change menu handling routine.
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
      subroutine tlchg (kfl,cmsg,kerr)
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
c...Display Tool change menu
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
c...Walk through Tool change
c
 1000 IWALK  = 1
c
c...Tool change support
c
 1100 MLEVL(NLEVL-1) = 1
      call tlcsup (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Loading the tool
c
 1200 MLEVL(NLEVL-1) = 2
      call tlclod (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Tool change codes
c
 1300 MLEVL(NLEVL-1) = 3
      call tlccod (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Tool length offsets
c
 1400 MLEVL(NLEVL-1) = 4
      call tlcoff (kfl,cmsg,kerr)
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
c   SUBROUTINE:  tlcsup (kfl,cmsg,kerr)
c
c   FUNCTION:  Tool change support prompt handling routine.
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
      subroutine tlcsup (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (TOOLFL,KPOSMP(1804))
      equivalence (TURFL ,KPOSMP(1824))
c
      integer*4 MACHTP,TOOLFL(20),TURFL(2)
c
      equivalence (TURDIS,POSMAP(3961)), (MAXTN ,POSMAP(3995))
c
      real*8 TURDIS,MAXTN
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inum,inc,itr(2)
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/, itr /45,46/
c
      data rmn /-99999999./, rmx /99999999./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,210,210,210,500,600,700,800), MLEVL(NLEVL)
c
c...Maximum tool number allowed
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      rnum   = MAXTN
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MAXTN  = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...SELCTL, Grippers & Spindles Support
c
  200 ist    = 2
  210 inc    = ist    - 2
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 290 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TOOLFL(inc)
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TOOLFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
  295 continue
c
c...Support LOAD and SELECT commands
c
  500 MLEVL(NLEVL) = 5
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = TOOLFL(18)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TOOLFL(18) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Number of turrets
c
  600 MLEVL(NLEVL) = 6
      if (MACHTP .ne. 2 .and. MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = TURFL(1)
      nc     = -1
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TURFL(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  695 continue
c
c...Distance between turrets
c
  700 MLEVL(NLEVL) = 7
      if (MACHTP .ne. 2 .and. MACHTP .ne. 4 .or.
     -                      TURFL(1) .ne. 2) then
          if (kfl .lt. NLEVL) go to 795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = TURDIS
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TURDIS = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
c
c...Default turret
c
  800 MLEVL(NLEVL) = 8
      if (MACHTP .ne. 2 .and. MACHTP .ne. 4 .or.
     1                      TURFL(1) .ne. 2) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = TURFL(2)
      nc     = 1
      call prmvoc (inum,itr,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TURFL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
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
c   SUBROUTINE:  tlclod (kfl,cmsg,kerr)
c
c   FUNCTION:  Load tool prompt handling routine.
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
      subroutine tlclod (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (TOOLFL,KPOSMP(1804))
c
      integer*4 MACHTP,TOOLFL(20)
c
      equivalence (TLCTIM,POSMAP(3962))
c
      real*8 TLCTIM
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inum,inc,isub(10)
c
      real*8 rnum,rmx
c
      data iyesno /13,12/
c
      data isub /4,5,15,6,7,8,9,10,19,20/
c
      data rmx /99999999./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,110,1100),
     1    MLEVL(NLEVL)
c
c...Miscellaneous LOADTL flags
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,10,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          if (i .le. 3 .and. (MACHTP .eq. 2 .or. TOOLFL(1) .ne. 1)) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          iwrn   = 0
          inum   = TOOLFL(isub(inc))
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TOOLFL(isub(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Amount of time for tool change
c
 1100 MLEVL(NLEVL) = 11
      iwrn   = 0
      rnum   = TLCTIM * 60.
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLCTIM = rnum   / 60.
          if (kfl .ge. NLEVL) go to 8000
      endif
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
c   SUBROUTINE:  tlccod (kfl,cmsg,kerr)
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
      subroutine tlccod (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (TOOLFL,KPOSMP(1804))
      equivalence (TLDIG ,KPOSMP(1829)), (TLCCD ,KPOSMP(1839))
      equivalence (TLCBLK,KPOSMP(1859)), (MUSRBK,KPOSMP(3022))
c
      integer*4 MACHTP,TOOLFL(20),TLDIG(2,5),TLCCD(20),TLCBLK,MUSRBK
c
      equivalence (TLCVL ,POSMAP(3963))
c
      real*8 TLCVL(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum,iary(2)
c
      real*8 rnum
c
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,500,610,610,610,610,610,610,610,610,1400),
     1           MLEVL(NLEVL)
c
c...Misc tool change codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,4,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .le. 3 .and. (MACHTP .eq. 2 .or. TOOLFL(1) .ne. 1))
     1            iwrn = 1
          if (inc .eq. 2 .and. TOOLFL(2) .ne. 1) iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TLCCD(inc)
          rnum   = TLCVL(inc)
          nc     = 1
  120     call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Get digits within
c......register for tool codes
c
              if (inc .eq. 3 .or. inc .eq. 4) then
                  call tolcod (rnum,iary,iwrn)
                  if (iwrn .eq. 1) then
                      call errmsg ('INVRSP',1,2)
                      if (IMSCAN .eq. 2) go to 8000
                      go to 120
                  endif
                  TLDIG(1,inc-2) = iary(1)
                  TLDIG(2,inc-2) = iary(2)
              endif
              TLCCD(inc) = inum
              TLCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Tool code in block by itself
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = TOOLFL(11)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TOOLFL(11) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...More tool change codes
c
  600 ist    = 6
  610 inc    = ist    - 2
      do 690 i=ist,13,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .ge. 7 .and. inc .le. 10 .and. (MACHTP .eq. 2 .or.
     1        MACHTP .eq. 4 .or. TOOLFL(1) .ne. 1)) iwrn = 1
          if (inc .ge. 11 .and. inc .le. 12 .and. MACHTP .ne. 2 .and.
     1        MACHTP .ne. 4) iwrn = 1
          if (inc .ge. 9 .and. inc .le. 10 .and. TOOLFL(2) .ne. 1)
     1            iwrn = 1
          if ((inc .eq. 8 .or. inc .eq. 10) .and. TOOLFL(3) .ne. 1)
     1            iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TLCCD(inc)
          rnum   = TLCVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TLCCD(inc) = inum
              TLCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
c
c...User defined tool change block
c
 1400 MLEVL(NLEVL) = 14
      iwrn   = 0
      inum   = TLCBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLCBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
       endif
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
c   SUBROUTINE:  tlcoff (kfl,cmsg,kerr)
c
c   FUNCTION:  Tool length offsets prompt handling routine.
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
      subroutine tlcoff (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (NOTOOL,KPOSMP(1802))
      equivalence (TOOLFL,KPOSMP(1804)), (TLDIG ,KPOSMP(1829))
      equivalence (TLOCD ,KPOSMP(1860)), (TLOBLK,KPOSMP(1868))
      equivalence (TLOSGN,KPOSMP(1869)), (MUSRBK,KPOSMP(3022))
c
      integer*4 MACHTP,TLDIG(2,5),TLOCD(8),TLOBLK,TLOSGN,MUSRBK,NOTOOL,
     1          TOOLFL(20)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (TLOVL ,POSMAP(3983)), (TLOBAS,POSMAP(3988))
c
      real*8 TLOVL(5),TLOBAS,DUMMY
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),iary(2),inc,inum
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
      go to (100,200,300,410,410,410,410,800,910,910,910,1200,1310,1310,
     1       1500), MLEVL(NLEVL)
c
c...Add tool length to output axes
c
  100 MLEVL(NLEVL) = 1
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = NOTOOL
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NOTOOL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  195 continue
c
c...Tool length offset register
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = TLOCD(1)
      rnum   = TLOVL(1)
      nc     = 1
  220 call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          call tolcod (rnum,iary,iwrn)
          if (iwrn .eq. 1) then
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 220
          endif
          TLDIG(1,3) = iary(1)
          TLDIG(2,3) = iary(2)
          TLOCD(1) = inum
          TLOVL(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Base number for tool length offsets
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      rnum   = TLOBAS
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLOBAS = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Tool length offset codes
c
  400 ist    = 4
  410 inc    = ist    - 3
      do 490 i=ist,7,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TLOCD(inc)
          rnum   = TLOVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TLOCD(inc) = inum
              TLOVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  490 continue
c
c...Output MINUS registers as negatives
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = TLOSGN
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLOSGN = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Tool length axis codes
c
  900 ist    = 9
  910 inc    = ist    - 4
      do 990 i=ist,11,1
          inc    = inc    + 1
          if (inc .eq. 7 .and. MACHTP .eq. 2) then
              if (kfl .lt. NLEVL) go to 990
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TLOCD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TLOCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  990 continue
c
c...Add plane selection with tool length offsets
c
 1200 MLEVL(NLEVL) = 12
      iwrn   = 0
      inum   = TOOLFL(14)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TOOLFL(14) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Output tool offset codes in block by themselves
c
 1300 ist    = 13
 1310 inc    = ist    + 2
      do 1390 i=ist,14,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = TOOLFL(inc)
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TOOLFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1390 continue
c
c...User defined tool length offset block
c
 1500 MLEVL(NLEVL) = 15
      iwrn   = 0
      inum   = TLOBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLOBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
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
c
c***********************************************************************
c
c   SUBROUTINE:  tolcod (gnum,kary,kerr)
c
c   FUNCTION:  Converts tool register value to range of digits inside
c              register for current code.
c
c   INPUT:  gnum    R*8  D1  -  Floating point number containing the
c                               beginning and ending digit for this
c                               code in the tool register; in the for
c                               mat 'b.e'.
c
c   OUTPUT: kary    I*4  D2  -  Contains the beginning and ending digits
c                               for this code.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine tolcod (gnum,kary,kerr)
c
      include 'post.inc'
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kary(2),kerr
c
      real*8 gnum
c
      integer*4 inc,nc,ie
c
      character*20 lnum
c
c...Use entire register
c
      kerr   = 0
      if (gnum .eq. DUMMY) then
          kary(1) = 0
          kary(2) = 0
          go to 8000
      endif
c
c...Get starting digit
c
      call rtoc (gnum,lnum,nc)
      inc    = index(lnum(1:nc),'.')
      if (nc .eq. 0) then
          ie     = nc
      else
          ie     = inc    - 1
      endif
      call ctoi (lnum(1:ie),kary(1),kerr)
      if (kerr .ne. 0) go to 8000
c
c...Get ending digit
c
      if (inc .eq. 0 .or. inc .eq. nc) then
          kary(2) = kary(1)
      else
          call ctoi (lnum(inc+1:nc),kary(2),kerr)
          if (kerr .ne. 0) go to 8000
          if (kary(2) .lt. kary(1)) go to 9000
      endif
c
c...End of routine
c
 8000 return
c
c...Error parsing number
c
 9000 kerr   = 1
      go to 8000
      end
