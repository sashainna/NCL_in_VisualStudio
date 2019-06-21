c
c***********************************************************************
c
c   FILE NAME:  cyclth.for
c   CONTAINS:
c               cyclth  cylcds  cylprm  cylpos  cylout  cylblk  cyltim
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cyclth.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:54
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cyclth (kfl,cmsg,kerr)
c
c   FUNCTION:  Lathe canned cycles menu handling routine.
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
      subroutine cyclth (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (ICYLFL,KPOSMP(1901))
c
      integer*4 ICYLFL(20),MACHTP
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ifl,igo,iwlk
c
c...This menu valid for lathes only
c
      if (MACHTP .ne. 2 .and. MACHTP .ne. 4) then
      if (IWALK .eq. 1) go to 8000
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
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
c...Display menu
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
      go to (1000,1100,1200,1300,1400,1500,1600,8000), igo
c
c...Walk through menu
c
 1000 IWALK  = 1
c
c...Cycle definition codes
c
 1100 MLEVL(NLEVL-1) = 1
      call cylcds (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle parameter codes
c
 1200 MLEVL(NLEVL-1) = 2
      call cylprm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle positioning parameters
c
 1300 MLEVL(NLEVL-1) = 3
      call cylpos (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle block output
c
 1400 MLEVL(NLEVL-1) = 4
      call cylout (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle user defined blocks
c
 1500 if (ICYLFL(1) .ne. 1) then
	  if (IWALK .eq. 1) go to 1600
          if (MOTIF .eq. 1-1) IMENDT(1,NLEVL) = 1
	  call errmsg ('NOTAPPLY',1,2)
	  go to 7000
      endif
      MLEVL(NLEVL-1) = 5
      call cylblk (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle machining times
c
 1600 if (ICYLFL(1) .ne. 1) then
	  if (IWALK .eq. 1) go to 7000
	  call errmsg ('NOTAPPLY',1,2)
	  go to 7000
      endif
      MLEVL(NLEVL-1) = 6
      call cyltim (kfl,cmsg,kerr)
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
c   SUBROUTINE:  cylcds (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle definition codes prompt handling routine.
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
      subroutine cylcds (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYLFL,KPOSMP(1901)), (CYLCOD,KPOSMP(1921))
c
      integer*4 ICYLFL(20),CYLCOD(25)
c
      equivalence (CYLCDV,POSMAP(2968))
c
      real*8 CYLCDV(25)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum
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
      go to (100,210,210,210,210,210,210,210,210,210,210,210,210,210,
     1       210,210,210,210,210), MLEVL(NLEVL)
c
c...Canned cycle support
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ICYLFL(1)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYLFL(1) = inum
          ICYLFL(20) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Cycle codes
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,19,1
	  inc    = inc    + 1
	  if (ICYLFL(1) .ne. 1 .and. inc .ge. 4) then
	      if (kfl .lt. NLEVL) go to 295
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = CYLCOD(inc)
	  rnum   = CYLCDV(inc)
	  nc     = 1
	  call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CYLCOD(inc) = inum
              CYLCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
  295 continue
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
c   SUBROUTINE:  cylprm (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle parameter codes prompt handling routine.
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
      subroutine cylprm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYLFL,KPOSMP(1901)), (CYLCOD,KPOSMP(1921))
      equivalence (CYLREG,KPOSMP(1946))
c
      integer*4 ICYLFL(20),CYLCOD(25),CYLREG(25)
c
      equivalence (DUMMY ,POSMAP(0003)), (CYLCDV,POSMAP(2968))
c
      real*8 DUMMY,CYLCDV(25)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum
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
      go to (110,110,110,110,110,110,110,110,110,110,110,110,110,110,
     1       110,110,110,1810,1810), MLEVL(NLEVL)
c
c...Cycle parameter codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,17,1
	  inc    = inc    + 1
	  if (ICYLFL(1) .ne. 1 .and. inc .ge. 5) then
	      if (kfl .lt. NLEVL) go to 195
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = CYLREG(inc)
	  rnum   = DUMMY
	  nc     = 1
	  call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      CYLREG(inc) = inum
	      if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Chamfering codes
c
 1800 ist    = 18
 1810 inc    = ist    + 5
      if (ICYLFL(1) .ne. 1) then
	  if (kfl .lt. NLEVL) go to 1895
	  call errmsg ('NOTAPPLY',1,2)
	  go to 8000
      endif
      do 1890 i=ist,19,1
	  inc    = inc    + 1
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = CYLCOD(inc)
	  rnum   = CYLCDV(inc)
	  nc     = 1
	  call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      CYLCOD(inc) = inum
	      CYLCDV(inc) = rnum
	      if (kfl .ge. NLEVL) go to 8000
          endif
 1890 continue
 1895 continue
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
c   SUBROUTINE:  cylpos (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle positioning parameters.
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
      subroutine cylpos (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYLFL,KPOSMP(1901)), (CYLCOD,KPOSMP(1921))
c
      integer*4 ICYLFL(30),CYLCOD(25)
c
      equivalence (CYLVR ,POSMAP(2993))
c
      real*8 CYLVR(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum,iprm(2),inc,i
c
      real*8 rnum,rmx
c
      data iprm /14,18/
      data iyesno /13,12/
      data rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,300,400,500), MLEVL(NLEVL)
c
c...Position to taper prior to cycle
c
  100 ist    = 1
  110 inc    = ist    + 7
      do 190 i=ist,2,1
	  inc    = inc    + 1
	  MLEVL(NLEVL) = i
	  if (ICYLFL(1) .ne. 1 .or. CYLCOD(iprm(i)) .eq. 0) then
	      if (kfl .lt. NLEVL) go to 190
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  iwrn   = 0
	  inum   = ICYLFL(inc)
	  call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      ICYLFL(inc) = inum
	      if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Mode for canned cycles
c
  300 MLEVL(NLEVL) = 3
      if (ICYLFL(1) .ne. 1) then
	  if (kfl .lt. NLEVL) go to 395
	  call errmsg ('NOTAPPLY',1,2)
	  go to 8000
      endif
      iwrn   = 0
      inum   = ICYLFL(2)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYLFL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Step parameters format
c
  400 MLEVL(NLEVL) = 4
      if (ICYLFL(1) .ne. 1) then
	  if (kfl .lt. NLEVL) go to 495
	  call errmsg ('NOTAPPLY',1,2)
	  go to 8000
      endif
      iwrn   = 0
      inum   = ICYLFL(3)
      nc     = 1
c...only 3 choices in 'postmenu.txt' shows
c...Yurong
c...      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYLFL(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Rapid cycle rate
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      rnum   = CYLVR(1)
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CYLVR(1) = rnum
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
c   SUBROUTINE:  cylout (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle output blocks format prompt handling routine.
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
      subroutine cylout (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYLFL,KPOSMP(1901))
c
      integer*4 ICYLFL(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,iyesno(2),inum,i,inc,isub(6)
c
      data iyesno /13,12/
c
      data isub /4,5,6,7,8,11/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110), MLEVL(NLEVL)
c
c...Output cycle block formats
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,6,1
	  inc    = inc    + 1
	  if (inc .ne. 6 .and. ICYLFL(1) .ne. 1) then
	      if (kfl .lt. NLEVL) go to 190
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = ICYLFL(isub(inc))
	  call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      ICYLFL(isub(inc)) = inum
	      if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
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
c   SUBROUTINE:  cylblk (kfl,cmsg,kerr)
c
c   FUNCTION:  User defined cycle blocks prompt handling routine.
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
      subroutine cylblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CYLCOD,KPOSMP(1921))
      equivalence (MUSRBK,KPOSMP(3022)), (ICYLBK,KPOSMP(3080))
c
      integer*4 MUSRBK,ICYLBK(20),CYLCOD(25)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,inum,i,inc
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,110,110,110,110,
     1       110), MLEVL(NLEVL)
c
c...User defined cycle blocks
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,15,1
	  inc    = inc    + 1
	  if (inc .ne. 1 .and. CYLCOD(inc+3) .eq. 0) then
	      if (kfl .lt. NLEVL) go to 190
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = ICYLBK(inc)
	  nc     = 1
	  call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      ICYLBK(inc) = inum
	      if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
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
c   SUBROUTINE:  cyltim (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle time calculations prompt handling routine.
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
      subroutine cyltim (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CYLCOD,KPOSMP(1921)), (ICYLTM,KPOSMP(1971))
c
      integer*4 CYLCOD(25),ICYLTM(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,iyesno(2),inum,ityp(7),i,inc
c
      data iyesno /13,12/
      data ityp /53,54,55,62,63,61,64/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,110,110,110,110),
     1           MLEVL(NLEVL)
c
c...Cycle time calculations
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,14,1
	  inc    = inc    + 1
	  if (CYLCOD(inc+4) .eq. 0) then
	      if (kfl .lt. NLEVL) go to 190
	      call errmsg ('NOTAPPLY',1,2)
	      go to 8000
	  endif
	  MLEVL(NLEVL) = i
	  iwrn   = 0
	  inum   = ICYLTM(inc)
	  call prmvoc (inum,ityp,7,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
	      if (iwrn .eq. 2) go to 7000
	      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
	      ICYLTM(inc) = inum
	      if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
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
