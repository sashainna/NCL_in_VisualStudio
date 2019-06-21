c
c***********************************************************************
c
c   FILE NAME:  miscsu.for
c   CONTAINS:
c               miscsu
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        miscsu.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:57
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  miscsu (kfl,cmsg,kerr)
c
c   FUNCTION:  Miscellaneous setup handling routine.
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
      subroutine miscsu (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (MACH  ,KPOSMP(0086))
      equivalence (MCHOPT,KPOSMP(0308)), (UNTCOD,KPOSMP(1078))
c
      integer*4 MCHOPT(20),UNTCOD(2),MACH
c
      equivalence (UNTCDV,POSMAP(1170))
c
      real*8 UNTCDV(2)
c
      equivalence (LMNAME,CPOSMP(0141)), (MDESC ,CPOSMP(3853))
      equivalence (PU1EXT,CPOSMP(4067))
c
      character*40 LMNAME
      character*80 PU1EXT,MDESC
c
      integer*4 ilod,iwrn,nc,inum,iunit(2),ist,i,strlen1,inc,ierr,
     1          ierror(4)
c
      real*8 rnum
c
      character*80 lmach
      character*80 sbuf
c
      data ierror /12,1016,1017,1018/, iunit /14,15/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,510,510,700,800), MLEVL(NLEVL)
c
c...Get current Machine name
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      lmach  = LMNAME
      nc     = 1
  110 call prmstr (nc,lmach,0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (nc .eq. 0) lmach = '0'
          LMNAME = lmach
          MACH = 0
          call ctoi (LMNAME,MACH,ierr)
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Machine description
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      lmach  = MDESC
      nc     = 1
  210 call prmstr (nc,lmach,0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MDESC  = lmach
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get input units
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = MCHOPT(1)
      call prmvoc (inum,iunit,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MCHOPT(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get output units
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = MCHOPT(2)
      call prmvoc (inum,iunit,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MCHOPT(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Unit selection codes
c
  500 ist    = 5
  510 inc    = ist    - 5
      do 590 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = UNTCOD(inc)
          rnum   = UNTCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              UNTCOD(inc) = inum
              UNTCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  590 continue
c
c...Punch file extension
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      sbuf   = PU1EXT
      nc     = strlen1(sbuf)
      call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (nc .eq. -1 .or. kerr .ne. 0) go to 8000
      if (nc .eq. -2) go to 7000
      PU1EXT = sbuf
      if (kfl .ge. NLEVL) go to 8000
c
c...Output MCD file if an error occurs
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = MCHOPT(7) + 1
      nc     = 1
      call prmvoc (inum,ierror,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MCHOPT(7) = inum   - 1
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
