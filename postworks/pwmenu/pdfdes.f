c
c***********************************************************************
c
c   FILE NAME:  pdfdes.for
c   CONTAINS:
c               pdfdes
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pdfdes.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:57
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  pdfdes (kfl,cmsg,kerr)
c
c   FUNCTION:  Print Descriptor File handling routine.
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
      subroutine pdfdes (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (IPRDES,KPOSMP(1154))
c
      integer*4 IPRDES(2,10)
c
      equivalence (PDFFIL,CPOSMP(3249))
c
      character*40 PDFFIL
c
      integer*4 ilod,iwrn,nc,inum,iary(2),imn(2),imx(2),ist,i,inc
c
      character*40 lfil
c
      data imn /0,0/, imx /10,10/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist   = MLEVL(NLEVL)
      go to (100,210,210,210,210,210,210,210,210,210,210), MLEVL(NLEVL)
c
c...Get Print File Descriptor name
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      lfil   = PDFFIL
      nc     = 1
  110 call prmstr (nc,lfil,0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0 .or. inum .lt. 0) go to 8000
          if (nc .eq. 0) then
              lfil   = '0'
              nc     = 1
          endif
          PDFFIL = lfil
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Print File Descriptor records
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,11,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          iary(1) = IPRDES(1,inc)
          iary(2) = IPRDES(2,inc)
          nc     = 2
          if (iary(2) .eq. 0) nc = 1
          if (iary(1) .eq. 0 .and. nc .eq. 1) nc = 0
          call prmint (iary,nc,2,imn,imx,kfl,ilod,iwrn,cmsg,kerr)
          if (iwrn .eq. 2) go to 7000
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (nc .eq. 0) then
                  IPRDES(1,inc) = 0
                  IPRDES(2,inc) = 0
              else if (nc .eq. 1) then
                  IPRDES(1,inc) = iary(1)
                  IPRDES(2,inc) = 0
              else
                  IPRDES(1,inc) = iary(1)
                  IPRDES(2,inc) = iary(2)
              endif
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
