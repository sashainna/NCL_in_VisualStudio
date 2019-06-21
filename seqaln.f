c
c***********************************************************************
c
c   FILE NAME:  seqaln.for
c   CONTAINS:
c               seqaln
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        seqaln.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  seqaln (kfl,cmsg,kerr)
c
c   FUNCTION:  Sequence numbers and alignment blocks prompt handling
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
      subroutine seqaln (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ISEQSW,KPOSMP(0845)), (SEQFRQ,KPOSMP(0846))
      equivalence (SEQCOD,KPOSMP(0847)), (ALNCOD,KPOSMP(0848))
      equivalence (AL1BLK,KPOSMP(0849)), (AL2BLK,KPOSMP(0850))
      equivalence (AL3BLK,KPOSMP(0854)), (MUSRBK,KPOSMP(3022))
c
      integer*4 ISEQSW,SEQFRQ,SEQCOD,ALNCOD,AL1BLK,AL2BLK,MUSRBK,AL3BLK
c
      equivalence (DUMMY ,POSMAP(0003)), (ISEQ  ,POSMAP(1164))
      equivalence (SEQINC,POSMAP(1165)), (SEQMAX,POSMAP(1167))
      equivalence (ISEQSV,POSMAP(1182))
c
      real*8 DUMMY,ISEQ,SEQINC,SEQMAX,ISEQSV
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,nc,inum,iyesno(2),imx
c
      real*8 rnum,rmx
c
      data iyesno /13,12/, imx /99999999/
      data rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 go to (100,200,300,400,500,600,700,800,900,1000), MLEVL(NLEVL)
c
c...Output sequence numbers
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ISEQSW
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISEQSW = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Sequence number register
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = SEQCOD
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SEQCOD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Beginning sequence number
c
  300 MLEVL(NLEVL) = 3
      if (ISEQSW .eq. 2) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = ISEQ
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISEQ   = rnum
          ISEQSV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Sequence number increment
c
  400 MLEVL(NLEVL) = 4
      if (ISEQSW .eq. 2) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = SEQINC
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SEQINC = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Sequence number on every nth block
c
  500 MLEVL(NLEVL) = 5
      if (ISEQSW .eq. 2) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = SEQFRQ
      nc     = 1
      call prmint (inum,nc,1,1,imx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SEQFRQ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Beginning sequence number
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      rnum   = SEQMAX
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SEQMAX = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  695 continue
c
c...Alignment block register
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = ALNCOD
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ALNCOD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Non-motion alignment block
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = AL1BLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          AL1BLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Motion alignment block
c
  900 MLEVL(NLEVL) = 9
      iwrn   = 0
      inum   = AL2BLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          AL2BLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Cycle alignment block
c
 1000 MLEVL(NLEVL) = 10
      iwrn   = 0
      inum   = AL3BLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          AL3BLK  = inum
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
