c
c***********************************************************************
c
c   FILE NAME:  cycmil.for
c   CONTAINS:
c               cycmil  cymcod  cymprm  cympos  cympln  cymout  cymblk
c               cymtim
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cycmil.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:54
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cycmil (kfl,cmsg,kerr)
c
c   FUNCTION:  Mill canned cycles menu handling routine.
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
      subroutine cycmil (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (MACHTP,KPOSMP(1201))
c
      integer*4 ICYCFL(30),MACHTP
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ifl,igo,iwlk
c
c...This menu valid for mills only
c
      if (MACHTP .eq. 2) then
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
      go to (1000,1100,1200,1300,1400,1500,1600,1700,8000), igo
c
c...Walk through menu
c
 1000 IWALK  = 1
c
c...Cycle definition codes
c
 1100 MLEVL(NLEVL-1) = 1
      call cymcod (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle parameter codes
c
 1200 if (ICYCFL(1) .ne. 1) then
          if (IWALK .eq. 1) go to 1300
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 2
      call cymprm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle positioning parameters
c
 1300 MLEVL(NLEVL-1) = 3
      call cympos (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Rapto & Retract plane logic
c
 1400 MLEVL(NLEVL-1) = 4
      call cympln (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle block output
c
 1500 MLEVL(NLEVL-1) = 5
      call cymout (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle user defined blocks
c
 1600 if (ICYCFL(1) .ne. 1) then
          if (IWALK .eq. 1) go to 1700
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 6
      call cymblk (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Cycle machining times
c
 1700 if (ICYCFL(1) .ne. 1) then
          if (IWALK .eq. 1) go to 7000
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 7
      call cymtim (kfl,cmsg,kerr)
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
c   SUBROUTINE:  cymcod (kfl,cmsg,kerr)
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
      subroutine cymcod (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (PLCYCD,KPOSMP(0296)), (MACCOD,KPOSMP(0801))
c
      integer*4 ICYCFL(30),CYCCOD(20),PLCYCD(3),MACCOD(5)
c
      equivalence (MACCDV,POSMAP(1184))
      equivalence (CYCCDV,POSMAP(2901)), (CYPCDV,POSMAP(2998))
c
      real*8 CYCCDV(25),CYPCDV(3),MACCDV(5)
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
      go to (100,200,300,400,510,510,510,510,510,510,510,510,510,510,
     1       510,510,510,510,1910,1910,1910,2210,2210,2210),
     2           MLEVL(NLEVL)
c
c...Canned cycle support
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ICYCFL(1)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (iwrn .eq. 2) go to 7000
      if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
      ICYCFL(1) = inum
      ICYCFL(21) = inum
      if (kfl .ge. NLEVL) go to 8000
c
c...All planes canned cycle support
c
  200 MLEVL(NLEVL) = 2
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(3)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Output cycles as Macro Call
c
  300 MLEVL(NLEVL) = 3
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(30)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(30) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Macro Call Code
c
  400 MLEVL(NLEVL) = 4
      if (ICYCFL(1) .ne. 1 .or. ICYCFL(30) .ne. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = MACCOD(1)
      rnum   = MACCDV(1)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MACCOD(1) = inum
          MACCDV(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Cycle codes
c
  500 ist    = 5
  510 inc    = ist    - 5
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 590 i=ist,18,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CYCCOD(inc)
          rnum   = CYCCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CYCCOD(inc) = inum
              CYCCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  590 continue
  595 continue
c
c...Cycle plane selection codes
c
 1900 ist    = 19
 1910 inc    = ist    - 19
      if (ICYCFL(1) .ne. 1 .or. ICYCFL(3) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1990 i=ist,21,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PLCYCD(inc)
          rnum   = CYPCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PLCYCD(inc) = inum
              CYPCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1990 continue
 1995 continue
c
c...Rapid & Retract codes
c
 2200 ist    = 22
 2210 inc    = ist    - 8
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 2295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2290 i=ist,24,1
          inc    = inc    + 1
c
c...vp 5/5/98 RP interupt is in (15), RCT/ON-OFF in (19,20)
c
          if (i .ne. 22) inc = i - 4
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CYCCOD(inc)
          rnum   = CYCCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CYCCOD(inc) = inum
              CYCCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2290 continue
 2295 continue
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
c   SUBROUTINE:  cymprm (kfl,cmsg,kerr)
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
      subroutine cymprm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
c
      integer*4 ICYCFL(30),CYCREG(20)
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,inum,isub(15)
c
      real*8 rnum
c
      data iyesno /13,12/
      data isub /1,15,2,3,4,5,6,7,8,9,10,11,12,13,14/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,110,110,1300,1410,
     1       1410,1410), MLEVL(NLEVL)
c
c...Cycle parameter codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,12,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CYCREG(isub(inc))
          rnum   = DUMMY
          nc     = 1
          if (ICYCFL(30) .eq. 1) then
              if (inum .gt. 30) inum = 30
              call prmint (inum,nc,1,0,30,kfl,ilod,iwrn,cmsg,kerr)
          else
              call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          endif
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CYCREG(isub(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Output XY offset codes in all planes
c
 1300 MLEVL(NLEVL) = 13
      if (CYCREG(9) .eq. 0 .and. CYCREG(10) .eq. 0) then
          if (kfl .lt. NLEVL) go to 1395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(19)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(19) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1395 continue
c
c...Cycle parameter codes
c
 1400 ist    = 14
 1410 inc    = ist    - 2
      do 1490 i=ist,16,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CYCREG(isub(inc))
          rnum   = DUMMY
          nc     = 1
          if (ICYCFL(30) .eq. 1) then
              if (inum .gt. 30) inum = 30
              call prmint (inum,nc,1,0,30,kfl,ilod,iwrn,cmsg,kerr)
          else
              call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          endif
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CYCREG(isub(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1490 continue
 1495 continue
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
c   SUBROUTINE:  cympos (kfl,cmsg,kerr)
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
      subroutine cympos (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (CYCREG,KPOSMP(0231))
c
      integer*4 ICYCFL(30),CYCCOD(20),CYCREG(20)
c
      equivalence (CYCVR ,POSMAP(2926)), (CYCJVC,POSMAP(2961))
c
      real*8 CYCVR(5),CYCJVC(3)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum
c
      real*8 rnum,rmn(3),rmx(3),rary(3)
c
      data iyesno /13,12/
      data rmn /-99999999.d0,-99999999.d0,-99999999.d0/
      data rmx /99999999.d0,99999999.d0,99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,500,600,700,800,900), MLEVL(NLEVL)
c
c...Unidirectional positioning
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ICYCFL(2)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Unidirectional positioning direction
c
  200 MLEVL(NLEVL) = 2
      if (ICYCFL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rary(1) = CYCJVC(1)
      rary(2) = CYCJVC(2)
      rary(3) = CYCJVC(3)
      nc     = 3
  220 call prmrel (rary,nc,3,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          rnum   = dsqrt(rary(1)**2 + rary(2)**2 + rary(3)**2)
          if (nc .ne. 3 .or. rnum .eq. 0.) then
              call errmsg ('INVRESP',1,2)
              iwrn   = 1
              if (IMSCAN .eq. 2) go to 8000
              go to 220
          endif
          CYCJVC(1) = rary(1) / rnum
          CYCJVC(2) = rary(2) / rnum
          CYCJVC(3) = rary(3) / rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Unidirectional positioning distance
c
  300 MLEVL(NLEVL) = 3
      if (ICYCFL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = CYCVR(1)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CYCVR(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Position prior to CYCLE/DEEP
c
  400 MLEVL(NLEVL) = 4
      if (ICYCFL(1) .ne. 1 .or. CYCCOD(6) .eq. 0) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(24)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(24) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Final depth output
c
  500 MLEVL(NLEVL) = 5
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(4)
      nc     = -1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Final depth relative position
c
  600 MLEVL(NLEVL) = 6
      if (ICYCFL(1) .ne. 1 .or. ICYCFL(4) .eq. 2) then
          if (kfl .lt. NLEVL) go to 695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(6)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(6) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  695 continue
c
c...Top-of-part mode
c
  700 MLEVL(NLEVL) = 7
      if (ICYCFL(1) .ne. 1 .or. CYCREG(15) .eq. 0) then
          if (kfl .lt. NLEVL) go to 795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(7)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(7) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
c
c...Step parameters format
c
  800 MLEVL(NLEVL) = 8
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(12)
      nc     = 1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(12) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Rapid cycle rate
c
  900 MLEVL(NLEVL) = 9
      iwrn   = 0
      rnum   = CYCVR(3)
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CYCVR(3) = rnum
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
c   SUBROUTINE:  cympln (kfl,cmsg,kerr)
c
c   FUNCTION:  Cycle rapto and retract plane parameters.
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
      subroutine cympln (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (CYCREG,KPOSMP(0231)), (NUMLIN,KPOSMP(1202))
      equivalence (ICY2FL,KPOSMP(4004))
c
      integer*4 ICYCFL(30),CYCCOD(20),CYCREG(20),ICY2FL(20),
     -          NUMLIN(3)
c
      equivalence (CYCVR ,POSMAP(2926))
c
      real*8 CYCVR(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
      data rmn /-99999999.d0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300)
     1       MLEVL(NLEVL)
c
c...Rapto plane mode
c
  100 MLEVL(NLEVL) = 1
      if (ICYCFL(1) .ne. 1 .or. CYCREG(2) .eq. 0) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(8)
      nc     = 1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(8) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  195 continue
c
c...Rapto plane calcultaion type
c
  200 MLEVL(NLEVL) = 2
      if (ICYCFL(1) .ne. 1 .or. CYCREG(2) .eq. 0 .or.
     -    NUMLIN(1)+NUMLIN(2)+NUMLIN(3) .lt. 4) then
          if (kfl .lt. NLEVL) go to 300
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICY2FL(1)
      nc     = 1
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICY2FL(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Rapto plane gage height
c
  300 MLEVL(NLEVL) = 3
      if (ICYCFL(1) .ne. 1 .or. CYCREG(2) .eq. 0) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = CYCVR(2)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CYCVR(2) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Retract plane mode
c
  400 MLEVL(NLEVL) = 4
      if (ICYCFL(1) .ne. 1 .or. CYCREG(12) .eq. 0) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(27)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(27) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Retract plane calcultaion type
c
  500 MLEVL(NLEVL) = 5
      if (ICYCFL(1) .ne. 1 .or. CYCREG(12) .eq. 0 .or.
     -    NUMLIN(1)+NUMLIN(2)+NUMLIN(3) .lt. 4) then
          if (kfl .lt. NLEVL) go to 600
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICY2FL(2)
      nc     = 1
c...only 2 choices in 'postmenu.txt' shows
c...Yurong
c...      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICY2FL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract plane logic
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = ICYCFL(9)
      nc     = -1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(9) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract plane override
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = ICYCFL(26)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(26) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract code output
c
  800 MLEVL(NLEVL) = 8
      if (ICYCFL(1) .ne. 1 .or. CYCREG(12) .eq. 0 .or. ICYCFL(9) .ne. 1)
     1        then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(28)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(28) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Retract block output
c
  900 MLEVL(NLEVL) = 9
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(10)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(10) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  995 continue
c
c...Retract block format
c
 1000 MLEVL(NLEVL) = 10
      if (ICYCFL(1) .ne. 1 .or. ICYCFL(10) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(11)
      nc     = 1
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(11) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1095 continue
c
c...Retract tool at each level during CYCLE/THRU
c
 1100 MLEVL(NLEVL) = 11
      if (ICYCFL(1) .ne. 1 .or. CYCCOD(14) .eq. 0) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYCFL(25)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYCFL(25) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1195 continue
c
c...Alter motion for rapid moves
c
 1200 MLEVL(NLEVL) = 12
      iwrn   = 0
      inum   = ICY2FL(4)
      nc     = -1
      call prmint (inum,nc,1,1,5,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICY2FL(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract/plunge along tool axis
c
 1300 MLEVL(NLEVL) = 13
      if (ICY2FL(4) .ne. 5) then
          if (kfl .lt. NLEVL) go to 1395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICY2FL(5)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICY2FL(5) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1395 continue
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
c   SUBROUTINE:  cymout (kfl,cmsg,kerr)
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
      subroutine cymout (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCREG,KPOSMP(0231))
      equivalence (ICY2FL,KPOSMP(4004))
c
      integer*4 ICYCFL(30),CYCREG(20),ICY2FL(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,iyesno(3),inum,i,isw(10),inc,nc
c
      data isw /20,13,29,23,14,5,-3,15,16,22/
      data iyesno /13,12,83/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110), MLEVL(NLEVL)
c
c...Output cycle block formats
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,10,1
          inc    = inc    + 1
          if ((isw(inc) .ne. 16 .and. ICYCFL(1) .ne. 1) .or.
     1            (isw(inc) .eq. 5 .and. ICYCFL(14) .eq. 2) .or.
     2            (isw(inc) .eq. -3 .and. CYCREG(3) .eq. 0)) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          if (isw(inc) .lt. 0) then
              inum   = ICY2FL(-isw(inc))
          else
              inum   = ICYCFL(isw(inc))
          endif
          nc     = 2
          if (i .eq. 5) nc = -3
          call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (isw(inc) .lt. 0) then
                  ICY2FL(-isw(inc)) = inum
              else
                  ICYCFL(isw(inc)) = inum
              endif
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
c   SUBROUTINE:  cymblk (kfl,cmsg,kerr)
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
      subroutine cymblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CYCCOD,KPOSMP(0211))
      equivalence (MUSRBK,KPOSMP(3022)), (ICYBLK,KPOSMP(3060))
c
      integer*4 MUSRBK,ICYBLK(20),CYCCOD(20)
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
      go to (110,110,110,110,110,110,110,110,110,110,110,110,110,110),
     1           MLEVL(NLEVL)
c
c...User defined cycle blocks
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,14,1
      inc    = inc    + 1
      if (inc .ne. 1 .and. CYCCOD(inc) .eq. 0) then
          if (kfl .lt. NLEVL) go to 190
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = i
      iwrn   = 0
      inum   = ICYBLK(inc)
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYBLK(inc) = inum
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
c   SUBROUTINE:  cymtim (kfl,cmsg,kerr)
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
      subroutine cymtim (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICYCFL,KPOSMP(0181)), (CYCCOD,KPOSMP(0211))
      equivalence (ICYTIM,KPOSMP(0251)), (ICYDLT,KPOSMP(1742))
c
      integer*4 ICYCFL(30),CYCCOD(20),ICYTIM(20),ICYDLT
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,iyesno(2),inum,ityp(13),i,inc
c
      data iyesno /13,12/
      data ityp /49,50,51,52,53,54,55,56,57,58,59,60,61/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,110,110,110,
     1       1410,1410,1600), MLEVL(NLEVL)
c
c...Cycle time calculations
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,13,1
      inc    = inc    + 1
      if (CYCCOD(inc+1) .eq. 0) then
          if (kfl .lt. NLEVL) go to 190
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = i
      iwrn   = 0
      inum   = ICYTIM(inc)
      call prmvoc (inum,ityp,13,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYTIM(inc) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  190 continue
c
c...STEP time considerations
c
 1400 ist    = 14
 1410 inc    = ist    + 2
      do 1490 i=ist,15,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = ICYCFL(inc)
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ICYCFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1490 continue
c
c...Estimate axes movement during automatic cycles
c
 1600 MLEVL(NLEVL) = 16
      if (ICYCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICYDLT
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICYDLT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1695 continue
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
