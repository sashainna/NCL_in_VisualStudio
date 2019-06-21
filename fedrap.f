c
c***********************************************************************
c
c   FILE NAME:  fedrap.for
c   CONTAINS:
c               fedrap  frpmsc  frpfpm  frpfpr  frpdpm  frpinv  frptbl
c               frprap  frpreg  frplmt  frpacl
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        fedrap.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:55
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  fedrap (kfl,cmsg,kerr)
c
c   FUNCTION:  Feedrate & rapid menu handling routine.
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
      subroutine fedrap (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IROTFT,KPOSMP(3110)), (IFDSUP,KPOSMP(3111))
      equivalence (IFDTYP,KPOSMP(3148))
c
      integer*4 IRTNUM,IROTFT,IFDSUP(4),IFDTYP(2)
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
      go to (1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     1       2100,8000), igo
c
c...Walk through menu
c
 1000 IWALK  = 1
c
c...Feedrate miscellaneous set-up
c
 1100 MLEVL(NLEVL-1) = 1
      call frpmsc (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Feed per Minute (FPM)
c
 1200 MLEVL(NLEVL-1) = 2
      call frpfpm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Feed per Revolution (FPR)
c
 1300 MLEVL(NLEVL-1) = 3
      call frpfpr (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Degrees per Minute (DPM)
c
 1400 if (IRTNUM .eq. 0 .or. (IROTFT .ne. 3 .and. IROTFT .ne. 5 .and.
     1    IROTFT .ne. 6) .or. IFDSUP(3) .ne. 1) then
          if (IWALK .eq. 1) go to 1500
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 4
      call frpdpm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Inverse time
c
 1500 if (IFDSUP(4) .ne. 1) then
          if (IWALK .eq. 1) go to 1600
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 5
      call frpinv (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...FPM table
c
 1600 if (IFDSUP(1) .ne. 1 .or. IFDTYP(1) .ne. 2) then
          if (IWALK .eq. 1) go to 1700
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 6
      call frptbl (kfl,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...FPR table
c
 1700 if (IFDSUP(2) .ne. 1 .or. IFDTYP(2) .ne. 2) then
          if (IWALK .eq. 1) go to 1800
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 7
      call frptbl (kfl,2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Rapid setup
c
 1800 MLEVL(NLEVL-1) = 8
      call frprap (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Output value limits
c
 1900 MLEVL(NLEVL-1) = 9
      call frpreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Feedrate limits
c
 2000 MLEVL(NLEVL-1) = 10
      call frplmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Accel/Decel time calculations
c
 2100 MLEVL(NLEVL-1) = 11
      call frpacl (kfl,cmsg,kerr)
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
c   SUBROUTINE:  frpmsc (kfl,cmsg,kerr)
c
c   FUNCTION:  Miscellaneous feedrate prompt handling routine.
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
      subroutine frpmsc (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IROTFT,KPOSMP(3110))
      equivalence (IFDSUP,KPOSMP(3111)), (IFITYP,KPOSMP(3150))
      equivalence (IFOTYP,KPOSMP(3151)), (NFDSUP,KPOSMP(3152))
      equivalence (IFDFRC,KPOSMP(3153)), (IFDRCD,KPOSMP(3213))
      equivalence (FEDOCD,KPOSMP(3217)), (IFDOUT,KPOSMP(3158))
      equivalence (IFDRAD,KPOSMP(3210)), (IFDRCN,KPOSMP(3211))
c
      integer*4 IRTNUM,IROTFT,IFDSUP(4),IFITYP,NFDSUP,IFDFRC(4),
     1          FEDOCD(2),IFDOUT(4),IFOTYP,IFDRAD,IFDRCD(4),IFDRCN
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDOVL,POSMAP(3001))
c
      real*8 FEDOVL(2),DUMMY
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),isub(4),ityp(3),inc,inum
c
      real*8 rnum
c
      data iyesno /13,12/, ityp /40,41,42/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,500,610,610,800,900,1050,1050,1050,
     -       1050,1110,1110) MLEVL(NLEVL)
c
c...Supported feedrate modes
c
  100 ist    = 1
  110 inc    = ist    - 1
      isub(1) = 1
      isub(2) = 1
      isub(3) = 1
      isub(4) = 1
      if (IRTNUM .eq. 0) then
          isub(3) = 0
          IFDSUP(3) = 2
      endif
      do 190 i=ist,4,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IFDSUP(inc)
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IFDSUP(inc) = inum
              if (kfl .ge. NLEVL) go to 192
          endif
  190 continue
  192 NFDSUP = 0
      do 195 i=1,4,1
          if (IFDSUP(i) .eq. 1) then
              if (i .le. 2) IFDOUT(i) = i
              NFDSUP = NFDSUP + 1
              inc    = i
          endif
  195 continue
      do 205 i=1,4
         if (IFDSUP(i) .ne. 1 .and. IFDOUT(i) .ne. i) then
            IFDOUT(i) = inc
         end if
  205 continue
      if (NFDSUP .eq. 1 .and. IROTFT .ne. 7) IROTFT = inc
      if (kfl .ge. NLEVL) go to 8000
c
c...Default feedrate mode
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = IFITYP
      if (inum .eq. 4) inum = 3
      call prmvoc (inum,ityp,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (inum .eq. 3) inum = 4
          IFITYP = inum
          IFOTYP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Force feedrate output
c
  600 ist    = 6
  610 inc    = ist    - 6
      isub(1) = 1
      isub(2) = 1
      if (NFDSUP .le. 1) isub(2) = 0
      do 690 i=ist,7,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IFDFRC(inc)
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IFDFRC(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
c
c...Rotary axes feed rate mode
c
  800 if (IRTNUM .eq. 0) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = IROTFT
      nc     = -1
      call prmint (inum,nc,1,1,7,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IROTFT  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Output rotary axes radius with
c...Feedrate control point output
c
  900 if (IRTNUM .eq. 0 .or. IROTFT .ne. 7) then
          if (kfl .lt. NLEVL) go to 995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 9
      iwrn   = 0
      inum   = IFDRAD
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDRAD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  995 continue
c
c..Rotary axes radius code
c
 1000 ist    = 10
c
 1050 inc    = ist - 9
      if (inc .gt. IRTNUM .or. IROTFT .ne. 7 .or. IFDRAD .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1055 i=inc,IRTNUM,1
          MLEVL(NLEVL) = i + 9
          iwrn   = 0
          inum   = IFDRCD(i)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IFDRCD(i) = inum
          endif
 1055 continue
      IFDRCN = 0
      do 1065 i=1,IRTNUM
          if (IFDRCD(i) .ne. 0) IFDRCN = IFDRCN + 1
 1065 continue
      if (kfl .ge. NLEVL) go to 8000
 1095 continue
c
c..Feedrate override codes
c
 1100 ist    = 14
 1110 if (NFDSUP .eq. 0) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      inc    = ist    - 14
      do 1190 i=ist,15,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = FEDOCD(inc)
          rnum   = FEDOVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              FEDOCD(inc) = inum
              FEDOVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
 1195 continue
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
c   SUBROUTINE:  frpfpm (kfl,cmsg,kerr)
c
c   FUNCTION:  Feed per Minute prompt handling routine.
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
      subroutine frpfpm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IFDSUP,KPOSMP(3111)), (IFDTYP,KPOSMP(3148))
      equivalence (IFDCTP,KPOSMP(3157))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170)), (IFDEXD,KPOSMP(3178))
      equivalence (IRTNUM,KPOSMP(1243))
c
      integer*4 IFDSUP(4),IFDTYP(2),IFDOUT(4),FEDCD(8),FEDMCD(8),
     1          IFDEXD(4),IRTNUM,IFDCTP
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDCNS,POSMAP(3003))
      equivalence (FEDMVL,POSMAP(3320)), (DPMBPW,POSMAP(3551))
c
      real*8 DUMMY,FEDCNS(4),FEDMVL(8),DPMBPW(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),ityp(2),inum,i,inc
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/, ityp /41,42/
      data rmn /-99999999.d0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,500,610,610,700,800,900,1000), MLEVL(NLEVL)
c
c...Output FPM feed rates as ...
c
  100 MLEVL(NLEVL) = 1
      if (IFDSUP(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      if (IFDOUT(1) .ne. 2 .and. IFDOUT(1) .ne. 4) IFDOUT(1) = 2
      inum   = IFDOUT(1)
      if (inum .eq. 2) inum = 1
      if (inum .eq. 4) inum = 2
      call prmvoc (inum,ityp,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (inum .eq. 2) inum = 4
          if (inum .eq. 1) inum = 2
          IFDOUT(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  195 continue
c
c...Output FPM feed rates using table
c
  200 MLEVL(NLEVL) = 2
      if (IFDSUP(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = 1
      if (IFDTYP(1) .eq. 1) inum = 2
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDTYP(1) = 1
          if (inum .eq. 1) then
              IFDTYP(1) = 2
              IFDEXD(1) = 2
          endif
      endif
      if (kfl .ge. NLEVL) go to 8000
  295 continue
c
c...Output FPM feed rates using control point
c
  300 MLEVL(NLEVL) = 3
      if (IFDSUP(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      if (IFDCTP .eq. 0) IFDCTP = 2
      inum   = IFDCTP
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDCTP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...FPM feed rate register
c
  400 MLEVL(NLEVL) = 4
      if (IFDSUP(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDCD(1)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...FPM mode register
c
  500 MLEVL(NLEVL) = 5
      if (IFDSUP(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDMCD(1)
      rnum   = FEDMVL(1)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(1) = inum
          FEDMVL(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Linear/rotary pulse weights
c
  600 ist    = 6
  610 inc    = ist    - 6
      if (IRTNUM .eq. 0) then
          if (kfl .lt. NLEVL) go to 700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 690 i=ist,7,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = DPMBPW(inc)
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              DPMBPW(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
c
c...Extended precision feed rates
c
  700 MLEVL(NLEVL) = 8
      if (IFDSUP(1) .ne. 1 .or. IFDTYP(1) .eq. 2) then
          if (kfl .lt. NLEVL) go to 795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IFDEXD(1)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDEXD(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
c
c...Multiplication constant for ex. prec. FPM
c
  800 if (IFDSUP(1) .ne. 1 .or. IFDEXD(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 9
      iwrn   = 0
      rnum   = FEDCNS(1)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCNS(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Ext. Prec. FPM feed rate register
c
  900 MLEVL(NLEVL) = 10
      if (IFDSUP(1) .ne. 1 .or. IFDEXD(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDCD(5)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(5) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  995 continue
c
c...Ext. Prec. FPM mode register
c
 1000 MLEVL(NLEVL) = 11
      if (IFDSUP(1) .ne. 1 .or. IFDEXD(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDMCD(5)
      rnum   = FEDMVL(5)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(5) = inum
          FEDMVL(5) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1095 continue
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
c   SUBROUTINE:  frpfpr (kfl,cmsg,kerr)
c
c   FUNCTION:  Feed per Revolution prompt handling routine.
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
      subroutine frpfpr (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IFDSUP,KPOSMP(3111)), (IFDTYP,KPOSMP(3148))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170)), (IFDSPC,KPOSMP(3185))
c
      integer*4 IFDSUP(4),IFDTYP(2),IFDOUT(4),FEDCD(8),FEDMCD(8),
     1          IFDSPC(2)
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDMVL,POSMAP(3320))
      equivalence (FEDSPC,POSMAP(3510))
c
      real*8 DUMMY,FEDMVL(8),FEDSPC(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),ityp(2),inum
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/, ityp /40,42/
      data rmn /0.D0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,500,600), MLEVL(NLEVL)
c
c...Output FPR feed rates as ...
c
  100 MLEVL(NLEVL) = 1
      if (IFDSUP(2) .eq. 1) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      if (IFDOUT(2) .ne. 1 .and. IFDOUT(2) .ne. 4) IFDOUT(2) = 1
      inum   = IFDOUT(2)
      if (inum .eq. 4) inum = 2
      call prmvoc (inum,ityp,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (inum .eq. 2) inum = 4
          IFDOUT(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  195 continue
c
c...Output FPR feed rates using table
c
  200 MLEVL(NLEVL) = 2
      if (IFDSUP(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = 1
      if (IFDTYP(2) .eq. 1) inum = 2
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDTYP(2) = 1
          if (inum .eq. 1) IFDTYP(2) = 2
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...FPR feed rate register
c
  300 MLEVL(NLEVL) = 3
      if (IFDSUP(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDCD(2)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...FPR mode register
c
  400 MLEVL(NLEVL) = 4
      if (IFDSUP(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDMCD(2)
      rnum   = FEDMVL(2)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(2) = inum
          FEDMVL(2) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Force IPR when Z-axis moves
c
  500 MLEVL(NLEVL) = 5
      if (IFDSUP(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IFDSPC(1)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDSPC(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Force FPR when FPM is under ...
c
  600 MLEVL(NLEVL) = 6
      if (IFDSUP(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = FEDSPC(1)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDSPC(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
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
c   SUBROUTINE:  frpdpm (kfl,cmsg,kerr)
c
c   FUNCTION:  Degrees per Minute prompt handling routine.
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
      subroutine frpdpm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (FEDCD ,KPOSMP(3162)), (FEDMCD,KPOSMP(3170))
      equivalence (IDPMOV,KPOSMP(3188))
c
      integer*4 FEDCD(8),FEDMCD(8),IDPMOV
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDMVL,POSMAP(3320))
      equivalence (DPMBPW,POSMAP(3551))
c
      real*8 DUMMY,FEDMVL(8),DPMBPW(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,iyesno(2),inum,nc
c
      real*8 rnum,rmx
c
      data iyesno /13,12/
      data rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300), MLEVL(NLEVL)
c
c...DPM feed rate register
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = FEDCD(3)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...DPM mode register
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = FEDMCD(3)
      rnum   = FEDMVL(3)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(3) = inum
          FEDMVL(3) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...DPM register overrides linear register
c
  300 MLEVL(NLEVL) = 3
      if (FEDCD(3) .eq. 0) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IDPMOV
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IDPMOV = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Linear/rotary pulse weights
c
c 400 ist    = 4
c 410 inc    = ist    - 4
c     do 490 i=ist,5,1
c         inc    = inc    + 1
c         MLEVL(NLEVL) = i
c         iwrn   = 0
c         rnum   = DPMBPW(inc)
c         nc     = 1
c         call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
c         if (iwrn .eq. 2) go to 7000
c         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c         DPMBPW(inc) = rnum
c         if (kfl .ge. NLEVL) go to 8000
c 490 continue
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
c   SUBROUTINE:  frpinv (kfl,cmsg,kerr)
c
c   FUNCTION:  Inverse time prompt handling routine.
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
      subroutine frpinv (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IFDSUP,KPOSMP(3111))
      equivalence (IFDOUT,KPOSMP(3158)), (FEDCD ,KPOSMP(3162))
      equivalence (FEDMCD,KPOSMP(3170)), (IFDEXD,KPOSMP(3178))
      equivalence (IFDUNT,KPOSMP(3182)), (IFDCIR,KPOSMP(3206))
c
      integer*4 IFDSUP(4),IFDOUT(4),FEDCD(8),FEDMCD(8),IFDEXD(4),IFDUNT,
     1          IFDCIR
c
      equivalence (DUMMY ,POSMAP(0003)), (FEDCNS,POSMAP(3003))
      equivalence (FRTVAL,POSMAP(3014))
      equivalence (FEDMVL,POSMAP(3320)), (FDIVCO,POSMAP(3328))
c
      real*8 DUMMY,FEDCNS(4),FEDMVL(8),FDIVCO(2),FRTVAL(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),inc,iunit(2),inum
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/, iunit /43,44/
      data rmn /-99999999.d0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,310,310,500,600,700,800,900,1000,1100,1210,1210),
     1           MLEVL(NLEVL)
c
c...Inverse time calculation mode
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = IFDOUT(4)
      nc     = -1
      call prmint (inum,nc,1,1,6,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDOUT(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Circular Inverse time calculation mode
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      if (IFDOUT(4) .eq. 2) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      inum   = IFDCIR
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDCIR = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Constants for inverse time calculations
c
  300 ist    = 3
  310 inc    = ist    - 3
      if (IFDOUT(4) .ne. 2) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 390 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = FDIVCO(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              FDIVCO(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  390 continue
  395 continue
c
c...Units of time for inverse time feedrates
c
  500 MLEVL(NLEVL) = 5
      if (IFDOUT(4) .eq. 2) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IFDUNT
      call prmvoc (inum,iunit,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDUNT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Inverse time feed rate register
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = FEDCD(4)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  695 continue
c
c...Inverse time mode register
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = FEDMCD(4)
      rnum   = FEDMVL(4)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(4) = inum
          FEDMVL(4) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
c
c...Extended precision feed rates
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = IFDEXD(4)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDEXD(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Multiplication constant for ex. prec. feed
c
  900 if (IFDEXD(4) .ne. 1) then
          if (kfl .lt. NLEVL) go to 995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 9
      iwrn   = 0
      rnum   = FEDCNS(4)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCNS(4) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  995 continue
c
c...Ext. Prec. 1/T feed rate register
c
 1000 MLEVL(NLEVL) = 10
      if (IFDEXD(4) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDCD(8)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDCD(8) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1095 continue
c
c...Ext. Prec. 1/T mode register
c
 1100 MLEVL(NLEVL) = 11
      if (IFDEXD(4) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = FEDMCD(8)
      rnum   = FEDMVL(8)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FEDMCD(8) = inum
          FEDMVL(8) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1195 continue
c
c...Constants for Okuma calculations
c
 1200 ist    = 12
 1210 inc    = ist    - 12
      if (IFDOUT(4) .lt. 4 .or. IFDOUT(4) .gt. 6) then
          if (kfl .lt. NLEVL) go to 1295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1290 i=ist,13,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = FRTVAL(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              FRTVAL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1290 continue
 1295 continue
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
c   SUBROUTINE:  frptbl (kfl,ksw,cmsg,kerr)
c
c   FUNCTION:  Feedrate table definition form handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.  If kfl is greater than or equal
c                               to the current level, then the user re-
c                               quested direct access to a menu/prompt.
c
c           ktb     I*4  D1  -  1 = FPM feedrate table.  2 = FPR feedrate
c                               table.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine frptbl (kfl,ksw,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NFEDTB,KPOSMP(3183))
c
      integer*4 NFEDTB(2)
c
      equivalence (FEDTCD,POSMAP(3330)), (FEDTVL,POSMAP(3420))
c
      real*8 FEDTCD(90),FEDTVL(90)
c
      integer*4 kfl,ksw,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ist,ien,ipt1,ibeg,ierr,inc,isw,imax,strlen1,
     -          nc,nmax
c
      real*8 rnum,rvl(45),rcd(45)
c
      character*20 abuf
      character*80 sbuf,ebuf
c
c...Initialize routine
c
      MLEVL(NLEVL) = 0
c
c...Define form layout
c
      FRMBEG = 7
      FRMNFD = 14
      FRMREC = 9
      FRMWID = 77
c
c...Motif - Store total number of fields in form
c
      if (IMSCAN .eq. 1) FRMWID = 126
c
      FRMST(1) = 1
      FRMST(2) = 8
      FRMST(3) = 14
      FRMST(4) = 17
      FRMST(5) = 24
      FRMST(6) = 30
      FRMST(7) = 33
      FRMST(8) = 40
      FRMST(9) = 46
      FRMST(10) = 49
      FRMST(11) = 56
      FRMST(12) = 62
      FRMST(13) = 65
      FRMST(14) = 72
c
      FRMEN(1) = 6
      FRMEN(2) = 13
      FRMEN(3) = 16
      FRMEN(4) = 22
      FRMEN(5) = 29
      FRMEN(6) = 32
      FRMEN(7) = 38
      FRMEN(8) = 45
      FRMEN(9) = 48
      FRMEN(10) = 54
      FRMEN(11) = 61
      FRMEN(12) = 64
      FRMEN(13) = 70
      FRMEN(14) = 77
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 4
      FRMTYP(2) = 4
      FRMTYP(3) = 3
      FRMTYP(4) = 4
      FRMTYP(5) = 4
      FRMTYP(6) = 3
      FRMTYP(7) = 4
      FRMTYP(8) = 4
      FRMTYP(9) = 3
      FRMTYP(10) = 4
      FRMTYP(11) = 4
      FRMTYP(12) = 3
      FRMTYP(13) = 4
      FRMTYP(14) = 4
c
c...Set up form records
c
      ipt1   = (ksw-1) * 45
      imax   = ipt1   + NFEDTB(ksw)
      do 300 i=1,FRMREC,1
          FRMFNC(3,i) = 2
          FRMFNC(6,i) = 2
          FRMFNC(9,i) = 2
          FRMFNC(12,i) = 2
          sbuf   = '      =       |       =       |       =       |' //
     1             '       =       |       =       '
          inc    = 1
          do 200 j=1,15,3
c
c......Feedrate code
c
              ipt1   = ipt1   + 1
              if (ipt1 .gt. imax) go to 350
              call rtoc (FEDTCD(ipt1),abuf,FRMFNC(j,i))
              ist    = FRMST(j)
              ien    = FRMST(j) + FRMFNC(j,i) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j,i))
c
c......Feedrate speed
c
              rnum   = FEDTVL(ipt1)
              nmax   = FRMEN(j+1) - FRMST(j+1) + 1
              call dpoint (rnum,rnum,nmax-2)
              call rtoc (rnum,abuf,nc)
              if (nc .gt. nmax) nc = nmax
              FRMFNC(j+1,i) = nc
              ist    = FRMST(j+1)
              ien    = FRMST(j+1) + FRMFNC(j+1,i) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j+1,i))
  200     continue
          FRMBUF(i) = sbuf
  300 continue
      go to 400
c
c...Zero out rest of form
c
  350 do 380 i=i,FRMREC,1
          FRMFNC(3,i) = 2
          FRMFNC(6,i) = 2
          FRMFNC(9,i) = 2
          FRMFNC(12,i) = 2
          do 370 j=j,15,3
              FRMFNC(j,i) = 0
              FRMFNC(j+1,i) = 0
  370     continue
          j      = 1
          FRMBUF(i) = sbuf
          sbuf   = '      =       |       =       |       =       |' //
     1             '       =       |       =       '
  380 continue
c
c...Go get form input from Motif
c
      if (IMSCAN .eq. 1) then
          IMTYP  = 4
          go to 8000
      endif
c
c...Display the form header
c
  400 if (ksw .eq. 1) then
          call getsap ('FEDFPM',FRMHPT(1),IPRMDS,SALABL)
      else
          call getsap ('FEDFPR',FRMHPT(1),IPRMDS,SALABL)
      endif
      call getsap ('BLANK',FRMHPT(2),IPRMDS,SALABL)
      call getsap ('FEDTB2',FRMHPT(3),IPRMDS,SALABL)
      NFRMHD = 3
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
c      call plott (5,1)
c      call dmpbuf (SAPRM(ipt2),SAPNC(ipt2))
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 ipt1   = 0
      do 700 i=1,FRMREC,1
          sbuf   = FRMBUF(i)
c
c......Get number of chars in each field
c......for Motif interface
c
          if (IMSCAN .eq. 2) then
              do 550 j=1,FRMNFD,1
                  FRMFNC(j,i) = strlen1(sbuf(FRMST(j):FRMEN(j)))
  550         continue
          endif
c
c......Feedrate code
c
          do 600 j=1,15,3
              inc    = j      + 1
              if (FRMFNC(j,i) .eq. 0) then
                  if (FRMFNC(inc,i) .ne. 0) go to 9000
              else
                  ipt1   = ipt1   + 1
                  ist    = FRMST(j)
                  ien    = FRMST(j) + FRMFNC(j,i) - 1
                  call ctor (sbuf(ist:ien),rcd(ipt1),ierr)
                  if (ierr .ne. 0) go to 9000
c
c......Spindle speed
c
                  if (FRMFNC(inc,i) .eq. 0) go to 9000
                  ist    = FRMST(inc)
                  ien    = FRMST(inc) + FRMFNC(inc,i) - 1
                  call ctor (sbuf(ist:ien),rvl(ipt1),ierr)
                  if (ierr .ne. 0) go to 9000
              endif
  600     continue
  700 continue
c
c...Sort table
c
  800 isw    = 0
      do 1000 i=1,ipt1-1,1
          if (rvl(i) .gt. rvl(i+1)) then
              rnum   = rcd(i)
              rcd(i) = rcd(i+1)
              rcd(i+1) = rnum
c
              rnum   = rvl(i)
              rvl(i) = rvl(i+1)
              rvl(i+1) = rnum
              isw    = 1
          endif
 1000 continue
      if (isw .eq. 1) go to 800
c
      NFEDTB(ksw) = ipt1
      ipt1   = (ksw-1) * 45
      do 1200 i=1,NFEDTB(ksw),1
          FEDTCD(ipt1+i) = rcd(i)
          FEDTVL(ipt1+i) = rvl(i)
 1200 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      if (FRMFNC(j,i) .eq. 0) then
          abuf   = ' '
      else
          ist    = FRMST(j)
          ien    = FRMST(j) + FRMFNC(j,i) - 1
          abuf   = sbuf(ist:ien)
      endif
      call errstr (ebuf,abuf,0)
c
      j      = j      + 1
      if (FRMFNC(j,i) .eq. 0) then
          abuf   = ' '
      else
          ist    = FRMST(j)
          ien    = FRMST(j) + FRMFNC(j,i) - 1
          abuf   = sbuf(ist:ien)
      endif
      call errstr (ebuf,abuf,0)
c
c......Display the error message
c
      ibeg   = 1
      call plott (FRMBEG,1)
      call clreos
      call errmsg (ebuf,2,2)
      if (IMSCAN .eq. 2) go to 8000
      go to 500
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frprap (kfl,cmsg,kerr)
c
c   FUNCTION:  Rapid setup prompt handling routine.
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
      subroutine frprap (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (LTHPCL,KPOSMP(1899))
      equivalence (IFDFRC,KPOSMP(3153)), (IRPSUP,KPOSMP(3189))
      equivalence (NRAPCD,KPOSMP(3190)), (RAPCD ,KPOSMP(3191))
      equivalence (IRPALT,KPOSMP(3196)), (IRPTAX,KPOSMP(3197))
      equivalence (IRPMOD,KPOSMP(3198)), (LNCRAP,KPOSMP(3212))
      equivalence (IRPRET,KPOSMP(3219)), (IRPSMD,KPOSMP(3242))
      equivalence (IRFSUP,KPOSMP(4136)), (IRDSUP,KPOSMP(4137))
      equivalence (FRAPCD,KPOSMP(4138)), (DRAPCD,KPOSMP(4139))
c
      integer*4 IFDFRC(4),IRPSUP,NRAPCD,RAPCD(5),IRPALT,IRPTAX,
     -          IRPMOD,LNCRAP,IRPRET,FRAPCD,DRAPCD,IRFSUP,IRDSUP,
     -          LTHPCL(2),MACHTP,IRPSMD
c
      equivalence (RAPVL ,POSMAP(3577)), (FRAPVL,POSMAP(4882))
      equivalence (DRAPVL,POSMAP(4883))
c
      real*8 RAPVL(5),FRAPVL,DRAPVL
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),i,iary(5),inum
c
      real*8 rary(5)
c
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,350,400,450,500,600,650,700,800,900,
     -       1000,1400), ist
c
c...Is rapid mode supported
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = IRPSUP
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPSUP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Codes for setting rapid mode
c
  200 MLEVL(NLEVL) = 2
      if (IRPSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      do 220 i=1,NRAPCD,1
          iary(i) = RAPCD(i)
          rary(i) = RAPVL(i)
  220 continue
      nc     = NRAPCD
      call prmcod (iary,rary,nc,5,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          do 240 i=1,nc,1
              RAPCD(i) = iary(i)
              RAPVL(i) = rary(i)
  240     continue
          NRAPCD = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Is rapid mode supported in LMFP mode
c
  300 MLEVL(NLEVL) = 3
      if (MACHTP .ne. 4 .or. MACHTP .eq. 4 .and.
     -    LTHPCL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRFSUP
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRFSUP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Codes for setting rapid mode
c
  350 MLEVL(NLEVL) = 4
      if (IRFSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      iary(1) = FRAPCD
      rary(1) = FRAPVL
      nc     = 1
      call prmcod (iary,rary,nc,5,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FRAPCD = iary(1)
          FRAPVL = rary(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is rapid mode supported in LMDP mode
c
  400 MLEVL(NLEVL) = 5
      if (MACHTP .ne. 4 .or. MACHTP .eq. 4 .and.
     -    LTHPCL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 500
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRDSUP
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRDSUP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Codes for setting rapid mode
c
  450 MLEVL(NLEVL) = 6
      if (IRDSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 500
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      iary(1) = DRAPCD
      rary(1) = DRAPVL
      nc     = 1
      call prmcod (iary,rary,nc,5,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DRAPCD = iary(1)
          DRAPVL = rary(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Linear interpolation code with rapid code
c
  500 MLEVL(NLEVL) = 7
      if (IRPSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 515 i=1,NRAPCD
          if (RAPCD(i) .eq. -1 .or. (RAPCD(i) .ge. 18 .and.
     -        RAPCD(i) .le. 28)) go to 595
  515 continue
      iwrn   = 0
      inum   = LNCRAP
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LNCRAP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Reset feed rate after rapid
c
  600 MLEVL(NLEVL) = 8
      if (IRPSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 650
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IFDFRC(3)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDFRC(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Reset feed rate mode after rapid
c
  650 MLEVL(NLEVL) = 9
      if (IRPSUP .ne. 1) then
          if (kfl .lt. NLEVL) go to 700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IFDFRC(4)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDFRC(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Alter motion for rapid moves
c
  700 MLEVL(NLEVL) = 10
      iwrn   = 0
      inum   = IRPALT
      nc     = -1
      call prmint (inum,nc,1,1,5,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPALT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract/plunge along tool axis
c
  800 MLEVL(NLEVL) = 11
      if (IRPALT .ne. 5) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRPTAX
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPTAX = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Retract/plunge along tool axis
c
  900 MLEVL(NLEVL) = 12
      iwrn   = 0
      inum   = IRPRET
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPRET = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Rapid ABS/INCR mode
c
 1000 MLEVL(NLEVL) = 13
      iwrn   = 0
      inum   = IRPMOD
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPMOD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Use RAPID positioning move during simulation
c
 1400 MLEVL(NLEVL) = 14
      iwrn   = 0
      inum   = IRPSMD
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRPSMD = inum
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
c   SUBROUTINE:  frpreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Feedrate register limits prompt handling routine.
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
      subroutine frpreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (IFDSUP,KPOSMP(3111)), (IFDEXD,KPOSMP(3178))
c
      integer*4 IFDSUP(4),IFDEXD(4)
c
      equivalence (FDLCNV,POSMAP(3013)), (FEDRNG,POSMAP(3512))
c
      real*8 FEDRNG(2,8),FDLCNV
c
      integer*4 nc,iwrn,ilod,i,ist,inc,isub(8)
c
      real*8 rnum(2),rmn(2),rmx(2),rmn1,rmx1
c
      data rmn /-99999999.d0,-99999999d0/
      data rmx /99999999.d0,99999999.d0/
      data rmn1 /.001/, rmx1 /1000./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,100,100,100,100,100,700), ist
c
c...Get Feedrate register value limits
c
  100 do 120 i=1,4,1
          isub(i) = IFDSUP(i)
          isub(i+4) = IFDEXD(i)
  120 continue
      do 190 i=ist,6,1
          inc    = i
          if (i .eq. 6) inc = 8
          MLEVL(NLEVL) = i
          if (isub(inc) .ne. 1) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          rnum(1) = FEDRNG(1,inc)
          rnum(2) = FEDRNG(2,inc)
          iwrn   = 0
          nc     = 2
  150     call prmrel (rnum,nc,2,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
              if (nc .ne. 2 .or. rnum(1) .gt. rnum(2)) then
                  iwrn   = 1
                  call errmsg ('INVRSP',1,2)
                  if (IMSCAN .eq. 2) go to 8000
                  go to 150
              endif
c
              FEDRNG(1,inc) = rnum(1)
              FEDRNG(2,inc) = rnum(2)
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Units conversion factor for regsiter limits
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      rnum(1) = FDLCNV
      nc     = 1
      call prmrel (rnum(1),nc,1,rmn1,rmx1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FDLCNV = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
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
c   SUBROUTINE:  frplmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Maximum axis feedrates prompt handling routine.
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
      subroutine frplmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IRPSUP,KPOSMP(3189)), (IFDADJ,KPOSMP(3243))
c
      integer*4 NUMLIN(3),IRTNUM,IRPSUP,IFDADJ
c
      equivalence (FEDLMT,POSMAP(3528)), (RDRLMT,POSMAP(3538))
      equivalence (RAPLMT,POSMAP(3567))
c
      real*8 FEDLMT(10),RDRLMT,RAPLMT(10)
c
      integer*4 nc,iwrn,ilod,i,ist,inc,inum,ipt,iyesno(2)
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
      data rmn /0.d0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,1110,1110,1110,
     1       1110,1110,1110,1110,1110,1110,1110,2100,2200), MLEVL(NLEVL)
c
c...Get axis maximum feedrates
c
  100 ist    = 1
  110 do 190 i=ist,10,1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (i .le. 6) then
              inc    = (i-1) / 2 + 1
              inum   = 2 + (i - inc*2)
              if (NUMLIN(inc) .lt. inum) iwrn = 1
c
          else
              if (i-6 .gt. IRTNUM) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          rnum   = FEDLMT(i)
          iwrn   = 0
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              FEDLMT(i) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Get axis rapid rates
c
 1100 ist    = 11
 1110 inc    = ist    - 11
c     if (IRPSUP .ne. 1) then
c         if (kfl .lt. NLEVL) go to 1195
c         call errmsg ('NOTAPPLY',1,2)
c         go to 8000
c     endif
c
      do 1190 i=ist,20,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (inc .le. 6) then
              ipt    = (inc-1) / 2 + 1
              inum   = 2 + (inc - ipt*2)
              if (NUMLIN(ipt) .lt. inum) iwrn = 1
c
          else
              if (inc-6 .gt. IRTNUM) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 1190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          rnum   = RAPLMT(inc)
          iwrn   = 0
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RAPLMT(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
 1195 continue
c
c...Minimum time allowed for move
c
 2100 MLEVL(NLEVL) = 21
      iwrn   = 0
      rnum   = RDRLMT * 60.d0
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RDRLMT = rnum   / 60.d0
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract/plunge along tool axis
c
 2200 MLEVL(NLEVL) = 22
      iwrn   = 0
      if (IFDADJ .eq. 0) IFDADJ = 1
      inum   = IFDADJ
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IFDADJ = inum
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
c   SUBROUTINE:  frpacl (kfl,cmsg,kerr)
c
c   FUNCTION:  Acceleration/Deceleration time calculation prompt handling
c              routine.
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
      subroutine frpacl (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IACLFL,KPOSMP(1732))
c
      integer*4 NUMLIN(3),IRTNUM,IACLFL
c
      equivalence (ACLDCL,POSMAP(2099))
c
      real*8 ACLDCL(10)
c
      integer*4 nc,iwrn,ilod,i,ist,inc,inum,ipt,iyesno(2)
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
      data rmn /0.d0/, rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,210,210,210,210,210,210,210,210,210,210), MLEVL(NLEVL)
c
c...Retract/plunge along tool axis
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = IACLFL
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IACLFL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is accel/decel time adjustments allowed
c
  200 ist    = 2
  210 inc    = ist    - 2
      if (IACLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 290 i=ist,11,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
c
c......Verify that this machine has this axis
c
          if (inc .le. 6) then
              ipt    = (inc-1) / 2 + 1
              inum   = 2 + (inc - ipt*2)
              if (NUMLIN(ipt) .lt. inum) iwrn = 1
c
          else
              if (inc-6 .gt. IRTNUM) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          rnum   = ACLDCL(inc)
          iwrn   = 0
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ACLDCL(inc) = rnum
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
