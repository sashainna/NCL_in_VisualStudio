c
c***********************************************************************
c
c   FILE NAME:  spncol.for
c   CONTAINS:
c               spncol  scospn  scoreg  scotbl  scocol
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        spncol.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:58
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  spncol (kfl,cmsg,kerr)
c
c   FUNCTION:  Spindle & coolant menu handling routine.
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
      subroutine spncol (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NSPRG ,KPOSMP(3101)), (SPNTYP,KPOSMP(3102))
c
      integer*4 NSPRG,SPNTYP
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
c...Display Spindle & Coolant menu
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
c...Walk through Machine Configuration
c
 1000 IWALK  = 1
c
c...Spindle set-up
c
 1100 MLEVL(NLEVL-1) = 1
      call scospn (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Spindle registers
c
 1200 MLEVL(NLEVL-1) = 2
      call scoreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Low range spindle table
c
 1300 if (SPNTYP .ne. 3 .or. NSPRG .eq. 1) then
          if (IWALK .eq. 1) go to 1400
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 3
      call scotbl (kfl,1,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Medium range spindle table
c
 1400 if (SPNTYP .ne. 3 .or. NSPRG .eq. 2) then
          if (IWALK .eq. 1) go to 1500
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 4
      call scotbl (kfl,2,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...High range spindle table
c
 1500 if (SPNTYP .ne. 3 .or. NSPRG .eq. 1) then
          if (IWALK .eq. 1) go to 1600
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 5
      call scotbl (kfl,3,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Coolant setup
c
 1600 MLEVL(NLEVL-1) = 6
      call scocol (kfl,cmsg,kerr)
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
c   SUBROUTINE:  scospn (kfl,cmsg,kerr)
c
c   FUNCTION:  Spindle setup prompt handling routine.
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
      subroutine scospn (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNTYP,KPOSMP(3102))
      equivalence (SPNSFM,KPOSMP(3103)), (SPNRCD,KPOSMP(3115))
      equivalence (SPNOCD,KPOSMP(3121))
      equivalence (SPNSCD,KPOSMP(3144)), (SPNXCD,KPOSMP(3147))
c
      integer*4 NSPRG,SPNTYP,SPNSFM,SPNOCD(2),SPNSCD(2),SPNXCD,
     1          SPNRCD(3),MACHTP
c
      equivalence (MAXSFM,POSMAP(3304))
      equivalence (SPNLMT,POSMAP(3309)), (SFMTOL,POSMAP(3317))
      equivalence (SPMLMT,POSMAP(4930))
c
      real*8 MAXSFM,SPNLMT(2,4),SFMTOL,SPMLMT(2,4)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),isub(4),inum,inc
c
      real*8 rnum(2),rmn(2),rmx(2)
c
      data iyesno /13,12/
      data rmn /0.,0./, rmx /99999999.d0,99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,410,410,410,410,800,910,910,910), MLEVL(NLEVL)
c
c...Spindle speed output format
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = SPNTYP
      nc     = -1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNTYP  = inum
          if (SPNTYP .eq. 1) then
              NSPRG  = 1
              SPNSFM = 2
              SPNOCD(1) = 0
              SPNOCD(2) = 0
              SPNRCD(1) = 0
              SPNRCD(2) = 0
              SPNRCD(3) = 0
              SPNSCD(1) = 0
              SPNSCD(2) = 0
              SPNXCD = 0
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...SFM support
c
  200 if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = SPNSFM
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNSFM = inum
          if (SPNSFM .eq. 0) then
              SPNSCD(2) = 0
              SPNXCD = 0
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Number of spindle ranges
c
  300 if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = NSPRG
      nc     = -1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NSPRG   = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Spindle speed limits
c
  400 ist    = 4
  410 inc    = ist    - 4
      isub(1) = 1
      isub(2) = 1
      isub(3) = 1
      isub(4) = 1
      if (NSPRG .eq. 1) then
          isub(1) = 0
          isub(3) = 0
      else if (NSPRG .eq. 2) then
          isub(2) = 0
      endif
      if (SPNSFM .eq. 2) isub(4) = 0
      do 490 i=ist,7,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 490
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum(1) = SPNLMT(1,inc)
          rnum(2) = SPNLMT(2,inc)
          nc     = 2
          call prmrel (rnum,nc,2,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (nc .ge. 1) SPNLMT(1,inc) = rnum(1)
              if (nc .ge. 2) then
                  SPNLMT(2,inc) = rnum(2)
                  if (inc .ne. 4) MAXSFM = rnum(2)
              endif
              if (kfl .ge. NLEVL) go to 8000
          endif
  490 continue
c
c...RPM in SFM mode tolerance
c
  800 if (SPNTYP .eq. 1 .or. SPNSFM .eq. 1) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 8
      iwrn   = 0
      rnum(1) = SFMTOL
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SFMTOL = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
  895 continue
c
c...Mill head spindle speed limits
c
  900 if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      ist    = 9
  910 inc    = ist    - 9
      isub(1) = 1
      isub(2) = 1
      isub(3) = 1
      if (NSPRG .eq. 1) then
          isub(1) = 0
          isub(3) = 0
      else if (NSPRG .eq. 2) then
          isub(2) = 0
      endif
      do 990 i=ist,11,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 990
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum(1) = SPMLMT(1,inc)
          rnum(2) = SPMLMT(2,inc)
          nc     = 2
          call prmrel (rnum,nc,2,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (nc .ge. 1) SPMLMT(1,inc) = rnum(1)
              if (nc .ge. 2) then
                  SPMLMT(2,inc) = rnum(2)
              endif
              if (kfl .ge. NLEVL) go to 8000
          endif
  990 continue
  995 continue
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
c   SUBROUTINE:  scoreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Spindle registers prompt handling routine.
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
      subroutine scoreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MUSRBK,KPOSMP(3022)), (NSPRG ,KPOSMP(3101))
      equivalence (SPNTYP,KPOSMP(3102)), (SPNSFM,KPOSMP(3103))
      equivalence (SPNDCD,KPOSMP(3105)), (SPNBCD,KPOSMP(3109))
      equivalence (SPNRCD,KPOSMP(3115)), (SPNFCD,KPOSMP(3118))
      equivalence (SPNOCD,KPOSMP(3121)), (SPNBLK,KPOSMP(3123))
      equivalence (SPNOFF,KPOSMP(3124)), (SPNSCD,KPOSMP(3144))
      equivalence (SPNXCD,KPOSMP(3147))
c
      integer*4 MUSRBK,NSPRG,SPNTYP,SPNSFM,SPNDCD(4),SPNBCD,
     1          SPNRCD(3),SPNFCD(3),SPNOCD(2),SPNBLK,SPNOFF,SPNSCD(2),
     2          SPNXCD
c
      equivalence (DUMMY ,POSMAP(0003)), (SPNDVL,POSMAP(3007))
      equivalence (SPNBVL,POSMAP(3011)), (SPNRVL,POSMAP(3016))
      equivalence (SPNFVL,POSMAP(3019)), (SPNOVL,POSMAP(3022))
c
      real*8 DUMMY,SPNDVL(4),SPNBVL,SPNRVL(3),SPNFVL(3),SPNOVL(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),isub(3),inc,inum
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
      go to (110,110,310,310,310,310,700,810,810,810,1110,1110,
     1       1110,1400,1510,1510,1700,1800), MLEVL(NLEVL)
c
c...Spindle speed register
c
  100 ist    = 1
  110 inc    = ist    - 1
      if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 190 i=ist,2,1
          inc    = inc    + 1
          if (inc .eq. 2 .and. SPNSFM .eq. 2) then
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPNSCD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPNSCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Spindle direction codes
c
  300 ist    = 3
  310 inc    = ist    - 3
      do 390 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPNDCD(inc)
          rnum   = SPNDVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPNDCD(inc) = inum
              SPNDVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  390 continue
c
c...Spindle BOTH codes
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = SPNBCD
      rnum   = SPNBVL
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNBCD = inum
          SPNBVL = rnum
          if (kfl .ge. NLEVL) go to 8000
       endif
c
c..Spindle range codes
c
  800 ist    = 8
  810 if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      inc    = ist    - 8
      isub(1) = 1
      isub(2) = 1
      isub(3) = 1
      if (NSPRG .eq. 1) then
          isub(1) = 0
          isub(3) = 0
      else if (NSPRG .eq. 2) then
          isub(2) = 0
      endif
      do 890 i=ist,10,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 890
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPNRCD(inc)
          rnum   = SPNRVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPNRCD(inc) = inum
              SPNRVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  890 continue
  895 continue
c
c..RPM/SFM spindle codes
c
 1100 ist    = 11
 1110 inc    = ist    - 11
      if (SPNSFM .ne. 1) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1190 i=ist,13,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPNFCD(inc)
          rnum   = SPNFVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPNFCD(inc) = inum
              SPNFVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
 1195 continue
c
c...SFM spindle radius register
c
 1400 MLEVL(NLEVL) = 14
      if (SPNSFM .eq. 2) then
          if (kfl .lt. NLEVL) go to 1495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = SPNXCD
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNXCD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1495 continue
c
c..Spindle override codes
c
 1500 ist    = 15
 1510 if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 1595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      inc    = ist    - 15
      do 1590 i=ist,16,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPNOCD(inc)
          rnum   = SPNOVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPNOCD(inc) = inum
              SPNOVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1590 continue
 1595 continue
c
c...User defined spindle/on block
c
 1700 MLEVL(NLEVL) = 17
      iwrn   = 0
      inum   = SPNBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Spindle/off in block by itself
c
 1800 MLEVL(NLEVL) = 18
      iwrn   = 0
      inum   = SPNOFF
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SPNOFF = inum
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
c   SUBROUTINE:  scotbl (kfl,ksw,cmsg,kerr)
c
c   FUNCTION:  Spindle table definition form handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.  If kfl is greater than or equal
c                               to the current level, then the user re-
c                               quested direct access to a menu/prompt.
c
c           ktb     I*4  D1  -  1 = Low range spindle table.  2 = Medium
c                               range.  3 = High range.
c
c   OUTPUT: cmsg    C*n  D1  -  Text of error message.
c
c           kerr    I*4  D1  -  Returns non-zero when an error occurred.
c
c***********************************************************************
c
      subroutine scotbl (kfl,ksw,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NSPNTB,KPOSMP(3125))
c
      integer*4 NSPNTB(3)
c
      equivalence (SPNTCD,POSMAP(3024)), (SPNTVL,POSMAP(3159))
c
      real*8 SPNTCD(135),SPNTVL(135)
c
      integer*4 kfl,ksw,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ist,ien,ipt1,ibeg,ierr,inc,isw,imax,strlen1
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
c
c...Set up form records
c
      ipt1   = (ksw-1) * 45
      imax   = ipt1   + NSPNTB(ksw)
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
c......Spindle code
c
              ipt1   = ipt1   + 1
              if (ipt1 .gt. imax) go to 350
              call rtoc (SPNTCD(ipt1),abuf,FRMFNC(j,i))
              ist    = FRMST(j)
              ien    = FRMST(j) + FRMFNC(j,i) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j,i))
c
c......Spindle speed
c
              call rtoc (SPNTVL(ipt1),abuf,FRMFNC(j+1,i))
c
c...only allow FRMST(j+2) - FRMST(j+) chars for this field
c...Yurong 1/27/03
c
              if (( FRMFNC(j+1,i) .gt. FRMST(j+2) - FRMST(j+1))
     1              .and. (j .lt. 13) ) then
                  FRMFNC(j+1,i) = FRMST(j+2) - FRMST(j+1)
              endif
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
          call getsap ('SPNLRG',FRMHPT(1),IPRMDS,SALABL)
      else if (ksw .eq. 2) then
          call getsap ('SPNMRG',FRMHPT(1),IPRMDS,SALABL)
      else
          call getsap ('SPNHRG',FRMHPT(1),IPRMDS,SALABL)
      endif
      call getsap ('BLANK',FRMHPT(2),IPRMDS,SALABL)
      call getsap ('SPNRG2',FRMHPT(3),IPRMDS,SALABL)
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
c......Spindle code
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
      NSPNTB(ksw) = ipt1
      ipt1   = (ksw-1) * 45
      do 1200 i=1,NSPNTB(ksw),1
          SPNTCD(ipt1+i) = rcd(i)
          SPNTVL(ipt1+i) = rvl(i)
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
c   SUBROUTINE:  scocol (kfl,cmsg,kerr)
c
c   FUNCTION:  Coolant set-up prompt handling routine.
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
      subroutine scocol (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (SPNTYP,KPOSMP(3102))
      equivalence (SPNDCD,KPOSMP(3105)), (COOLCD,KPOSMP(3128))
      equivalence (SPCOCD,KPOSMP(3132)), (COLBLK,KPOSMP(3138))
c
      integer*4 SPNDCD(4),COOLCD(4),SPCOCD(6),COLBLK(2),SPNTYP
c
      equivalence (COOLVL,POSMAP(3294)), (SPCOVL,POSMAP(3298))
c
      real*8 COOLVL(4),SPCOVL(6)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,i,iyesno(2),isub(6),inc,inum
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
      go to (110,110,110,110,510,510,510,510,510,510,1110,1110),
     1           MLEVL(NLEVL)
c
c...Coolant codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = COOLCD(inc)
          rnum   = COOLVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              COOLCD(inc) = inum
              COOLVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c..Spindle range codes
c
  500 ist    = 5
  510 if (SPNTYP .eq. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      inc    = ist    - 5
      isub(1) = 1
      isub(2) = 1
      isub(3) = 1
      isub(4) = 1
      isub(5) = 1
      isub(6) = 1
c
      if (SPNDCD(1) .eq. 0) then
          isub(1) = 0
          isub(3) = 0
          isub(5) = 0
      endif
c
      if (SPNDCD(2) .eq. 0) then
          isub(2) = 0
          isub(4) = 0
          isub(6) = 0
      endif
c
cc      if (COOLCD(1) .eq. 0) then
cc          isub(1) = 0
cc          isub(2) = 0
cc      endif
c
cc      if (COOLCD(2) .eq. 0) then
cc          isub(3) = 0
cc          isub(4) = 0
cc      endif
c
cc      if (COOLCD(3) .eq. 0) then
cc          isub(5) = 0
cc          isub(6) = 0
cc      endif
c
      do 590 i=ist,10,1
          inc    = inc    + 1
          if (isub(inc) .eq. 0) then
              if (kfl .lt. NLEVL) go to 590
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SPCOCD(inc)
          rnum   = SPCOVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SPCOCD(inc) = inum
              SPCOVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  590 continue
  595 continue
c
c...Coolant in block by itself
c
 1100 ist    = 11
 1110 inc    = ist    - 11
      do 1190 i=ist,12,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          inum   = COLBLK(inc)
          nc     = 1
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              COLBLK(inc) = inum
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
