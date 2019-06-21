c
c***********************************************************************
c
c   FILE NAME:  motblk.for
c   CONTAINS:
c               motblk  mbkspc  mbkrot  mbkclm  mbktol  mbktrn  mbkreg
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        motblk.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 09:21:49
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  motblk (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion Block Parameters menu handling routine.
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
      subroutine motblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
c
      integer*4 IRTNUM
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
c...Display Motion Block menu
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
c...Walk through Motion Block
c
 1000 IWALK  = 1
c
c...Special events
c
 1100 MLEVL(NLEVL-1) = 1
      call mbkspc (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Rotary motion
c
 1200 if (IRTNUM .eq. 0 .and. IJKROT .eq. 2) then
          if (IWALK .eq. 1) go to 1300
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 2
      call mbkrot (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Clamping logic
c
 1300 if (IRTNUM .eq. 0) then
          if (IWALK .eq. 1) go to 1400
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 3
      call mbkclm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Axes tolerances
c
 1400 MLEVL(NLEVL-1) = 4
      call mbktol (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Default axes translations
c
 1500 MLEVL(NLEVL-1) = 5
      call mbktrn (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Motion block register definitions
c
 1600 MLEVL(NLEVL-1) = 6
      call mbkreg (kfl,cmsg,kerr)
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
c   SUBROUTINE:  mbkspc (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion block special events.
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
      subroutine mbkspc (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (FRMFLG,KPOSMP(1211))
      equivalence (LTHXY ,KPOSMP(1225))
      equivalence (INCR  ,KPOSMP(1226)), (INCRFC,KPOSMP(1227))
      equivalence (LTHDIA,KPOSMP(1228)), (IRTNUM,KPOSMP(1243))
      equivalence (LTHXD ,KPOSMP(1339)), (MLNFRC,KPOSMP(1373))
      equivalence (IJKROT,KPOSMP(1739)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MACHTP,FRMFLG,LTHDIA(2),LTHXY,INCR,INCRFC,IRTNUM,
     -          LTHXD,MTPDYN,MLNFRC,IJKROT
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,i,ist,inc,iyesno(2),ipos(2),iradm(2)
c
      data iyesno /13,12/, ipos /30,31/, iradm /38,39/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,300,400,500,600,800), MLEVL(NLEVL)
c
c...Get FROM statement handling
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = FRMFLG
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FRMFLG = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Double X-axis output
c
  200 ist    = 2
  210 inc    = ist    - 2
      if (MTPDYN .ne. 2) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 290 i=ist,3,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = LTHDIA(inc)
          call prmvoc (inum,iradm,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (inc .eq. 2) call limadj (inum,LTHDIA(inc))
              LTHDIA(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
  295 continue
c
c...Reverse X-axis direction
c
  300 MLEVL(NLEVL) = 4
      if (MTPDYN .ne. 2) then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LTHXD
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LTHXD  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Program lathe in XY plan
c
  400 MLEVL(NLEVL) = 5
      if (MTPDYN .ne. 2) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LTHXY
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LTHXY  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Default positioning mode
c
  500 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = INCR
      call prmvoc (inum,ipos,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          INCR   = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Force position when changing
c...from to Absolute mode
c
  600 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = INCRFC
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          INCRFC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Force all axes on motion block
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = MLNFRC
      nc     = 2
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MLNFRC = inum
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
c   SUBROUTINE:  mbkrot (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion block rotary motion.
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
      subroutine mbkrot (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (NOPIVS,KPOSMP(1282)), (MROTTV,KPOSMP(1334))
      equivalence (NOTABS,KPOSMP(1283)), (IRTSCL,KPOSMP(1288))
      equivalence (AXSINC,KPOSMP(1296))
      equivalence (IRTCLM,KPOSMP(1306)), (RSIZCD,KPOSMP(1328))
      equivalence (RSIZFL,KPOSMP(1333)), (MRTFRC,KPOSMP(1387))
      equivalence (ROTDCD,KPOSMP(1405)), (IRTYPE,KPOSMP(1486))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 IRTNUM,IRTYPE(20),NOPIVS,NOTABS,IRTSCL(4),AXSINC(10),
     2          IRTCLM,RSIZCD(5),RSIZFL,MROTTV,
     3          IJKROT,ROTDCD(2,4),MRTFRC
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (PPTOLR,POSMAP(1274)), (PPINCR,POSMAP(1443))
      equivalence (RSIZCV,POSMAP(1600)), (ROTDVL,POSMAP(2041))
      equivalence (ROTZER,POSMAP(4555))
c
      real*8 PPTOLR(10),PPINCR(10),ROTZER(2),DUMMY,RSIZCV,
     1       ROTDVL(2,4)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ipt(8),nc,iwrn,ilod,ist,i,iyesno(2),inum,isub(8),inc
c
      real*8 rnum,rmx
c
      data ipt /1,2,1,2,1,2,1,2/, isub /1,1,2,2,3,3,4,4/
      data iyesno /13,12/
c
      data rmx /99999999/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 60
   50 ist    = MLEVL(NLEVL)
      goto (60,100,200,250,310,310,310,310,710,710,
     1      1800,1900,2010,2010,2010,2010,2510,2510,
     2      2510,2510,2510,2510,2510,2510), MLEVL(NLEVL)
c
c...Use tool vector to get rotary coordinates
c
   60 if (IRTNUM .gt. 0) go to 70
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      go to 195
c
   70 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = MROTTV
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MROTTV = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
   95 continue
c
c...Adjust points for tables
c
  100 do 120 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 1) go to 140
  120 continue
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      go to 195
c
  140 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = NOTABS
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NOTABS = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  195 continue
c
c...Adjust points for pivots
c
  200 do 220 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 2) go to 240
  220 continue
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      go to 245
c
  240 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = NOPIVS
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NOPIVS = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  245 continue
c
c...Output all rotary axes on motion block
c
  250 if (IRTNUM .gt. 1 .or. IJKROT .eq. 1) go to 270
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      go to 295
c
  270 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = MRTFRC
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MRTFRC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Absolute/incremental mode for axes
c
  300 ist    = 5
  310 inc    = ist    - 5
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 395
         endif
      endif
      do 390 i=ist,8,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c......and this is a contouring axes output in degrees
c
          if (inc .gt. IRTNUM .or. IRTSCL(inc) .gt. 2) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              if (inc .gt. IRTNUM) go to 395
              go to 390
          endif
c
c......Get user input
c
          iwrn   = 0
          inum   = AXSINC(inc+6)
          nc     = 1
          call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              AXSINC(inc+6) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  390 continue
c
  395 continue
c
c...CCLW move to 0 value
c
  700 ist    = 9
  710 inc    = ist    - 9
      do 715 i=1,IRTNUM,1
          if (IRTSCL(i) .eq. 2) go to 720
  715 continue
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      else
          go to 795
      endif
c
  720 if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 795
         endif
      endif
c
      do 790 i=ist,10,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum   = ROTZER(inc)
          nc     = 1
          call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ROTZER(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  790 continue
  795 continue
c
c...Get change axis circumference code
c
 1800 MLEVL(NLEVL) = 11
      if (IRTNUM .eq. 0 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 1895
      endif
      iwrn   = 0
      inum   = RSIZCD(1)
      rnum   = RSIZCV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RSIZCD(1) = inum
          RSIZCV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1895 continue
c
c...Output rotary size change as ...
c
 1900 MLEVL(NLEVL) = 12
      if (IRTNUM .eq. 0 .or. RSIZCD(1) .eq. 0 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 1995
      endif
      iwrn   = 0
      inum   = RSIZFL
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RSIZFL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1995 continue
c
c...Get change circum registers for rotary axes
c
 2000 ist    = 13
c
 2010 if (IRTNUM .eq. 0 .or. RSIZCD(1) .eq. 0 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 2095
      endif
c
      inc    = ist      - 12
      do 2080 i=ist,16,1
          inc    = inc    + 1
          if (inc-1 .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 2095
          endif
          iwrn   = 0
          MLEVL(NLEVL) = i
          inum   = RSIZCD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RSIZCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2080 continue
 2095 continue
c
c...Get direction codes for rotary axes
c
 2500 ist    = 17
c
 2510 if (IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 2595
      endif
c
      inc    = ist      - 17
      do 2580 i=ist,24,1
          inc    = inc    + 1
          if (isub(inc) .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 2595
          endif
          iwrn   = 0
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = ROTDCD(ipt(inc),isub(inc))
          rnum   = ROTDVL(ipt(inc),isub(inc))
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ROTDCD(ipt(inc),isub(inc)) = inum
              ROTDVL(ipt(inc),isub(inc)) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2580 continue
 2595 continue
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
c   SUBROUTINE:  mbkclm (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion block clamping logic.
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
      subroutine mbkclm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IRTCLM,KPOSMP(1306)), (CLMPCD,KPOSMP(1307))
      equivalence (CLMPBK,KPOSMP(1327)), (IJKROT,KPOSMP(1739))
      equivalence (CLMPUB,KPOSMP(1757)), (MUSRBK,KPOSMP(3022))
c
      integer*4 IRTNUM,IRTCLM,CLMPCD(2,10),CLMPBK,IJKROT,CLMPUB(2,10),
     1          MUSRBK
c
      equivalence (CLMPVL,POSMAP(1454))
c
      real*8 CLMPVL(2,10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ipt(8),nc,iwrn,ilod,ist,i,iyesno(2),inum,isub(8),inc
c
      real*8 rnum
c
      data ipt /1,2,1,2,1,2,1,2/, isub /1,1,2,2,3,3,4,4/
      data iyesno /13,12/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210,210,210,1000,1110,1110,1110,
     1      1110,1110,1110,1110,1110), MLEVL(NLEVL)
c
c...Clamping logic for rotary axes
c
  100 MLEVL(NLEVL) = 1
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 200
         endif
      endif
      iwrn   = 0
      inum   = IRTCLM
      nc     = -1
      call prmint (inum,nc,1,1,5,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTCLM = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get clamping codes for rotary axes
c
  200 ist    = 2
  210 if (IRTCLM .eq. 1 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 295
      endif
c
      inc    = ist      - 2
      do 280 i=ist,9,1
          inc    = inc    + 1
          if (isub(inc) .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 295
          endif
          iwrn   = 0
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CLMPCD(ipt(inc),isub(inc)+6)
          rnum   = CLMPVL(ipt(inc),isub(inc)+6)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CLMPCD(ipt(inc),isub(inc)+6) = inum
              CLMPVL(ipt(inc),isub(inc)+6) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  280 continue
  295 continue
c
c...Clamping code in block by themselves
c
 1000 if (IRTCLM .eq. 1 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 1095
      endif
c
      MLEVL(NLEVL) = 10
      iwrn   = 0
      inum   = CLMPBK
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CLMPBK = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1095 continue
c
c...Get user defined blocks for clamping
c
 1100 ist    = 11
c
 1110 if (IRTCLM .eq. 1 .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 1195
      endif
c
      inc    = ist      - 11
      do 1180 i=ist,18,1
          inc    = inc    + 1
          if (isub(inc) .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 1195
          endif
          iwrn   = 0
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CLMPUB(ipt(inc),isub(inc)+6)
          nc     = 1
          call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CLMPUB(ipt(inc),isub(inc)+6) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1180 continue
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
c   SUBROUTINE:  mbklin (kfl,cmsg,kerr)
c
c   FUNCTION:  Rotary axes linearization prompt handling routine.
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
      subroutine mbklin (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (LRTRCT,KPOSMP(1278))
c
      integer*4 LRTRCT
c
      equivalence (IJKROT,POSMAP(1739)), (LNRTOL,POSMAP(2251))
      equivalence (RETDIS,POSMAP(2279)), (RETFED,POSMAP(3239))
c
      real*8 LNRTOL,RETDIS,RETFED(4),IJKROT
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,ilod,ist,i,iyesno(2),inum,inc
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
c
      data rmn /-99999999.d0/, rmx /99999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,410,410), MLEVL(NLEVL)
c
c...Default linearization tolerance
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      rnum   = LNRTOL
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LNRTOL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Automatic retract on longest route
c
  200 MLEVL(NLEVL) = 2
      if (LNRTOL .eq. 0.) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          else
              go to 295
          endif
      endif
      iwrn   = 0
      inum   = LRTRCT
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LRTRCT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Retract distance
c
  300 MLEVL(NLEVL) = 3
      if (LRTRCT .ne. 1 .or. LNRTOL .eq. 0.) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          else
              go to 395
          endif
      endif
c
      iwrn   = 0
      rnum   = RETDIS
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RETDIS = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Absolute/incremental mode for axes
c
  400 ist    = 4
  410 inc    = ist    - 4
      if (LRTRCT .ne. 1 .or. LNRTOL .eq. 0. .or. IJKROT .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          else
              go to 495
          endif
      endif
c
      do 490 i=ist,5,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum   = RETFED(inc)
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RETFED(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  490 continue
  495 continue
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
c   SUBROUTINE:  mbktol (kfl,cmsg,kerr)
c
c   FUNCTION:  Axes tolerances prompt handling routine.
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
      subroutine mbktol (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 NUMLIN(3),IRTNUM,IJKROT
c
      equivalence (PPTOLR,POSMAP(1274)), (PPINCR,POSMAP(1443))
      equivalence (PPMAXD,POSMAP(1584))
c
      real*8 PPTOLR(10),PPINCR(10),PPMAXD(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ipt(6),ilin(6),nc,iwrn,ilod,ist,i,inc
c
      real*8 rnum,rmx,rmn
c
      data ipt /1,2,1,2,1,2/, ilin /1,1,2,2,3,3/
c
      data rmn /-99999999/, rmx /99999999/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,110,110,110,110,110,110,2110,2110,2110,2110,
     3      2110,2110,2110,2110,2110,2110), MLEVL(NLEVL)
c
c...Get minimum increment
c...for each axis
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 180 i=ist,10,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .le. 6) then
              if (NUMLIN(ilin(inc)) .lt. ipt(inc)) iwrn = 1
          else
              if (inc-6 .gt. IRTNUM .or. IJKROT .eq. 1) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = PPINCR(inc)
          nc     = 1
          call prmrel (rnum,nc,1,.0D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PPINCR(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...Get minimum delta move to output
c...for each axis
c
 2100 ist    = 11
 2110 inc    = ist    - 11
      do 2180 i=ist,20,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .le. 6) then
              if (NUMLIN(ilin(inc)) .lt. ipt(inc)) iwrn = 1
          else
              if (inc-6 .gt. IRTNUM .or. IJKROT .eq. 1) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 2180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = PPTOLR(inc)
          nc     = 1
          call prmrel (rnum,nc,1,0.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PPTOLR(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2180 continue
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
c   SUBROUTINE:  mbktrn (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion block default axes translations.
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
      subroutine mbktrn (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 NUMLIN(3),IRTNUM,IJKROT
c
      equivalence (TRANAX,POSMAP(1320))
c
      real*8 TRANAX(20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ipt(6),ilin(6),nc,iwrn,ilod,ist,i,inc
c
      real*8 rnum,rmn,rmx
c
      data ipt /1,2,1,2,1,2/, ilin /1,1,2,2,3,3/
c
      data rmn /-99999999/, rmx /99999999/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,110,110,710,710,710,710,1110,1110,1110,
     1      1110,1110,1110,1710,1710,1710,1710), MLEVL(NLEVL)
c
c...Get default linear
c...TRANS/-AXIS value
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 180 i=ist,6,1
          inc    = inc    + 1
          if (NUMLIN(ilin(inc)) .lt. ipt(inc)) then
              if (kfl .lt. NLEVL) go to 180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = TRANAX(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TRANAX(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...Get default rotary
c...TRANS/-AXIS value
c
  700 ist    = 7
  710 inc    = ist    - 7
      do 780 i=ist,10,1
          inc    = inc    + 1
          if (inc .gt. IRTNUM .or. IJKROT .eq. 1) then
              if (kfl .lt. NLEVL) go to 780
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = TRANAX(inc+6)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TRANAX(inc+6) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  780 continue
c
c...Get default linear
c...TRANS/-AXIS value
c
 1100 ist    = 11
 1110 inc    = ist    - 11
      do 1180 i=ist,16,1
          inc    = inc    + 1
          if (NUMLIN(ilin(inc)) .lt. ipt(inc)) then
              if (kfl .lt. NLEVL) go to 1180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = TRANAX(inc+10)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TRANAX(inc+10) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1180 continue
c
c...Get default rotary
c...TRANS/SCALE,-AXIS value
c
 1700 ist    = 17
 1710 inc    = ist    - 17
      do 1780 i=ist,20,1
          inc    = inc    + 1
          if (inc .gt. IRTNUM .or. IJKROT .eq. 1) then
              if (kfl .lt. NLEVL) go to 1780
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = TRANAX(inc+16)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              TRANAX(inc+16) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1780 continue
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
c   SUBROUTINE:  mbkreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion block registers prompt handling routine.
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
      subroutine mbkreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (TAXCD ,KPOSMP(1081)), (NTAXCD,KPOSMP(1090))
      equivalence (MACHTP,KPOSMP(1201)), (LTHPCL,KPOSMP(1899))
      equivalence (MOTCOD,KPOSMP(1240)), (ABSCOD,KPOSMP(1241))
      equivalence (INCCOD,KPOSMP(1242)), (MUSRBK,KPOSMP(3022))
      equivalence (MOBLK ,KPOSMP(3023)), (NMOBLK,KPOSMP(3045))
      equivalence (LFLMCD,KPOSMP(4104)), (LDLMCD,KPOSMP(4105))
      equivalence (MILREG,KPOSMP(4106))
      equivalence (LFDLCD,KPOSMP(4107)), (LPCREG,KPOSMP(4108))
      equivalence (LTHREG,KPOSMP(4110)), (LTCREG,KPOSMP(4195))
      equivalence (LTPREG,KPOSMP(4201)), (MODCYL,KPOSMP(4204))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (ISRVAL,KPOSMP(4208))

c
      integer*4 MOTCOD,ABSCOD,INCCOD,MOBLK,MUSRBK,NMOBLK,LFLMCD,
     -          LFDLCD,LDLMCD,MACHTP,LTHPCL(2),LPCREG(2),LTHREG,
     2          LTCREG(3),LTPREG(2),MODCYL,LUNREG,ISRCYL,ISRVAL,
     3          MILREG,NTAXCD(3),TAXCD(3,3)
c
      equivalence (DUMMY ,POSMAP(0003)), (MOTCDV,POSMAP(1284))
      equivalence (TAXVL ,POSMAP(1173))
      equivalence (ABSCDV,POSMAP(1285)), (INCCDV,POSMAP(1286))
      equivalence (FLMCDV,POSMAP(4600)), (DLMCDV,POSMAP(4601))
      equivalence (GLMCDV,POSMAP(4602)), (TLMCDV,POSMAP(4880))
      equivalence (TLLCDV,POSMAP(4881))

c
      real*8 DUMMY,MOTCDV,ABSCDV,INCCDV,FLMCDV,DLMCDV,TAXVL(3,3),
     1       GLMCDV(2),TLMCDV,TLLCDV
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,i,j,ipt,iary(3),iyesno(2)
c
      real*8 rnum,rary(3)
c
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,400,500, 610, 610, 610,900,1000,1100,
     1      1210,1210,1400,1500,1600,1700,1800), MLEVL(NLEVL)
c
c...Get linear motion code
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = MOTCOD
      rnum   = MOTCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MOTCOD = inum
          MOTCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get linear motion code in LMFP mode
c
  200 if (MACHTP .eq. 4 .and. LTHPCL(1) .ne. 1 .or.
     -    MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 300
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = LFLMCD
      rnum   = FLMCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LFLMCD = inum
          FLMCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get linear motion code in LMDP mode
c
  300 if (MACHTP .eq. 4 .and. LTHPCL(2) .ne. 1 .or.
     -    MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = LDLMCD
      rnum   = DLMCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LDLMCD = inum
          DLMCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Absolute positioning code
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = ABSCOD
      rnum   = ABSCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ABSCOD = inum
          ABSCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Incremental positioning code
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = INCCOD
      rnum   = INCCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          INCCOD = inum
          INCCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...options moved from axis registers descriptors
c...Get codes for tool axis plane
c
  600 ist    = 6
  610 ipt    = ist    - 6
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 690
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 680 i=ist,8,1
          MLEVL(NLEVL) = i
          ipt    = ipt    + 1
          iwrn   = 0
          do 630 j=1,NTAXCD(ipt),1
              iary(j) = TAXCD(j,ipt)
              rary(j) = TAXVL(j,ipt)
  630     continue
          nc     = NTAXCD(ipt)
          call prmcod (iary,rary,nc,3,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              do 660 j=1,nc,1
                  TAXCD(j,ipt) = iary(j)
                  TAXVL(j,ipt) = rary(j)
  660         continue
              NTAXCD(ipt) = nc
              if (kfl .ge. NLEVL) go to 8000
          endif
  680 continue
  690 continue
c
c...LMFP/LMDP mode minimum span register
c
  900 MLEVL(NLEVL) = 9
      if ((MACHTP .eq. 4 .and.
     -    LTHPCL(1) .ne. 1 .and. LTHPCL(2) .ne. 1) .or.
     -    MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 1000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LFDLCD
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LFDLCD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...MOOD/MILL code set up
c
 1000 MLEVL(NLEVL) = 10
      if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 1100
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = MILREG
      rnum   = TLMCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MILREG = inum
          TLMCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...MOOD/LATHE code set up
c
 1100 MLEVL(NLEVL) = 11
      if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 1200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LTHREG
      rnum   = TLLCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LTHREG = inum
          TLLCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...MOOD/MILL polar/cylindrical interpolation code set up
c
 1200 ist    = 12
 1210 ipt   = ist - 12
      do 1250 i=ist,13
         ipt   = ipt + 1
         if (MACHTP .ne. 4 .or. LTHPCL(ipt) .ne. 1) then
            if (kfl .lt. NLEVL) go to 1250
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         endif
         MLEVL(NLEVL) = i
         iwrn   = 0
         inum   = LPCREG(ipt)
         rnum   = GLMCDV(ipt)
         nc     = 1
         call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
         if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
            if (iwrn .eq. 2) go to 7000
            if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
            LPCREG(ipt) = inum
            GLMCDV(ipt) = rnum
            if (kfl .ge. NLEVL) go to 8000
         endif
 1250 continue
c
c...Register for unit circle in cylindrical interpolation
c
 1400 MLEVL(NLEVL) = 14
      if (MACHTP .ne. 4 .or. MODCYL .eq. 0) then
          if (kfl .lt. NLEVL) go to 1500
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LUNREG
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
         if (iwrn .eq. 2) go to 7000
         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
         LUNREG = inum
         if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is Rotary axis code required in the Enable Cylinder block
c
 1500 MLEVL(NLEVL) = 15
      if (MACHTP .ne. 4 .or. MODCYL .eq. 0) then
          if (kfl .lt. NLEVL) go to 1600
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ISRCYL
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
         if (iwrn .eq. 2) go to 7000
         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
         ISRCYL = inum
         if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is Rotary axis value required in the Enable Cylinder block
c
 1600 MLEVL(NLEVL) = 16
      if (MACHTP .ne. 4 .or. MODCYL .eq. 0) then
          if (kfl .lt. NLEVL) go to 1700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ISRVAL
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
         if (iwrn .eq. 2) go to 7000
         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
         ISRVAL = inum
         if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Initial motion block registers
c
 1700 MLEVL(NLEVL) = 17
      iwrn   = 0
      inum   = MOBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MOBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...User defined non-motion block
c
 1800 MLEVL(NLEVL) = 18
      iwrn   = 0
      inum   = NMOBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NMOBLK = inum
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
