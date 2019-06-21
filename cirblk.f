c
c***********************************************************************
c
c   FILE NAME:  cirblk.for
c   CONTAINS:
c               cirblk  cbkfmt  cbktol  cbkhel  cbkreg bspblk
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cirblk.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:54
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cirblk (kfl,cmsg,kerr)
c
c   FUNCTION:  Circular interpolation menu handling routine.
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
      subroutine cirblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (ICIRFL,KPOSMP(1367))
      equivalence (LTHPCL,KPOSMP(1899)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MTPDYN,ICIRFL,MACHTP,LTHPCL(2)
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
c...Display Circular/Bspline interpolation menu
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
      go to (1000,1100,1200,1300,1400,1500,8000), igo
c
c...Walk through NonLinear interpolation
c
 1000 IWALK  = 1
c
c...Circular interpolation
c......Circular format
c
 1100 MLEVL(NLEVL-1) = 1
      call cbkfmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Circular tolerances
c
 1200 if (ICIRFL .ne. 1) then
          if (IWALK .eq. 1) go to 1300
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 2
      call cbktol (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Helical interpolation
c
 1300 if (MTPDYN .eq. 2) then
          if (IWALK .eq. 1) go to 1400
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 3
      call cbkhel (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Circular registers
c
 1400 MLEVL(NLEVL-1) = 4
      call cbkreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Bspline interpolation
c
 1500 MLEVL(NLEVL-1) = 5
      call bspblk (kfl,cmsg,kerr)
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
c   SUBROUTINE:  cbkfmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Circular interpolation format.
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
      subroutine cbkfmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (ICRXYZ,KPOSMP(1239))
      equivalence (ICIRFL,KPOSMP(1367))
      equivalence (ICRFMT,KPOSMP(1368)), (ICRSGN,KPOSMP(1369))
      equivalence (ICRPFL,KPOSMP(1370)), (ICRBRK,KPOSMP(1372))
      equivalence (ICRDIM,KPOSMP(1389))
      equivalence (ICRDIR,KPOSMP(1390)), (ICIRSW,KPOSMP(1391))
      equivalence (ICRPLS,KPOSMP(1398)), (ICIDLT,KPOSMP(1741))
      equivalence (IC3AXF,KPOSMP(1747))
      equivalence (LTHPCL,KPOSMP(1899)), (LCRFMT,KPOSMP(4103))
      equivalence (MTPDYN,KPOSMP(4126)), (ICRHEL,KPOSMP(4250))
c
      integer*4 MACHTP,ICIRFL,ICRFMT,ICRSGN,ICRPFL,ICRBRK,ICRHEL(10),
     1          ICIRSW,ICRXYZ,ICRDIM,ICRDIR,LTHPCL(2),LCRFMT,MTPDYN,
     2          ICRPLS(3),ICIDLT,IC3AXF(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,nc,iwrn,inum,ilod,ist,iyesno(2),iraddm(2),ipls(3),
     1          npls(3),jindex
c
      data iyesno /13,12/, iraddm /38,39/, ipls /73,74,75/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
c
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,
     1      1400), MLEVL(NLEVL)
c
c...Circular interpolation support
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ICIRFL
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICIRFL = inum
          ICIRSW = inum
          if (ICIRFL .eq. 2) ICRHEL(1) = 2
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Circular interpolation format
c
  200 MLEVL(NLEVL) = 2
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 290
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRFMT
      nc     = -1
      call prmint (inum,nc,1,1,10,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRFMT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  290 continue
c
c...Get Circular interpolation format in LMFP/LMDP mode
c
  300 MLEVL(NLEVL) = 3
      if (ICIRFL .ne. 1 .or. (MACHTP .eq. 4 .and.
     -    LTHPCL(1) .eq. 2 .and. LTHPCL(2) .eq. 2) .or.
     -    MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LCRFMT
      nc     = -1
      call prmint (inum,nc,1,1,9,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LCRFMT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Sign of incremental distances
c
  400 MLEVL(NLEVL) = 4
      if (ICIRFL .ne. 1 .or. (ICRFMT .ne. 2 .and. ICRFMT .ne. 4)) then
          if (kfl .lt. NLEVL) go to 500
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRSGN
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRSGN = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Circular interpolation support
c
  500 MLEVL(NLEVL) = 5
      if (ICIRFL .ne. 1 .or. MTPDYN .ne. 2 .or. ICRFMT .eq. 6) then
          if (kfl .lt. NLEVL) go to 600
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRDIM
      call prmvoc (inum,iraddm,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRDIM = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...All circular planes are allowed
c
  600 MLEVL(NLEVL) = 6
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRPFL
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRPFL = inum
          if (inum .eq. 1) then
             ICRPLS(1) = 1
             ICRPLS(2) = 1
             ICRPLS(3) = 1
          end if
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is 3-axis circular supported
c
  700 MLEVL(NLEVL) = 7
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 800
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IC3AXF(1)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IC3AXF(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Output final point on separate block
c
  800 MLEVL(NLEVL) = 8
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. IC3AXF(1) .eq. 2) then
          if (kfl .lt. NLEVL) go to 900
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IC3AXF(2)
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IC3AXF(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...3-axis intermediate point output
c
  900 MLEVL(NLEVL) = 9
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. IC3AXF(1) .eq. 2 .or.
     1    IC3AXF(2) .eq. 1) then
          if (kfl .lt. NLEVL) go to 950
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IC3AXF(9)
      nc     = 1
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IC3AXF(9) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  950 continue
c
c...Allowed circular planes
c
 1000 MLEVL(NLEVL) = 10
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. ICRPFL .eq. 1) then
          if (kfl .lt. NLEVL) go to 1100
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      nc     = 0
      do 1005 i=1,3
          if (ICRPLS(i) .eq. 2) goto 1005
          nc  = nc + 1
          npls(nc) = ipls(i)
 1005 continue
      call prmvcs (npls,nc,ipls,3,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRPLS(1) = 2
          ICRPLS(2) = 2
          ICRPLS(3) = 2
          do 1015 i=1,nc
             inum = jindex(ipls,npls(i),3)
             if (inum .ne. 0) ICRPLS(inum) = 1
 1015     continue
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...How circular moves are broken up
c
 1100 MLEVL(NLEVL) = 11
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRBRK
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRBRK = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Force position on circular output
c
 1200 MLEVL(NLEVL) = 12
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1300
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRXYZ
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRXYZ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Force direction code on circular output
c
 1300 MLEVL(NLEVL) = 13
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1350
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRDIR
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRDIR = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1350 continue
c
c...Estimate axes deltas with circular interpolation
c
 1400 MLEVL(NLEVL) = 14
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1450
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICIDLT
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICIDLT = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1450 continue
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
c   SUBROUTINE:  cbktol (kfl,cmsg,kerr)
c
c   FUNCTION:  Circular interpolation tolerances prompt handling rou-
c              tine.
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
      subroutine cbktol (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICRADJ,KPOSMP(1371))
c
      integer*4 ICRADJ
c
      equivalence (CIRTOL,POSMAP(2201))
c
      real*8 CIRTOL(4)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,ilod,ist,i,inum,iyesno(2),inc
c
      real*8 rnum,rmx
c
      data iyesno /13,12/
c
      data rmx /99999999/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,500), MLEVL(NLEVL)
c
c...Get circular tolerances
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 180 i=ist,4,1
          inc    = inc    + 1
          iwrn   = 0
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = CIRTOL(inc)
          nc     = 1
          call prmrel (rnum,nc,1,.0D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CIRTOL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...Adjust circular end point
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = ICRADJ
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRADJ = inum
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
c   SUBROUTINE:  cbkhel (kfl,cmsg,kerr)
c
c   FUNCTION:  Helical interpolation prompt handling routine.
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
      subroutine cbkhel (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ICIRFL,KPOSMP(1367)), (ICRHEL,KPOSMP(4250))
c
      integer*4 ICIRFL,ICRHEL(10)
c
      equivalence (CIRTOL,POSMAP(2201))
c
      real*8 CIRTOL(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,ilod,ist,i,iyesno(2),inc,inum
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
c
      data rmn /.00001d0/, rmx /99999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,300,400,500), MLEVL(NLEVL)
c
c...Get Helical interpolation flags
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 180 i=ist,2,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          if (ICIRFL .eq. 0 .or. (i .gt. 1 .and. ICRHEL(1) .ne. 1)) then
              if (kfl .lt. NLEVL) go to 180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          iwrn   = 0
          inum   = ICRHEL(inc)
          nc     = 2
          nc = -2
          call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ICRHEL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...How helical depth is output
c
  300 MLEVL(NLEVL) = 3
      if (ICIRFL .ne. 1 .or. ICRHEL(1) .ne. 1 .or. ICRHEL(2) .ne. 1)
     1        then
          if (kfl .lt. NLEVL) go to 400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRHEL(6)
      nc     = 1
      call prmint (inum,nc,1,0,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRHEL(6) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...How helical lead is output
c
  400 MLEVL(NLEVL) = 4
      if (ICIRFL .ne. 1 .or. ICRHEL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 500
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRHEL(3)
      nc     = 1
      call prmint (inum,nc,1,0,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRHEL(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Manual Helical generation tolerance
c
  500 MLEVL(NLEVL) = 5
      if (ICRHEL(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 8000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = CIRTOL(5)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CIRTOL(5) = rnum
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
c   SUBROUTINE:  cbkreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Circular interpolation registers prompt handling routine.
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
      subroutine cbkreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (POLCOD,KPOSMP(1262))
      equivalence (ICIRFL,KPOSMP(1367)), (ICRFMT,KPOSMP(1368))
      equivalence (CIRCOD,KPOSMP(1378)), (ICRIJP,KPOSMP(1384))
      equivalence (IPLNDF,KPOSMP(1388)), (ICRPLF,KPOSMP(1397))
      equivalence (IC3AXF,KPOSMP(1747))
      equivalence (LTHPCL,KPOSMP(1899)), (LFDLCD,KPOSMP(4107))
      equivalence (LFIRCD,KPOSMP(4112)), (LDIRCD,KPOSMP(4118))
      equivalence (ISXVAL,KPOSMP(4209)), (PLNCOD,KPOSMP(4222))
      equivalence (CIRCON,KPOSMP(4242)), (ICRHEL,KPOSMP(4250))
c
      integer*4 ICIRFL,CIRCOD(6),ICRIJP,PLNCOD(4),IPLNDF,ICRFMT,
     1          POLCOD(3),MACHTP,ICRPLF,LFIRCD(6),LDIRCD(6),IC3AXF(10),
     2          LFDLCD,LTHPCL(2),ISXVAL,ICRHEL(10),CIRCON(8)
c
      equivalence (DUMMY ,POSMAP(0003)), (CIRCDV,POSMAP(2206))
      equivalence (PLNCDV,POSMAP(2208)), (PLNDWL,POSMAP(2212))
      equivalence (CIRCOV,POSMAP(2215))
      equivalence (POLCDV,POSMAP(2227)), (CFRCDV,POSMAP(4595))
      equivalence (HELCDV,POSMAP(2242)), (C3XCDV,POSMAP(2244))
      equivalence (CDRCDV,POSMAP(4597))
c
      real*8 DUMMY,CIRCDV(2),PLNCDV(4),PLNDWL,POLCDV(2),CFRCDV(2),
     1       CDRCDV(2),HELCDV(2),C3XCDV(2),CIRCOV(8)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,i,iyesno(2),inc,n,ix
c
      real*8 rnum,rmn,rmx
c
      data iyesno /13,12/
c
      data rmn /.001d0/, rmx /99999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,310,310,510,510,510,810,810,1010,1010,1210,1210,
     1      1410,1410,1610,1610,1610,1610,2010,2010,2010,2300,2400,2510,
     2      2510,2510,2510,2900,3000,3100,3200), MLEVL(NLEVL)
c
c...Get polar coord linear code
c
  100 ist    = 1
  110 inc    = ist    - 1
      if (ICIRFL .ne. 1 .or. ICRFMT .ne. 7) then
          if (kfl .lt. NLEVL) go to 300
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 190 i=ist,2,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = POLCOD(inc)
          rnum   = POLCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              POLCOD(inc) = inum
              POLCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Get conversational circular codes
c
  300 ist    = 3
  310 inc    = ist    - 3
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 350 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = CIRCOD(inc)
          rnum   = CIRCDV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CIRCOD(inc) = inum
              CIRCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  350 continue
  395 continue
c
c...Get circular direction codes
c
  500 ist    = 5
  510 inc    = ist    - 5
      if (ICIRFL .ne. 1 .or. ICRFMT .ne. 10) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 550 i=ist,7,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = CIRCON(inc)
          rnum   = CIRCOV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CIRCON(inc) = inum
              CIRCOV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  550 continue
  595 continue
c
c...Get LMFP circular direction codes
c
  800 ist    = 8
  810 inc    = ist    - 8
      if (ICIRFL .ne. 1 .or. MACHTP .ne. 4 .or. LTHPCL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 895
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 850 i=ist,9,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = LFIRCD(inc)
          rnum   = CFRCDV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LFIRCD(inc) = inum
              CFRCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  850 continue
  895 continue
c
c...Get LMDP circular direction codes
c
 1000 ist    = 10
 1010 inc    = ist    - 10
      if (ICIRFL .ne. 1 .or. MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1050 i=ist,11,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = LDIRCD(inc)
          rnum   = CDRCDV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LDIRCD(inc) = inum
              CDRCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1050 continue
 1095 continue
c
c...Get Helical circular direction codes
c
 1200 ist    = 12
 1210 inc    = ist    - 12
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. ICRHEL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1250 i=ist,13,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = ICRHEL(inc+3)
          rnum   = HELCDV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ICRHEL(inc+3) = inum
              HELCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1250 continue
 1295 continue
c
c...Get 3-axis circular direction codes
c
 1400 ist    = 14
 1410 inc    = ist    - 14
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. IC3AXF(1) .eq. 2) then
          if (kfl .lt. NLEVL) go to 1495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1450 i=ist,15,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = IC3AXF(inc+2)
          rnum   = C3XCDV(inc)
          iwrn   = 0
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IC3AXF(inc+2) = inum
              C3XCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1450 continue
 1495 continue
c
c...Get Center point codes
c
 1600 ist    = 16
 1610 inc    = ist    - 16 + 2
      if (ICIRFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1680 i=ist,19,1
          inc    = inc    + 1
          if (MACHTP .eq. 2 .and. inc .eq. 4) then
              if (kfl .lt. NLEVL) go to 1680
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = CIRCOD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              CIRCOD(inc) = inum
              if (LTHPCL(1) .eq. 1) LFIRCD(inc) = inum
              if (LTHPCL(2) .eq. 1) LDIRCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1680 continue
 1695 continue
c
c...Get Intermediate point codes
c
 2000 ist    = 20
 2010 inc    = ist    - 20 + 4
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2 .or. IC3AXF(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 2095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2080 i=ist,22,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IC3AXF(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IC3AXF(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2080 continue
 2095 continue
c
c...Polar angle code
c
 2300 MLEVL(NLEVL) = 23
      if (ICIRFL .ne. 1 .or. ICRFMT .ne. 7) then
          if (kfl .lt. NLEVL) go to 2390
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = POLCOD(3)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          POLCOD(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2390 continue
c
c...Output all planes of circular
c...with XY plane Center point codes
c
 2400 MLEVL(NLEVL) = 24
      if (ICIRFL .ne. 1 .or. MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 2490
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ICRIJP
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRIJP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2490 continue
c
c...Get Plane selection codes
c
 2500 ist    = 25
 2510 inc    = ist    - 25
      do 2580 i=ist,28,1
          inc    = inc    + 1
          if ((MACHTP .eq. 2 .and. inc .ne. 2) .or. (inc .eq. 4 .and.
     1        (MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1))) then
              PLNCOD(inc) = 0
              PLNCDV(inc) = DUMMY
              if (kfl .lt. NLEVL) go to 2580
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PLNCOD(inc)
          rnum   = PLNCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PLNCOD(inc) = inum
              PLNCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 2580 continue
 2595 continue
c
c...Include axis value in code for select user plane command
c
 2900 MLEVL(NLEVL) = 29
      if (MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 2995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ISXVAL
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISXVAL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2995 continue
c
c...Output dwell with plane selection
c
 3000 MLEVL(NLEVL) = 30
      iwrn   = 0
      inum   = IPLNDF
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IPLNDF = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Amount of dwell to output with plane selection
c
 3100 MLEVL(NLEVL) = 31
      if (IPLNDF .ne. 1) then
          if (kfl .lt. NLEVL) go to 3150
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = PLNDWL
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PLNDWL = rnum
          if (kfl .ge. NLEVL) go to 8000
       endif
 3150 continue
c
c...Output plane selection codes in block by themselves
c
 3200 MLEVL(NLEVL) = 32
      iwrn   = 0
      inum   = ICRPLF
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICRPLF = inum
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
c   SUBROUTINE:  bspblk (kfl,cmsg,kerr)
c
c   FUNCTION:  Bspline interpolation format.
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
      subroutine bspblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (BSPLSW,KPOSMP(4084)), (BSPLFL,KPOSMP(4088))
      equivalence (BSPCOD,KPOSMP(4089)), (BSPPCD,KPOSMP(4090))
      equivalence (BSPFRC,KPOSMP(4091))
      equivalence (NBSCDS,KPOSMP(4230)), (BSTCOD,KPOSMP(4231))
      equivalence (NBECDS,KPOSMP(4236)), (BNDCOD,KPOSMP(4237))
c
      integer*4 MACHTP,BSPLSW,BSPLFL,BSPCOD,BSPPCD,BSPFRC(4)
      integer*4 NBSCDS,BSTCOD(5),NBECDS,BNDCOD(5)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (BSPCDV,POSMAP(4901)), (BSPTOL,POSMAP(4902))
      equivalence (BSTVAL,POSMAP(4936)), (BNDVAL,POSMAP(4941))
c
      real*8 BSPCDV,BSPTOL(2),DUMMY,BSTVAL(5),BNDVAL(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,inc,nc,iwrn,inum,ilod,ist,iyesno(2),iary(5)
      real*8 rmx,rnum,rary(5)
c
      integer*4 knotnode(2) /76,77/
      data iyesno /13,12/
      data rmx /99999999.d0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
c
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,410,410,600,700,800,900,1000,1100),MLEVL(NLEVL)
c
c...Bspline interpolation support
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = BSPLFL
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPLFL = inum
          BSPLSW = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Registers to output when entering spline interpolation mode.
c
  200 MLEVL(NLEVL) = 2
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      nc = NBSCDS
      do 220 i=1,nc
        iary(i) = BSTCOD(i)
        rary(i) = BSTVAL(i)
  220 continue
      call prmcod (iary,rary,nc,5,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          do 240 i=1,nc
            BSTCOD(i) = iary(i)
            BSTVAL(i) = rary(i)
  240     continue
          NBSCDS = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Registers to output when leaving spline interpolation mode.
c
  300 MLEVL(NLEVL) = 3
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      nc = NBECDS
      do 320 i=1,nc
        iary(i) = BNDCOD(i)
        rary(i) = BNDVAL(i)
  320 continue
      call prmcod (iary,rary,nc,5,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          do 340 i=1,nc
            BNDCOD(i) = iary(i)
            BNDVAL(i) = rary(i)
  340     continue
          NBECDS = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Get Bspline interpolation tolerance
c
  400 ist    = 4
  410 inc    = ist    - 4
      do 480 i=ist,5,1
          inc    = inc    + 1
          if (BSPLFL .ne. 1) then
             if (kfl .lt. NLEVL) go to 480
             call errmsg ('NOTAPPLY',1,2)
             go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = BSPTOL(inc)
          nc     = 1
          call prmrel (rnum,nc,1,.0D-5,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              BSPTOL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  480 continue
c
c...Get Bspline interpolation tolerance
c
c 200 MLEVL(NLEVL) = 2
c     if (BSPLFL .ne. 1) then
c         if (kfl .lt. NLEVL) go to 300
c         call errmsg ('NOTAPPLY',1,2)
c         go to 8000
c     endif
c     iwrn   = 0
c     rnum   = BSPTOL
c     nc     = 1
c     call prmrel (rnum,nc,1,0.D-4,rmx,kfl,ilod,iwrn,cmsg,kerr)
c     if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
c         if (iwrn .eq. 2) go to 7000
c         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c         BSPTOL = rnum
c         if (kfl .ge. NLEVL) go to 8000
c     endif
c
c...Get Bspline code register
c
  600 MLEVL(NLEVL) = 6
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPCOD
      rnum   = BSPCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPCOD = inum
          BSPCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Force interpolation mode after Bspline block
c
  700 MLEVL(NLEVL) = 7
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 800
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPFRC(1)
      nc     = 2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPFRC(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Output Bspline code by itself
c
  800 MLEVL(NLEVL) = 8
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 900
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPFRC(2)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPFRC(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Knot values or node distances.
c
  900 continue
      MLEVL(NLEVL) = 9
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPFRC(3)
      call prmvoc (inum,knotnode,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPFRC(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Node parameter register
c
 1000 MLEVL(NLEVL) = 10
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1100
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPPCD
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPPCD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Duplicate start point
c
 1100 continue
      MLEVL(NLEVL) = 11
      if (BSPLFL .ne. 1) then
          if (kfl .lt. NLEVL) go to 1200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = BSPFRC(4)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BSPFRC(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
 1200 continue
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
