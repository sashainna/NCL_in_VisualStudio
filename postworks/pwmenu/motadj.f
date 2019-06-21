c***********************************************************************
c
c   FILE NAME:  motadj.for
c   CONTAINS:
c               motadj  mtamsc  mtalin  mtalka  mtaslw  mtamxd  mtaacc
c               mtaxfm  mtaxfo
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        motadj.f , 25.4
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:38:24
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  motadj (kfl,cmsg,kerr)
c
c   FUNCTION:  Motion Adjustment Parameters menu handling routine.
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
      subroutine motadj (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (MACHTP,KPOSMP(1201))
      equivalence (IJKROT,KPOSMP(1739))
c
      integer*4 MACHTP,IRTNUM,IJKROT
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
c...Display Motion Adjustments menu
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
      go to (1000,1100,1200,1300,1400,1500,1600,1700,1800,8000), igo
c
c...Walk through Motion Adjustments
c
 1000 IWALK  = 1
c
c...Miscellaneous Adjustments
c
 1100 MLEVL(NLEVL-1) = 1
      call mtamsc (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Linearization
c
 1200 if (IRTNUM .eq. 0 .and. IJKROT .ne. 1) then
          if (IWALK .eq. 1) go to 1300
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 2
      call mtalin (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Look Ahead
c
 1300 if (IRTNUM .lt. 2 .or. IJKROT .eq. 1) then
          if (IWALK .eq. 1) go to 1400
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 3
      call mtalka (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Slowdowns
c
 1400 MLEVL(NLEVL-1) = 4
      call mtaslw (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Maximum axes departures
c
 1500 MLEVL(NLEVL-1) = 5
      call mtamxd (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Acceleration blocks
c
 1600 MLEVL(NLEVL-1) = 6
      call mtaacc (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Transformation blocks
c
 1700 MLEVL(NLEVL-1) = 7
      call mtaxfm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Transformation block output
c
 1800 MLEVL(NLEVL-1) = 8
      call mtaxfo (kfl,cmsg,kerr)
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
c   SUBROUTINE:  mtamsc (kfl,cmsg,kerr)
c
c   FUNCTION:  Miscellaneous motion adjustments.
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
      subroutine mtamsc (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (MAXAXS,KPOSMP(1212))
      equivalence (AXSPRI,KPOSMP(1213)), (HLDBCK,KPOSMP(1223))
      equivalence (IRTNUM,KPOSMP(1243)), (EXCLAX,KPOSMP(3401))
      equivalence (EXCLCO,KPOSMP(3417)), (EXCLPS,KPOSMP(3421))
      equivalence (EXCLNE,KPOSMP(3469)), (NUMEXC,KPOSMP(3517))
      equivalence (EXCLNM,KPOSMP(3518))
c
      integer*4 IDUMMY,MACHTP,NUMLIN(3),MAXAXS,AXSPRI(10),HLDBCK,
     1          IRTNUM,EXCLAX(4,4),EXCLCO(4),EXCLPS(12,4),EXCLNE(12,4),
     2          NUMEXC,EXCLNM(4)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,inc,iyesno(2),mea1(4),mea2(4),
     1          mea3(4),mea4(4),jindex
c
      character*2 laxs(10)
c
      data iyesno /13,12/
      data mea1 /4,8,12,16/, mea2 /5,9,13,17/, mea3 /6,10,14,18/
      data mea4 /7,11,15,19/
      data laxs /'X1','X2','Y1','Y2','Z1','Z2','A1','A2','A3','A4'/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,410,510,610,710,410,510,610,710,410,510,610,710,
     1      410,510,610,710,2000), MLEVL(NLEVL)
c
c...Maximum # of axis in single block
c
  100 MLEVL(NLEVL) = 1
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = MAXAXS
      nc     = -1
      call prmint (inum,nc,1,1,10,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MAXAXS = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Priority for too many axis moves in a single block
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      if (MAXAXS .ge. NUMLIN(1)+NUMLIN(2)+NUMLIN(3)+IRTNUM) then
          if (kfl .lt. NLEVL) go to 300
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      nc     = jindex(AXSPRI,IDUMMY,10) - 1
      if (nc .eq. -1) nc = 10
      call prmaxs (AXSPRI,nc,10,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1) go to 8000
          if (nc .ne. 10) AXSPRI(nc+1) = IDUMMY
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Number of MEA sets
c
  300 MLEVL(NLEVL) = 3
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 390
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = NUMEXC
      nc     = -1
      call prmint (inum,nc,1,0,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NUMEXC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  390 continue
c
c...Mutually exclusive axes parameters
c
  400 ist    = 4
c
c......Mutually exclusive axes
c
  410 MLEVL(NLEVL) = ist
      inc    = jindex(mea1,ist,4)
      iwrn   = 0
      if (inc .gt. NUMEXC) then
          if (kfl .lt. NLEVL) go to 790
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      call prmaxs (EXCLAX(1,inc),EXCLNM(inc),4,2,kfl,ilod,iwrn,cmsg,
     1             kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1) go to 8000
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c......Controlling axis for MEA
c
      ist    = ist    + 1
  510 MLEVL(NLEVL) = ist
      inc    = jindex(mea2,ist,4)
      iwrn   = 0
      if (inc .gt. NUMEXC) then
          if (kfl .lt. NLEVL) go to 790
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      nc     = 1
      call prmaxs (EXCLCO(inc),nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1) go to 8000
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c......Positive move axes priority for MEA
c
      ist    = ist    + 1
  610 MLEVL(NLEVL) = ist
      inc    = jindex(mea3,ist,4)
      iwrn   = 0
      if (inc .gt. NUMEXC) then
          if (kfl .lt. NLEVL) go to 790
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      nc     = jindex(EXCLPS(1,inc),IDUMMY,12) - 1
      if (nc .eq. -1) nc = 12
      call prmaxs (EXCLPS(1,inc),nc,12,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1) go to 8000
          if (nc .ne. 12) EXCLPS(nc+1,inc) = IDUMMY
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c......Negative move axes priority for MEA
c
      ist    = ist    + 1
  710 MLEVL(NLEVL) = ist
      inc    = jindex(mea4,ist,4)
      iwrn   = 0
      if (inc .gt. NUMEXC) then
          if (kfl .lt. NLEVL) go to 790
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      nc     = jindex(EXCLNE(1,inc),IDUMMY,12) - 1
      if (nc .eq. -1) nc = 12
      call prmaxs (EXCLNE(1,inc),nc,12,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1) go to 8000
          if (nc .ne. 12) EXCLNE(nc+1,inc) = IDUMMY
          if (kfl .ge. NLEVL) go to 8000
      endif
      ist    = ist    + 1
      if (inc .lt. NUMEXC) go to 410
  790 continue
c
c...Hold back motion
c
 2000 MLEVL(NLEVL) = 20
      iwrn   = 0
      inum   = HLDBCK
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          HLDBCK = inum
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
c   SUBROUTINE:  mtalin (kfl,cmsg,kerr)
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
      subroutine mtalin (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (IRTNUM,KPOSMP(1243))
      equivalence (LNRADJ,KPOSMP(1277)), (LRTRCT,KPOSMP(1278))
      equivalence (LRTTAD,KPOSMP(1335)), (IRTRTE,KPOSMP(1343))
      equivalence (LRTRFL,KPOSMP(1348))
      equivalence (IRTSHF,KPOSMP(1374)), (IJKROT,KPOSMP(1739))
c
      integer*4 LRTRCT,LRTTAD,MACHTP,IRTNUM,IRTRTE(2),IJKROT,
     1          IRTSHF,LNRADJ,LRTRFL
c
      equivalence (LNRTOL,POSMAP(2251)), (LNRATL,POSMAP(2286))
      equivalence (RETFED,POSMAP(2254)), (RETPL ,POSMAP(2258))
      equivalence (TAXTOL,POSMAP(2262))
      equivalence (RETDIS,POSMAP(2279)), (RTSHFD,POSMAP(4947))
c
      real*8 LNRTOL(2),RETDIS,RETFED(4),RTSHFD,LNRATL(5),RETPL(4),
     1       TAXTOL
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,ilod,ist,i,iyesno(2),inum,inc,idir(3),idipln(3)
c
      real*8 rnum(4),rmn,rmx,rmn4(4),rmx4(4),rdis
c
      data iyesno /13,12/, idir /12,81,82/, idipln /12,521,522/
c
      data rmn /-99999999.d0/, rmx /99999999.d0/
      data rmn4 /-1.,-1.,-1.,-99999999./, rmx4 /1.,1.,1.,99999999./
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,300,410,410,410,410,800,900,1000,1100,1200,1300,
     1      1400,1500,1610,1610,1610,1610,2000,2100), MLEVL(NLEVL)
c
c...Default linearization tolerance
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,2,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum(1) = LNRTOL(inc)
          nc     = 1
          call prmrel (rnum(1),nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LNRTOL(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Adjust tool axis to minimize rotary movement
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = LNRADJ
      nc     = -2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LNRADJ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Tool axis adjustment tolerances
c
  400 ist    = 4
  410 inc    = ist    - 4
      if (LNRADJ .eq. 2) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
          endif
          if (IMSCAN .eq. 2) go to 8000
          go to 495
      endif
      do 490 i=ist,7,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum(1) = LNRATL(inc)
          nc     = 1
          call prmrel (rnum(1),nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LNRATL(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
  490 continue
  495 continue
c
c...Tool axis component tolerance
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      rnum(1) = TAXTOL
      nc     = 1
      call prmrel (rnum(1),nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TAXTOL = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Automatic retract on longest route
c
  900 MLEVL(NLEVL) = 9
      iwrn   = 0
      inum   = LRTRCT + 1
      nc     = -3
      call prmvoc (inum,idipln,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LRTRCT = inum - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Automatic retract on longest route
c...When lintol is off
c
 1000 MLEVL(NLEVL) = 10
      iwrn   = 0
      inum   = LRTRFL
      nc     = 2
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LRTRFL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Retract adding tool length to retract distance
c
 1100 MLEVL(NLEVL) = 11
      if (LRTRCT .ne. 1) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = LRTTAD
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LRTTAD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1195 continue
c
c...Retract distance
c
 1200 MLEVL(NLEVL) = 12
      if (LRTRCT .ne. 1) then
          if (kfl .lt. NLEVL) go to 1295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum(1) = RETDIS
      nc     = 1
      call prmrel (rnum(1),nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RETDIS = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
 1295 continue
c
c...Get clearance plane
c
 1300 MLEVL(NLEVL) = 13
      if (LRTRCT .ne. 2) then
          if (kfl .lt. NLEVL) go to 1395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum(1) = RETPL(1)
      rnum(2) = RETPL(2)
      rnum(3) = RETPL(3)
      rnum(4) = RETPL(4)
 1320 nc     = 4
      call prmrel (rnum,nc,4,rmn4,rmx4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (nc .eq. 1) then
              nc    = 4
              rnum(4) = rnum(1)
              rnum(1) = 0.
              rnum(2) = 0.
              rnum(3) = 1.
          endif
          rdis   = dsqrt(rnum(1)**2 + rnum(2)**2 + rnum(3)**2)
          if (nc .ne. 4 .or. rdis .eq. 0.) then
              call errmsg ('INVRESP',1,2)
              iwrn   = 1
              if (IMSCAN .eq. 2) go to 8000
              go to 1320
          endif
          do 1370 i=1,3,1
              RETPL(i) = rnum(i) / rdis
 1370     continue
          RETPL(4) = rnum(4)
          if (kfl .ge. NLEVL) go to 8000
      endif
 1395 continue
c
c...Offset tool prior to retract
c
 1400 MLEVL(NLEVL) = 14
      if (LRTRCT .eq. 0 .or. MACHTP .eq. 3) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
          endif
          if (IMSCAN .eq. 2) go to 8000
          go to 1495
      endif
      iwrn   = 0
      inum   = IRTSHF
      nc     = -3
      call prmvoc (inum,idir,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTSHF = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1495 continue
c
c...Offset distance
c
 1500 MLEVL(NLEVL) = 15
      if (LRTRCT .eq. 0 .or. IRTSHF .eq. 1) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
          endif
          if (IMSCAN .eq. 2) go to 8000
          go to 1595
      endif
      iwrn   = 0
      rnum(1) = RTSHFD
      nc     = 1
      call prmrel (rnum(1),nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          RTSHFD = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
 1595 continue
c
c...Retract feeds
c
 1600 ist    = 16
 1610 inc    = ist    - 16
      if (LRTRCT .eq. 0) then
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
          endif
          if (IMSCAN .eq. 2) go to 8000
          go to 1695
      endif
      do 1690 i=ist,19,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum(1) = RETFED(inc)
          nc     = 1
          if ((inc .eq. 3 .and. MACHTP .ne. 3) .or.
     1        (inc .eq. 4 .and. IRTSHF .eq. 1)) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
              endif
              if (IMSCAN .eq. 2) go to 8000
              go to 1690
          endif
          call prmrel (rnum(1),nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RETFED(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
 1690 continue
 1695 continue
c
c...Shortest route determination
c
 2000 MLEVL(NLEVL) = 20
      if (IRTNUM .lt. 2) then
          if (kfl .ge. NLEVL) call errmsg ('NOTAPPLY',1,2)
          if (IMSCAN .eq. 2) go to 8000
          go to 2095
      endif
      iwrn   = 0
      inum   = IRTRTE(1)
      nc     = -1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTRTE(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2095 continue
c
c...Shortest route controlling axis
c
 2100 MLEVL(NLEVL) = 21
      if (IRTNUM .lt. 2 .or. IRTRTE(1) .ne. 4) then
          if (kfl .ge. NLEVL) call errmsg ('NOTAPPLY',1,2)
          if (IMSCAN .eq. 2) go to 8000
          go to 2195
      endif
      iwrn   = 0
      inum   = IRTRTE(2)
      nc     = 1
      call prmint (inum,nc,1,1,IRTNUM,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTRTE(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2195 continue
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
c   SUBROUTINE:  mtalka (kfl,cmsg,kerr)
c
c   FUNCTION:  AC-Style head prompt handling routine.
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
      subroutine mtalka (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (ACHEAD,KPOSMP(1645)), (IACFLG,KPOSMP(1648))
c
      integer*4 ACHEAD,IACFLG(5)
c
      equivalence (TLATOL,POSMAP(0057))
c
      real*8 TLATOL
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,inoyes(2),i,inc,iynaut(3),
     1          inyrap(3)
c
      real*8 rnum,rmn,rmx
c
      data inoyes /12,13/, iynaut /13,12,67/, inyrap /48,67,79/
c
      data rmn /.0/, rmx /1./
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,200,300,410,410,410), MLEVL(NLEVL)
c
c...Does this machine have an AC-style head
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = ACHEAD
      nc     = -3
      call prmvoc (inum,iynaut,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ACHEAD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Look ahead mode
c
  200 MLEVL(NLEVL) = 2
      if (ACHEAD .eq. 2) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IACFLG(1)
      nc     = -3
      call prmvoc (inum,inyrap,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IACFLG(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...Vector component deviation
c
  300 MLEVL(NLEVL) = 3
      if (ACHEAD .eq. 2 .or. IACFLG(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      nc     = 1
      rnum   = TLATOL
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TLATOL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Machine slowdown support
c
  400 ist    = 4
  410 inc    = ist    - 3
      if (ACHEAD .eq. 2 .or. IACFLG(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 490 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IACFLG(inc)
          nc     = 2
          call prmvoc (inum,inoyes,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IACFLG(inc) = inum
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
c   SUBROUTINE:  mtaslw (kfl,cmsg,kerr)
c
c   FUNCTION:  Slowdown parmaeters prompt handling routine.
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
      subroutine mtaslw (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087))
      equivalence (SLWFL ,KPOSMP(1701)), (SLWCD ,KPOSMP(1706))
      equivalence (ISLWDN,KPOSMP(1721))
c
      integer*4 SLWFL(5),SLWCD(10),ISLWDN,IUNIT
c
      equivalence (SLWVR ,POSMAP(2451)), (SLWVL ,POSMAP(2456))
c
      real*8 SLWVR(5),SLWVL(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,iyesno(2),i,inc,ionoff(2),cd(12)
c
      real*8 rnum,ssover(2,13)
c
      data iyesno /13,12/, ionoff /47,48/
      data cd /1,2,3,4, 1,1,2,2,3,3,4,4/
c
      data ssover /.0001,  1.000,   .0004,  3.000,   .0006,  5.000,
     1             .0009,  7.000,   .0017, 10.000,   .0028, 15.000,
     2             .0038, 20.000,   .0053, 25.000,   .0073, 30.000,
     3             .0093, 40.000,   .0126, 50.000,   .0186, 75.000,
     4             .0272,100.000  /
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,400,500,610,610,610,610,610,610,610,610,610,
     1      610,610,610,610,1900,2000), MLEVL(NLEVL)
c
c...Machine slowdown support
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,3,1
          inc    = inc    + 1
          if (inc .ge. 2 .and. SLWFL(1) .ne. 1) then
              if (kfl .lt. NLEVL) go to 195
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SLWFL(inc)
          nc     = 2
          if (i .eq. 1) nc = -2
          call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SLWFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Number of slowdown spans supported
c
  400 MLEVL(NLEVL) = 4
      if (SLWFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = SLWFL(4)
      nc     = -1
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SLWFL(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Min angular change for slowdowns
c
  500 MLEVL(NLEVL) = 5
      if (SLWFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = SLWVR(1)
      nc     = 1
      call prmrel (rnum,nc,1,0.d0,360.d0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SLWVR(1) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Slowdown codes
c
  600 ist    = 6
  610 inc    = ist    - 6
      if (SLWFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 690 i=ist,18,1
          inc    = inc    + 1
          if (inc .ne. 13 .and. cd(inc) .gt. SLWFL(4)) then
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SLWCD(inc)
          rnum   = SLWVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SLWCD(inc) = inum
              SLWVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
  695 continue
c
c...Default slowdown tolerance
c
 1900 MLEVL(NLEVL) = 19
      if (SLWFL(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 1995
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = SLWVR(2)
      nc     = 1
      call prmrel (rnum,nc,1,.0001d0,9999.d0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SLWVR(2) = rnum
c
c......Calculate maximum feed for slowdowns
c......when angular change = 90 degrees
c
          do 1980 i=2,13,1
               if (SLWVR(2) .ge. ssover(1,i-1) .and.
     1             SLWVR(2) .lt. ssover(1,i)) go to 1990
 1980     continue
 1990     SLWVR(3) = ssover(2,i-1)
          if (IUNIT .eq.  2) SLWVR(3) = SLWVR(3) * 25.4
          if (kfl .ge. NLEVL) go to 8000
      endif
 1995 continue
c
c...Default slowdown mode
c
 2000 MLEVL(NLEVL) = 20
      if (SLWFL(1) .eq. 1) then
          if (kfl .lt. NLEVL) go to 2095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = ISLWDN
      call prmvoc (inum,ionoff,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ISLWDN = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2095 continue
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
c   SUBROUTINE:  mtamxd (kfl,cmsg,kerr)
c
c   FUNCTION:  Maximum axis departures prompt handling routine.
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
      subroutine mtamxd (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IMXDRP,KPOSMP(1743)), (IJKROT,KPOSMP(1739))
c
      integer*4 NUMLIN(3),IRTNUM,IJKROT,IMXDRP
c
      equivalence (PPMAXD,POSMAP(1584))
c
      real*8 PPMAXD(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,ilod,ist,i,iyesno(2),inum,inc,ipt(6),ilin(6)
c
      real*8 rnum,rmn,rmx
c
      data ipt /1,2,1,2,1,2/, ilin /1,1,2,2,3,3/
c
      data iyesno /13,12/
c
      data rmn /-99999999.d0/, rmx /99999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,210,210,210,210,210,210,210), MLEVL(NLEVL)
c
c...Maximum departure moves apply with RAPID
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = IMXDRP
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IMXDRP = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Maximum departure distances
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,11,1
          inc    = inc    + 1
          iwrn   = 0
          if (inc .le. 6) then
              if (NUMLIN(ilin(inc)) .lt. ipt(inc)) iwrn = 1
          else
              if (inc-6 .gt. IRTNUM .or. IJKROT .eq. 1) iwrn = 1
          endif
c
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 290
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
          MLEVL(NLEVL) = i
          rnum   = PPMAXD(inc)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PPMAXD(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
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
c   SUBROUTINE:  mtaacc (kfl,cmsg,kerr)
c
c   FUNCTION:  Acceleration block parameters prompt handling routine.
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
      subroutine mtaacc (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IACCFL,KPOSMP(1745))
c
      integer*4 IACCFL(2)
c
      equivalence (MAXVEL,POSMAP(4293)), (AXSVEL,POSMAP(4294))
      equivalence (ACCSTP,POSMAP(4295))
c
      real*8 MAXVEL,AXSVEL,ACCSTP(2)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,iyesno(2),i,inc,ionoff(2)
c
      real*8 rnum
c
      data iyesno /13,12/, ionoff /47,48/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,300,400,510,510), MLEVL(NLEVL)
c
c...Machine acceleration support
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,2,1
          inc    = inc    + 1
          if (inc .eq. 2 .and. IACCFL(1) .ne. 1) then
              if (kfl .lt. NLEVL) go to 195
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IACCFL(inc)
          nc     = 2
          if (inc .eq. 1) nc = -2
          if (inc .eq. 1) then
              call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          else
              call prmvoc (inum,ionoff,nc,kfl,ilod,iwrn,cmsg,kerr)
          endif
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IACCFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Maximum vectorial velocity
c
  300 MLEVL(NLEVL) = 3
      if (IACCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = MAXVEL
      nc     = 1
      call prmrel (rnum,nc,1,.001d0,4000.d0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MAXVEL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Capped vectorial velocity
c
  400 MLEVL(NLEVL) = 4
      if (IACCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = AXSVEL
      nc     = 1
      call prmrel (rnum,nc,1,.001d0,4000.d0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          AXSVEL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Feed rate steps
c
  500 ist    = 5
  510 inc    = ist    - 5
      if (IACCFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 590 i=ist,6,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = ACCSTP(inc)
          nc     = 1
          call prmrel (rnum,nc,1,.001d0,4000.d0,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ACCSTP(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  590 continue
  595 continue
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
c   SUBROUTINE:  mtaxfm (kfl,cmsg,kerr)
c
c   FUNCTION:  Transformation parameters prompt handling routine.
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
      subroutine mtaxfm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (XFMFL ,KPOSMP(0969)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTYPE,KPOSMP(1486))
c
      integer*4 XFMFL(20),IRTYPE(20),IRTNUM
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,iyesno(2),i,inc,ixyz(2)
c
      data iyesno /13,12/, ixyz /523,524/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,300,400,500,600,700,810,810,1000,1100), MLEVL(NLEVL)
c
c...Machine transformation support
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,2,1
          inc    = inc    + 1
          if (inc .ge. 2 .and. XFMFL(1) .ne. 1) then
              if (kfl .lt. NLEVL) go to 195
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = XFMFL(inc)
          nc     = 2
          if (i .eq. 1 .or. i .eq. 3) nc = -2
          call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              XFMFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Rotation calculation
c
  300 MLEVL(NLEVL) = 3
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(3)
      nc     = -1
      call prmint (inum,nc,1,1,5,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  395 continue
c
c...Rotation order
c
  400 MLEVL(NLEVL) = 4
      if (XFMFL(1) .ne. 1 .or. (XFMFL(3) .ne. 2 .and. XFMFL(3) .ne. 3))
     1        then
          if (kfl .lt. NLEVL) go to 495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(16)
      nc     = 1
      call prmvoc (inum,ixyz,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(16) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  495 continue
c
c...Rotation direction
c
  500 MLEVL(NLEVL) = 5
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(17)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(17) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  595 continue
c
c...Use table rotations in xform
c
  600 do 620 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 1 .and. XFMFL(1) .eq. 1) go to 640
  620 continue
      if (kfl .lt. NLEVL) go to 695
      call errmsg ('NOTAPPLY',1,2)
      go to 8000
c
  640 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = XFMFL(11)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(11) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  695 continue
c
c...Coordinate transformations
c
  700 MLEVL(NLEVL) = 7
      if (XFMFL(1) .ne. 1 .or. (XFMFL(3) .ne. 5 .and. XFMFL(3) .ne. 6))
     1        then
          if (kfl .lt. NLEVL) go to 795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(12)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(12) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  795 continue
c
c...AUTO block handling
c
  800 ist    = 8
  810 inc    = ist    - 1
      do 890 i=ist,9,1
          inc    = inc    + 1
          if (XFMFL(1) .ne. 1) then
              if (kfl .lt. NLEVL) go to 895
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = XFMFL(inc)
          call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              XFMFL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  890 continue
  895 continue
c
c...Turn off xform prior to outputting point
c
 1000 MLEVL(NLEVL) = 10
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(13)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(13) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1095 continue
c
c...Rapid adjustments respect transformation
c
 1100 MLEVL(NLEVL) = 11
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(15)
      if (inum .eq. 0) inum = 2
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(15) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
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
c   SUBROUTINE:  mtaxfo (kfl,cmsg,kerr)
c
c   FUNCTION:  Transformation output prompt handling routine.
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
      subroutine mtaxfo (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (XFMREG,KPOSMP(0931)), (XFMCD ,KPOSMP(0946))
      equivalence (XFMFL ,KPOSMP(0969))
c
      integer*4 XFMFL(20),XFMCD(5),XFMREG(10)
c
      equivalence (DUMMY ,POSMAP(0003)), (XFMVL ,POSMAP(1189))
c
      real*8 DUMMY,XFMVL(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,iyesno(2),i,inc
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
      goto (110,110,110,110,510,510,510,510,510,510,510,
     1      510,510,510,1500,1600,1700), MLEVL(NLEVL)
c
c...Transformation codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 195
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 190 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = XFMCD(inc)
          rnum   = XFMVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              XFMCD(inc) = inum
              XFMVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Transformation registers
c
  500 ist    = 5
  510 inc    = ist    - 5
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 590 i=ist,14,1
          inc    = inc    + 1
          if ((XFMFL(3) .eq. 1 .and. (i .ge. 8 .and. i .le. 10)) .or.
     1        (XFMFL(3) .ne. 1 .and. i .ge. 11)) then
              if (kfl .lt. NLEVL) go to 590
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = XFMREG(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              XFMREG(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  590 continue
  595 continue
c
c...Non-Positioning code
c
 1500 MLEVL(NLEVL) = 15
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMCD(5)
      rnum   = XFMVL(5)
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMCD(5) = inum
          XFMVL(5) = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1595 continue
c
c...Output XYZ when cancelling translations
c...Output Xform block after motion
c
 1600 MLEVL(NLEVL) = 16
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(6)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(6) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1695 continue
c
c...Output XYZ when cancelling translations
c...Output Xform block after motion
c
 1700 MLEVL(NLEVL) = 17
      if (XFMFL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 1795
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = XFMFL(7)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          XFMFL(7) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1795 continue
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
