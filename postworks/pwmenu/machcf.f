c
c***********************************************************************
c
c   FILE NAME:  machcf.for
c   CONTAINS:
c               machcf  mcftyp  mcflin  mcfrot  mcfreg  mcfpos  mcfhom
c               mcflmt  mcfinf  mcfbld
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        machcf.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:56
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  machcf (kfl,cmsg,kerr)
c
c   FUNCTION:  Machine Configuration menu handling routine.
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
      subroutine machcf (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (IRTNUM,KPOSMP(1243)), (NUMINF,KPOSMP(1396))
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 IRTNUM,NUMINF,MACHTP
c
      integer*4 ifl,igo,iwlk
c
c...Specific level was requested
c...Go directly to that level
c
      kerr   = 0
      ifl    = kfl
      iwlk   = IWALK
      if (kfl .ge. NLEVL) go to 200
      if (IWALK .eq. 1) then
          NLEVL  = NLEVL  + 1
          go to 1000
      endif
c
c...Display Machine Configuration menu
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
      go to (1000,1100,1200,1300,1500,1600,1700,1800,
     -       1900,8000), igo
c
c...Walk through Machine Configuration
c
 1000 IWALK  = 1
c
c...Define type of machine
c
 1100 MLEVL(NLEVL-1) = 1
      call mcftyp (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Linear axis set-up
c
 1200 MLEVL(NLEVL-1) = 2
      call mcflin (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Rotary axis set-up
c
 1300 if (IRTNUM .eq. 0) then
          if (IWALK .eq. 1) go to 1500
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 3
      call mcfrot (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Blade Cutter set-up
c
c1400 if (MACHTP .ne. 3) then
c         if (IWALK .eq. 1) go to 1500
c         if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
c         call errmsg ('NOTAPPLY',1,2)
c         go to 7000
c     endif
c     MLEVL(NLEVL-1) = 4
c     call mcfbld (kfl,cmsg,kerr)
c     if (kerr .ne. 0) go to 8000
c     if (IWALK .eq. 0) go to 7000
c
c...Axes register description
c
 1500 MLEVL(NLEVL-1) = 4
      call mcfreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Current axes position
c
 1600 MLEVL(NLEVL-1) = 5
      call mcfpos (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Home position & Clearance plane
c
 1700 MLEVL(NLEVL-1) = 6
      call mcfhom (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Machine Limits
c
 1800 MLEVL(NLEVL-1) = 7
      call mcflmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Machine Interference Zones
c
 1900 if (NUMINF .eq. 0) then
          if (IWALK .eq. 1) go to 1995
          if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
          call errmsg ('NOTAPPLY',1,2)
          go to 7000
      endif
      MLEVL(NLEVL-1) = 8
      call mcfinf (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
 1995 continue
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
c   SUBROUTINE:  mcftyp (kfl)
c
c   FUNCTION:  Machine Type prompt handling routine.
c
c   INPUT:  kfl     I*4  D1  -  The number of levels that the user en-
c                               tered.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mcftyp (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IRTMAX,KPOSMP(0811))
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (IRTNUM,KPOSMP(1243)), (SPITAX,KPOSMP(1279))
      equivalence (NROT  ,KPOSMP(1366)), (NUMINF,KPOSMP(1396))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (IJKROT,KPOSMP(1739))
      equivalence (NOTOOL,KPOSMP(1802)), (LTHPCL,KPOSMP(1899))
      equivalence (MTPDYN,KPOSMP(4126))
      equivalence (MODCYL,KPOSMP(4204)), (MCYNUM,KPOSMP(4207))
      equivalence (ISXVAL,KPOSMP(4209))
c
      integer*4 MOTREG(24),MACHTP,NUMLIN(3),IRTNUM,IRTYPE(20),
     1          IRTWRK(20),NROT,NOTOOL,SPITAX,IRTMAX,NUMINF,LTHPCL(2),
     2          MTPDYN,MODCYL,MCYNUM,ISXVAL,IJKROT,IRDEAD(20)
c
      equivalence (SPIVEC,POSMAP(3583))
c
      real*8 SPIVEC(3)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,i,ist,inc,ityp,iyesno(2),ipt
c
      real*8 rnum(3),rdis,rmn(3),rmx(3)
c
      data rmn /-1.d0,-1.d0,-1.d0/, rmx /1.d0,1.d0,1.d0/
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,300,350,400,450,210,210,210,600,700,800,900), ist
c
c...Get type of machine
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = MACHTP
      nc     = -1
      call prmint (inum,nc,1,1,5,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c         if (LICOPT(6) .eq. 0 .and. inum .eq. 3) then
c         if (kfl .lt. NLEVL) go to 200
c         call errmsg ('NOTAPPLY',1,2)
c         go to 8000
c     endif
c
          MACHTP = inum
          call gtmach (MTPDYN)
c
c......Disable axes not used with Lathe or US cutting
c
          if (MACHTP .eq. 3) then
              IRTMAX = 3
c         MOTREG(22) = 4
c         MOTREG(23) = 5
c         MOTREG(24) = 6
          else if (MACHTP .eq. 2) then
              NUMLIN(1) = 1
              NUMLIN(2) = 0
              NUMLIN(3) = 1
              IRTNUM = 0
c
              do 150 i=3,8,1
                  MOTREG(i) = 0
  150         continue
              do 160 i=11,24,1
                  MOTREG(i) = 0
  160         continue
              NOTOOL = 2
          else
              IRTMAX = 4
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Milling lathe with polar interpolation
c
  300 if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 350
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = LTHPCL(1)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LTHPCL(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Milling lathe with cylindrical interpolation
c
  350 if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = LTHPCL(2)
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LTHPCL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Cylindrical interpolation in user defined plan
c
  400 if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = 2 - MODCYL
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MODCYL = 2 - inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Cylindrical interpolation defined in single plan
c
  450 if (MACHTP .ne. 4) then
          if (kfl .lt. NLEVL) go to 200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = MCYNUM
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MCYNUM = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Include axis value in code for select user plane command
c
c 470 if (MACHTP .ne. 4) then
c         if (kfl .lt. NLEVL) go to 200
c         call errmsg ('NOTAPPLY',1,2)
c         go to 8000
c     endif
c     MLEVL(NLEVL) = 6
c     iwrn   = 0
c     inum   = ISXVAL
c     call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
c     if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
c         if (iwrn .eq. 2) go to 7000
c         if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c         ISXVAL = inum
c         if (kfl .ge. NLEVL) go to 8000
c     endif
c
c...Get number of linear XYZ-axes
c
  200 ist    = 6
  210 if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 290
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      ipt   = ist - 6
      do 280 i=ist,8,1
          ipt    = ipt + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          nc     = 1
          inum   = NUMLIN(ipt)
          call prmint (inum,nc,1,0,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              NUMLIN(ipt) = inum
c
c......Disable inactive linear
c......axes registers
c
              inc    = (ipt - 1) * 4
              if (inum .eq. 0) then
                  MOTREG(inc+1) = 0
                  MOTREG(inc+2) = 0
              endif
              if (inum .le. 1) then
                  MOTREG(inc+3) = 0
                  MOTREG(inc+4) = 0
              endif
c
              if (kfl .ge. NLEVL) go to 8000
          endif
  280 continue
  290 continue
c
c...get GFM tool vector support
c
  600 MLEVL(NLEVL) = 9
      if (MACHTP .eq. 2 .or. MACHTP .eq. 4) then
          if (kfl .lt. NLEVL) go to 700
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IJKROT
      nc = -2
c
c...this prompt should be dynamic prompt
c...changed by Yurong 5/15/98
c
c      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IJKROT = inum
          if (inum .eq. 1) IRTNUM = 0
          NROT   = IRTNUM
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get number of rotary axes
c
  700 MLEVL(NLEVL) = 10
      if (MACHTP .eq. 2 .or. IJKROT .eq. 1) then
          if (kfl .lt. NLEVL) go to 790
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRTNUM
      nc     = 1
      call prmint (inum,nc,1,0,IRTMAX,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTNUM = inum
          NROT   = IRTNUM
          if (NROT .gt. 2) NROT = 2
c
c......Set up default rotary axes
c
          inc    = 0
          ityp   = 1
          do 740 i=1,IRTNUM,1
              if (IRTYPE(i) .eq. 0) IRTYPE(i) = ityp
              if (IRTYPE(i) .eq. 2) ityp = 2
              if (IRTWRK(i) .eq. 0) IRTWRK(i) = 1
              if (IRDEAD(i) .eq. 0) inc = inc + 1
  740     continue
c
          if (IRTNUM .ne. 0 .and. inc .eq. 0) then
              IRDEAD(1) = 0
              if (IRTNUM .ge. 2) IRDEAD(2) = 0
          endif
c
c......Disable unused rotary axes
c
          inc    = 15 + (IRTNUM-1) * 3
          do 750 i=IRTNUM+1,4,1
              IRTYPE(i) = 0
              IRDEAD(i) = 1
              IRTWRK(i) = 0
              if (i .gt. IRTMAX) go to 750
              MOTREG(inc+1) = 0
              MOTREG(inc+2) = 0
              MOTREG(inc+3) = 0
              inc    = inc    + 3
  750     continue
          if (kfl .ge. NLEVL) go to 8000
      endif
  790 continue
c
c...Get number of interference zones
c
  800 MLEVL(NLEVL) = 11
      iwrn   = 0
      inum   = NUMINF
      nc     = 1
      call prmint (inum,nc,1,0,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NUMINF = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get spindle tool axis vector
c
  900 MLEVL(NLEVL) = 12
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 8000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
      iwrn   = 0
      rnum(1) = SPIVEC(1)
      rnum(2) = SPIVEC(2)
      rnum(3) = SPIVEC(3)
  920 nc     = 3
      call prmrel (rnum,nc,3,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          rdis   = dsqrt(rnum(1)**2 + rnum(2)**2 + rnum(3)**2)
          if (nc .ne. 3 .or. rdis .eq. 0.) then
              call errmsg ('INVRESP',1,2)
              iwrn   = 1
              if (IMSCAN .eq. 2) go to 8000
              go to 920
          endif
          do 970 i=1,nc,1
              SPIVEC(i) = rnum(i) / rdis
  970     continue
          SPITAX = 1
          if (dabs(SPIVEC(1)) .gt. dabs(SPIVEC(3))) SPITAX = 3
          if (dabs(SPIVEC(2)) .gt. dabs(SPIVEC(4-SPITAX))) SPITAX = 2
          if (kfl .ge. NLEVL) go to 8000
      endif
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
c   SUBROUTINE:  mcflin (kfl,cmsg,kerr)
c
c   FUNCTION:  Linear axes Set-up prompt handling routine.
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
      subroutine mcflin (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205)), (LNCALC,KPOSMP(1208))
      equivalence (ACTBLN,KPOSMP(1336))
c
      integer*4 MACHTP,NUMLIN(3),ACTLIN(3),LNCALC(3),ACTBLN(3)
c
      equivalence (LNDIST,POSMAP(1251))
c
      real*8 LNDIST(3)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,i,iaxs(3),ipt(6),ilin(6)
c
      real*8 rnum,rmn,rmx
c
      data iaxs /28,29,70/, ipt /1,2,1,2,1,2/, ilin /1,1,2,2,3,3/
c
      data rmn /-99999999./, rmx /99999999./
c
c...This menu is only valid for Mills
c
      if (MACHTP .eq. 2) then
          if (kfl .lt. NLEVL) go to 8000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,410,410,410,710,710,710), MLEVL(NLEVL)
c
c...Get calculation type for multiple linear XYZ-axes
c
  100 ist    = 1
c
  110 do 180 i=ist,3,1
          if (NUMLIN(i) .ne. 2) then
              if (kfl .lt. NLEVL) go to 180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = LNCALC(i)
          nc     = -1
          call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LNCALC(i) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...Get distance between primary and secondary XYZ axes
c
  400 ist    = 4
c
  410 do 480 i=ist,6,1
          if (NUMLIN(i-3) .ne. 2 .or. LNCALC(i-3) .ne. 1) then
              if (kfl .lt. NLEVL) go to 480
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          rnum   = LNDIST(i-3)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              LNDIST(i-3) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  480 continue
c
c...Get default axis
c...Primary or Secondary
c
  700 ist    = 7
c
  710 do 780 i=ist,9,1
          if (NUMLIN(i-6) .ne. 2) then
              if (kfl .lt. NLEVL) go to 780
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = ACTLIN(i-6)
          if (ACTBLN(i-6) .eq. 1) inum = 3
          call prmvoc (inum,iaxs,3,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              if (inum .ne. 3) then
                  ACTLIN(i-6) = inum
                  ACTBLN(i-6) = 0
              else
                  ACTBLN(i-6) = 1
              end if
              if (kfl .ge. NLEVL) go to 8000
          endif
  780 continue
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
c   SUBROUTINE:  mcfrot (kfl,cmsg,kerr)
c
c   FUNCTION:  Rotary axes Set-up prompt handling routine.
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
      subroutine mcfrot (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201)), (IZSWIV,KPOSMP(1224))
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (IRTMOD,KPOSMP(1284)), (IRTSCL,KPOSMP(1288))
      equivalence (NCONTB,KPOSMP(1347))
      equivalence (IRTCLW,KPOSMP(1292)), (NROT  ,KPOSMP(1366))
      equivalence (IRTDUP,KPOSMP(1413))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTYPE,KPOSMP(1486))
      equivalence (IRTWRK,KPOSMP(1506)), (IJKROT,KPOSMP(1739))
      equivalence (POSFDF,KPOSMP(3208)), (NCPLAN,KPOSMP(4226))
c
      integer*4 IRTNUM,IRTYPE(20),IRDEAD(20),IRTWRK(20),IRTMOD(4),
     1          IRTSCL(4),IRTCLW(4),NCONTB,NROT,IZSWIV,POSFDF,
     2          MACHTP,NCPLAN(3),IJKROT,IRTDUP(4)
c
      equivalence (ROTCRM,POSMAP(1439)), (TABORG,POSMAP(5374))
c
      real*8 TABORG(3,20),ROTCRM(4)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,i,itab(2),inc,isw,iary(4),
     1          imode(2),iscal(2),idir(2),imn(2),imx(2),iyesno(2),
     2          j
c
      real*8 rnum(3),rmn(3),rmx(3)
c
      character*1 lax(3)
      character*80 sbuf
c
      data iyesno /13,12/
      data itab /10,11/, imode /32,33/, iscal /34,35/, idir /36,37/
      data imn /1,1/, imx /4,4/
      data rmn /-99999999,-99999999,-99999999/
      data rmx /99999999,99999999,99999999/
      data lax /'X','Y','Z'/
c
c...This machine does not have
c...any rotary axes
c
      if (IRTNUM .eq. 0) then
          if (kfl .lt. NLEVL) go to 8000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,510,510,510,510,900,1000,1110,1110,1110,
     1      1110,1510,1510,1510,1510,1910,1910,1910,1910,2310,2310,2310,
     2      2310,2700,2810,2810,2810,2810,3210,3210,3210,3210,3600,3710,
     3      3710,3710,3710), MLEVL(NLEVL)
c
c...Get rotary axes types (Table,Head)
c
  100 ist    = 1
c
  110 if (ist .gt. IRTNUM) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 180 i=ist,IRTNUM,1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = IRTYPE(i)
  120     nc     = -2
          call prmvoc (inum,itab,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Make sure tables and heads
c......are not intermixed
c
              do 140 j=1,i-1,1
                  if (IRTYPE(j) .gt. inum) then
                      iwrn   = 1
                      call errmsg ('TAB1ST',1,2)
                      if (IMSCAN .eq. 2) go to 8000
                      go to 120
                  endif
  140         continue
              IRTYPE(i) = inum
              if (kfl .ge. NLEVL) go to 190
          endif
  180 continue
c
c......Verify array order is correct
c
  190 if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          isw    = 0
          do 192 i=1,IRTNUM-1,1
              if (IRTYPE(i) .gt. IRTYPE(i+1)) then
                  inc    = IRTYPE(i+1)
                  IRTYPE(i+1) = IRTYPE(i)
                  IRTYPE(i) = inc
c
                  inc    = IRTWRK(i+1)
                  IRTWRK(i+1) = IRTWRK(i)
                  IRTWRK(i) = inc
c
                  inc    = IRDEAD(i+1)
                  IRDEAD(i+1) = IRDEAD(i)
                  IRDEAD(i) = inc
                  isw    = 1
              endif
  192     continue
          if (isw .eq. 1) go to 190
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...set default cylindrical plane if applicable
c...on the first specified table (rider)
c
      if (MACHTP .eq. 4) then
         i     = 1
  220    if (IRTYPE(i) .eq. 1) then
             NCPLAN(1) = IRTWRK(i) + 3
             NCPLAN(2) = IRTWRK(i)
             NCPLAN(3) = IRTWRK(i) + 1
             if (NCPLAN(3) .gt. 3) NCPLAN(3) = NCPLAN(3) - 3
         end if
      end if
c
c...Get rotation axes
c
  500 ist    = 5
c
  510 inc    = ist    - 5
      if (inc .ge. IRTNUM) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 580 i=ist,ist+IRTNUM-1,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          if (IRTWRK(inc) .eq. 0) IRTWRK(inc) = 1
          inum   = IRTWRK(inc)
          call prmstr (inum,lax,3,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IRTWRK(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  580 continue
c
c...Does Z-axis swivel with rotary head
c
  900 MLEVL(NLEVL) = 9
      do 910 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 2) go to 920
  910 continue
c
c......No rotary head
c
      IZSWIV = 2
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      else
          go to 995
      endif
c
c......Get user's response
c
  920 iwrn   = 0
      inum   = IZSWIV
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IZSWIV = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  995 continue
c
c...are tables connected if 2 or more
c
 1000 MLEVL(NLEVL) = 10
      inum   = IRTNUM
      do 1010 i=1,IRTNUM,1
          if (IRTYPE(i) .eq. 2) inum = inum - 1
 1010 continue
      if (inum .gt. 1) go to 1020
c
c......Less than 2 rotary tables
c
      NCONTB = 1
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      else
          go to 1100
      endif
c
c......Get user's response
c
 1020 iwrn   = 0
      inum   = NCONTB
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          NCONTB = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get number of physical rotary axes
c
 1100 ist    = 11
 1110 inc    = ist    - 11
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 1195
         endif
      endif
      do 1190 i=ist,14,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              if (inc .gt. IRTNUM) go to 1195
              go to 1190
          endif
c
c......Get user input
c
          iwrn   = 0
          inum   = IRTDUP(inc)
          nc     = 1
          call prmint (inum,nc,1,1,9,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IRTDUP(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
c
 1195 continue
c
c...Get rotary axes modes (Contouring,Positioning)
c
 1500 ist    = 15
 1510 inc    = ist    - 15
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 1595
         endif
      endif
      do 1590 i=ist,18,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 1595
          endif
c
c......Get user input
c
          iwrn   = 0
          inum   = IRTMOD(inc)
          nc     = -2
          call prmvoc (inum,imode,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IRTMOD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1590 continue
c
 1595 continue
c
c...Get contouring axes scales (Linear,Rotary)
c
 1900 ist    = 19
 1910 inc    = ist    - 19
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 1995
         endif
      endif
      do 1990 i=ist,22,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c......and this is a contouring axes
c
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              if (inc .gt. IRTNUM) go to 1995
              go to 1990
          endif
c
c......Get user input
c
          iwrn   = 0
          if (IRTSCL(inc) .lt. 1 .or. IRTSCL(inc) .gt. 2)
     1            IRTSCL(inc) = 1
          inum   = IRTSCL(inc)
          call prmvoc (inum,iscal,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IRTSCL(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1990 continue
c
 1995 continue
c
c...Get initial orientation of rotary axes
c...Axx.xx,Cxx.xx etc.
c
 2300 ist    = 23
c
 2310 inc    = ist    - 23
      if (inc .ge. IRTNUM) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2380 i=ist,ist+IRTNUM-1,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          call mcfgor (inc,sbuf)
 2320     nc     = 1
          call prmstr (nc,sbuf,0,kfl,ilod,iwrn,cmsg,kerr)

          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              call mcfsor (inc,sbuf,iwrn)
              if (iwrn .eq. 1) then
                  call errmsg ('INVRSP',1,2)
                  if (IMSCAN .eq. 2) go to 8000
                  go to 2320
              endif
              if (kfl .ge. NLEVL) go to 8000
          endif
 2380 continue
 2395 continue
c
c...Get positioning axis feedrate control
c
 2700 MLEVL(NLEVL) = 27
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 2795
         endif
      endif
      do 2710 i=1,IRTNUM,1
          if (IRTMOD(i) .eq. 2) go to 2720
 2710 continue
c
c......No positioning axis
c
      if (kfl .ge. NLEVL) then
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      else
          go to 2795
      endif
c
c......Get user's response
c
 2720 iwrn   = 0
      inum   = POSFDF
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          POSFDF = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 2795 continue
c
c...Get circumference of rotary axes
c...that move on a linear scale
c
 2800 ist    = 28
 2810 inc    = ist    - 28
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 2895
         endif
      endif
      do 2890 i=ist,31,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c......101193 now is available with both scales
c
c         if (inc .gt. IRTNUM .or. IRTSCL(inc) .ne. 1) then
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 2895
          endif
c
c......Get user input
c
          iwrn   = 0
          rnum(1) = ROTCRM(inc) * 360.D0
          nc     = 1
          call prmrel (rnum,nc,1,1.D0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ROTCRM(inc) = rnum(1) / 360.D0
              if (kfl .ge. NLEVL) go to 8000
           endif
 2890 continue
c
 2895 continue
c
c...Get direction for a CLW move
c
 3200 ist    = 32
 3210 inc    = ist    - 32
      if (IJKROT .eq. 1) then
         if (kfl .ge. NLEVL) then
            call errmsg ('NOTAPPLY',1,2)
            go to 8000
         else
            go to 3295
         endif
      endif
      do 3290 i=ist,35,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              if (inc .gt. IRTNUM) go to 3295
              go to 3290
          endif
c
c......Get user input
c
          iwrn   = 0
          if (IRTCLW(inc) .lt. 1 .or. IRTCLW(inc) .gt. 2)
     1            IRTCLW(inc) = 1
          inum   = IRTCLW(inc)
          call prmvoc (inum,idir,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              IRTCLW(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
 3290 continue
c
 3295 continue
c
c...Get default active rotary axes
c
 3600 MLEVL(NLEVL) = 36
c
c......Automatically set the active axes
c......when there are 2 or less rotaries
c
      if (IRTNUM .le. 2 .and. kfl .lt. NLEVL) then
          NROT   = IRTNUM
          IRDEAD(1) = 0
          IRDEAD(2) = 0
          if (IRTNUM .eq. 1) IRDEAD(2) = 1
          IRDEAD(3) = 1
          IRDEAD(4) = 1
          if (kfl .ge. NLEVL) then
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          go to 3695
      endif
c
c......Get the currently active axes
c
      inc    = 0
      do 3610 i=1,4,1
          if (IRDEAD(i) .eq. 0) then
              inc    = inc    + 1
              iary(inc) = i
              if (inc .eq. 2) go to 3615
          endif
 3610 continue
c
c......Get user's response
c
 3615 iwrn   = 0
      imx(1) = IRTNUM
      imx(2) = IRTNUM
 3620 call prmint (iary,inc,2,imn,imx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Check validity of answer
c
          IRDEAD(1) = 1
          IRDEAD(2) = 1
          IRDEAD(3) = 1
          IRDEAD(4) = 1
          if (inc .eq. 1) then
              IRDEAD(iary(1)) = 0
          else if (inc .eq. 2) then
              if (iary(1) .eq. iary(2)) then
                  iwrn   = 1
                  call errmsg ('INVRSP',1,2)
                  if (IMSCAN .eq. 2) go to 8000
                  go to 3620
              endif
              IRDEAD(iary(1)) = 0
              IRDEAD(iary(2)) = 0
          endif
          if (inc .lt. 1) iary(1) = 0
          if (inc .lt. 2) iary(2) = 0
          do 3650 i=1,IRTNUM,1
              if (i .ne. iary(1) .and. i .ne. iary(2)) IRDEAD(i) = 1
 3650     continue
          NROT   = inc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
 3695 continue
c
c...Get center of rotary tables
c
 3700 ist    = 37
 3710 inc    = ist    - 37
      do 3790 i=ist,40,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
c
c......Verify that this machine has this rotary axis
c
          if (inc .gt. IRTNUM) then
              if (kfl .ge. NLEVL) then
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
              go to 3795
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          rnum(1) = TABORG(1,inc)
          rnum(2) = TABORG(2,inc)
          rnum(3) = TABORG(3,inc)
          nc     = 3
          call prmrel (rnum,nc,3,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              do 3770 j=1,nc,1
                  TABORG(j,inc) = rnum(j)
 3770         continue
              do 3780 j=nc+1,3,1
                  TABORG(j,inc) = 0.
 3780         continue
          endif
 3790 continue
c
 3795 continue
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
c   SUBROUTINE:  mcfreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Axes registers prompt handling routine.
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
      subroutine mcfreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IJKREG,KPOSMP(0827))
      equivalence (TAXCD ,KPOSMP(1081))
      equivalence (NTAXCD,KPOSMP(1090)), (MACHTP,KPOSMP(1201))
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IJKROT,KPOSMP(1739)), (LTHPCL,KPOSMP(1899))
      equivalence (MILREG,KPOSMP(4106)), (LPCREG,KPOSMP(4108))
      equivalence (LTHREG,KPOSMP(4110)), (LTCREG,KPOSMP(4195))
      equivalence (LTPREG,KPOSMP(4201)), (MODCYL,KPOSMP(4204))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (LUNREG,KPOSMP(4205)), (ISRCYL,KPOSMP(4206))
      equivalence (ISRVAL,KPOSMP(4208))
c
      integer*4 MOTREG(24),NUMLIN(3),IRTNUM,TAXCD(3,3),NTAXCD(3),
     1          MACHTP,MILREG,LTHPCL(2),LPCREG(2),LTHREG,LTCREG(3),
     2          LTPREG(2),MODCYL,LUNREG,ISRCYL,ISRVAL,IJKROT,
     3          IJKREG(3)
c
      equivalence (DUMMY ,POSMAP(0003)), (TAXVL ,POSMAP(1173))
      equivalence (GLMCDV,POSMAP(4602)), (TLMCDV,POSMAP(4880))
      equivalence (TLLCDV,POSMAP(4881))
c
      real*8 DUMMY,TAXVL(3,3),GLMCDV(2),TLMCDV,TLLCDV
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 nc,iwrn,inum,ilod,ist,i,inc(24),isub(12),ipt,
     -          iyesno(2)
c
      real*8 rnum
c
      data inc  /1,1,1,1, 2,2,2,2, 3,3,3,3, 1,1,1, 2,2,2, 3,3,3, 4,4,4/
      data isub /1,1,2,2, 1,1,2,2, 1,1,2,2/
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,110,110,110,110,110,110,110,110,110,110,
     1      110,110,110,110,110,110,110,110,110,110,2010,2010,2010,
     2      2210,2210,2410,2410,2410), ist
c
c...Get registers for active axes
c
  100 ist    = 1
c
  110 do 180 i=ist,24,1
c
c......Verify that this machine has this axis
c
          iwrn   = 0
          if (i .le. 12) then
              if (NUMLIN(inc(i)) .lt. isub(i)) iwrn = 1
          else
              if (inc(i) .gt. IRTNUM) iwrn = 1
              if (i .ne. (10+inc(i)*3) .and. IJKROT .eq. 1) iwrn = 1
          endif
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 180
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......Get user's input
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = MOTREG(i)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              MOTREG(i) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  180 continue
c
c...Get tool axis vector registers
c
 2000 ist    = 25
 2010 ipt    = ist - 25
      if (IJKROT .eq. 2) then
          if (kfl .lt. NLEVL) go to 2200
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2050 i=ist,27,1
         ipt    = ipt + 1
         MLEVL(NLEVL) = i
         iwrn   = 0
         inum   = IJKREG(ipt)
         rnum   = DUMMY
         nc     = 1
         call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
         if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
             if (iwrn .eq. 2) go to 7000
             if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
             IJKREG(ipt) = inum
             if (kfl .ge. NLEVL) go to 8000
         endif
 2050 continue
c
c...Get Polar second axis registers
c
 2200 ist    = 28
 2210 ipt    = ist - 28
      if (MACHTP .ne. 4 .or. LTHPCL(1) .ne. 1) then
          if (kfl .lt. NLEVL) go to 2400
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2250 i=ist,29,1
         ipt    = ipt + 1
         MLEVL(NLEVL) = i
         iwrn   = 0
         inum   = LTPREG(ipt)
         rnum   = DUMMY
         nc     = 1
         call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
         if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
             if (iwrn .eq. 2) go to 7000
             if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
             LTPREG(ipt) = inum
             if (kfl .ge. NLEVL) go to 8000
         endif
 2250 continue
c
c...Get Cylindrical rotary axis registers
c
 2400 ist    = 30
 2410 ipt    = ist - 30
      if (MACHTP .ne. 4 .or. LTHPCL(2) .ne. 1) then
          if (kfl .lt. NLEVL) go to 8000
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 2450 i=ist,32,1
         ipt    = ipt + 1
         MLEVL(NLEVL) = i
         iwrn   = 0
         inum   = LTCREG(ipt)
         rnum   = DUMMY
         nc     = 1
         call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
         if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
             if (iwrn .eq. 2) go to 7000
             if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
             LTCREG(ipt) = inum
             if (kfl .ge. NLEVL) go to 8000
         endif
 2450 continue
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
c   SUBROUTINE:  mcfpos (kfl,cmsg,kerr)
c
c   FUNCTION:  Current Axes position prompt handling routine.
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
      subroutine mcfpos (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
c
      integer*4 NUMLIN(3),IRTNUM
c
      equivalence (AXSSTO,POSMAP(1425))
c
      real*8 AXSSTO(10)
c
      integer*4 nc,iwrn,ilod,i,ist,inc,inum
c
      real*8 rnum,rmn,rmx
c
      data rmn /-99999999/, rmx /99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
c
c...Get Axes position
c
  100 do 190 i=ist,10,1
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
          iwrn   = 0
          rnum   = AXSSTO(i)
          nc     = 1
          call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              AXSSTO(i) = rnum
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
c   SUBROUTINE:  mcfhom (kfl,cmsg,kerr)
c
c   FUNCTION:  Axes home position & clearance plane prompt handling
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
      subroutine mcfhom (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IRETMD,KPOSMP(1340))
c
      integer*4 NUMLIN(3),IRTNUM,IRETMD(2)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (HOME  ,POSMAP(0151)), (CLRPLN,POSMAP(0161))
c
      real*8 HOME(10),CLRPLN(4),DUMMY
c
      integer*4 nc,iwrn,ilod,i,ist,inc,inum,ipt,ipln(2),irmod(2)
c
      real*8 rnum(4),rmn(4),rmx(4),rdis
c
      data ipln /65,66/, irmod /72,66/
      data rmn /-99999999,-99999999,-99999999,-99999999/
      data rmx /99999999,99999999,99999999,99999999/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,110,110,110,110,110,110,110,110,1100,1200,1300,1400,
     1      1500), MLEVL(NLEVL)
c
c...Get Axes home position
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,10,1
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
              if (kfl .lt. NLEVL) go to 190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          rnum(1) = HOME(inc)
          nc     = 1
          call prmrel (rnum(1),nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              HOME(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Get clearance plane mode
c
 1100 MLEVL(NLEVL) = 11
      iwrn   = 0
      inum   = 1
      if (CLRPLN(1) .eq. DUMMY) inum = 2
      nc     = -2
      call prmvoc (inum,ipln,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          if (inum .eq. 1) then
              if (CLRPLN(1) .eq. DUMMY) then
                  CLRPLN(1) = 0.
                  CLRPLN(2) = 0.
                  CLRPLN(3) = 1.
                  CLRPLN(4) = 0.
              endif
          else
              if (CLRPLN(1) .ne. DUMMY) CLRPLN(2) = 0.
              CLRPLN(1) = DUMMY
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get clearance plane
c
 1200 MLEVL(NLEVL) = 12
      if (CLRPLN(1) .eq. DUMMY) then
          if (kfl .lt. NLEVL) go to 1295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum(1) = CLRPLN(1)
      rnum(2) = CLRPLN(2)
      rnum(3) = CLRPLN(3)
      rnum(4) = CLRPLN(4)
 1220 nc     = 4
      call prmrel (rnum,nc,4,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
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
              go to 1220
          endif
          do 1270 i=1,3,1
              CLRPLN(i) = rnum(i) / rdis
 1270     continue
          CLRPLN(4) = rnum(4)
          if (kfl .ge. NLEVL) go to 8000
      endif
 1295 continue
c
c...Get retract distance
c
 1300 MLEVL(NLEVL) = 13
      if (CLRPLN(1) .ne. DUMMY) then
          if (kfl .lt. NLEVL) go to 1395
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum(1) = CLRPLN(2)
      nc     = 1
      call prmrel (rnum,nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          CLRPLN(2) = rnum(1)
          if (kfl .ge. NLEVL) go to 8000
      endif
 1395 continue
c
c...Retract tool tip or tool stop
c
 1400 MLEVL(NLEVL) = 14
      if (CLRPLN(1) .eq. DUMMY) then
          if (kfl .lt. NLEVL) go to 1495
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRETMD(1)
      call prmvoc (inum,irmod,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRETMD(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1495 continue
c
c...Get retraction axis
c
 1500 MLEVL(NLEVL) = 15
      if (CLRPLN(1) .eq. DUMMY) then
          if (kfl .lt. NLEVL) go to 1595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = IRETMD(2)
      nc     = 1
c...only 4 choices in 'postmenu.txt' shows (from 1)
c...Yurong
c...      call prmint (inum,nc,1,0,4,kfl,ilod,iwrn,cmsg,kerr)
      call prmint (inum,nc,1,1,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRETMD(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
 1595 continue
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
c   SUBROUTINE:  mcflmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Machine Limits prompt handling routine.
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
      subroutine mcflmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (RTRMCD,KPOSMP(0093))
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (IRTSCL,KPOSMP(1288))
c
      integer*4 NUMLIN(3),IRTNUM,RTRMCD(4),IRTSCL(4)
c
      equivalence (LIMITS,POSMAP(1254)), (RTRMMX,POSMAP(4360))
      equivalence (RTRMVL,POSMAP(4364))
c
      real*8 LIMITS(2,10),RTRMMX(4),RTRMVL(4)
c
      integer*4 nc,iwrn,ilod,i,ist,nwds,inc,inum
c
      real*8 rnum(10),rmn,rmx
c
      character*20 sbuf
c
      data rmn /0./, rmx /99999999.0/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,110,110,110,1110,1110,1110,
     1       1110,1510,1510,1510,1510), MLEVL(NLEVL)
c
c...Get Machine limits
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
              if (NUMLIN(inc) .lt. inum .and. (NUMLIN(inc) .ne. 0 .or.
     1            inum .ne. 1)) iwrn = 1
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
          inc    = (i-1) * 2
          rnum(1) = LIMITS(1,i)
          rnum(2) = LIMITS(2,i)
          call getr8s (rnum,2,sbuf,nc)
  120     call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (kerr .ne. 0) go to 8000
              if (nc .eq. -1) go to 8000
              if (nc .eq. -2) go to 7000
c
c......Check validity of answer
c
              call getr8n (sbuf,nc,rnum,nwds,iwrn)
              if (iwrn .eq. 1 .or. nwds .gt. 2 .or.
     1            rnum(1) .gt. rnum(2)) then
                  iwrn   = 1
                  call errmsg ('INVRSP',1,2)
                  if (IMSCAN .eq. 2) go to 8000
                  go to 120
              endif
c
c......Answer is valid
c
              LIMITS(1,i) = rnum(1)
              LIMITS(2,i) = rnum(2)
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
  195 continue
c
c...Get maximum Register values
c
 1100 ist    = 11
 1110 do 1190 i=ist,14,1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inc    = i      - 10
          if (inc .gt. IRTNUM .or. IRTSCL(inc) .ne. 1) then
              if (kfl .lt. NLEVL) go to 1190
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          rnum(1) = RTRMMX(inc)
          nc     = 1
          call prmrel (rnum(1),nc,1,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RTRMMX(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
 1190 continue
 1195 continue
c
c...Get Register reset codes
c
 1500 ist    = 15
 1510 do 1590 i=ist,18,1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inc    = i      - 14
          if (inc .gt. IRTNUM .or. IRTSCL(inc) .ne. 1) then
              if (kfl .lt. NLEVL) go to 1590
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          inum   = RTRMCD(inc)
          rnum(1) = RTRMVL(inc)
          nc     = 1
          call prmcod (inum,rnum(1),nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              RTRMCD(inc) = inum
              RTRMVL(inc) = rnum(1)
              if (kfl .ge. NLEVL) go to 8000
          endif
 1590 continue
 1595 continue
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
c   SUBROUTINE:  mcfinf (kfl,cmsg,kerr)
c
c   FUNCTION:  Interference zones prompt handling routine.
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
      subroutine mcfinf (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (NUMINF,KPOSMP(1396))
c
      integer*4 NUMLIN(3),IRTNUM,NUMINF
c
      equivalence (DUMMY ,POSMAP(0003)), (INFZON,POSMAP(0071))
c
      real*8 INFZON(2,10,4),DUMMY
c
      integer*4 nc,iwrn,ilod,i,ist,nwds,inc,inum,j,inc1,isub,ipt,ilev
c
      real*8 rnum(10)
c
      character*20 sbuf
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
c
c...Get Interference zones
c
  100 isub   = (ist-1) / 10
      inc    = ist    - (isub*10)
      ilev   = ist
      do 190 i=ist,40,10
          isub   = isub   + 1
          inc1   = 0
          if (isub .gt. NUMINF) then
              if (kfl .lt. NLEVL) go to 195
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
          do 170 j=inc,10,1
              MLEVL(NLEVL) = ilev   + inc1
              inc1   = inc1   + 1
              iwrn   = 0
c
c......Verify that this machine has this axis
c
              if (j .le. 6) then
                  ipt    = (j-1) / 2 + 1
                  inum   = 2 + (j - ipt*2)
                  if (NUMLIN(ipt) .lt. inum) iwrn = 1
c
              else
                  if (j-6 .gt. IRTNUM) iwrn = 1
              endif
c
              if (iwrn .eq. 1) then
                  if (kfl .lt. NLEVL) go to 170
                  call errmsg ('NOTAPPLY',1,2)
                  go to 8000
              endif
c
c......This axis is present
c......Set up default answer & get user's input
c
              rnum(1) = INFZON(1,j,isub)
              rnum(2) = INFZON(2,j,isub)
              if (rnum(1) .eq. DUMMY) then
                  nc     = 0
              else if (rnum(2) .eq. DUMMY) then
                  call getr8s (rnum,1,sbuf,nc)
              else
                  call getr8s (rnum,2,sbuf,nc)
              endif
  120         call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
              if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
                  if (kerr .ne. 0) go to 8000
                  if (nc .eq. -1) go to 8000
                  if (nc .eq. -2) go to 7000
c
c......Check validity of answer
c
                  call getr8n (sbuf,nc,rnum,nwds,iwrn)
                  if (nwds .eq. 0) then
                      rnum(1) = DUMMY
                      rnum(2) = DUMMY
                  else if (nwds .eq. 1) then
                      rnum(2) = DUMMY
                  endif
                  if (iwrn .eq. 1 .or. nwds .gt. 2 .or.
     1                (rnum(1) .gt. rnum(2) .and. rnum(2) .ne. DUMMY))
     2                    then
                      iwrn   = 1
                      call errmsg ('INVRSP',1,2)
                      if (IMSCAN .eq. 2) go to 8000
                      go to 120
                  endif
c
c......Answer is valid
c
                  INFZON(1,j,isub) = rnum(1)
                  INFZON(2,j,isub) = rnum(2)
                  if (kfl .ge. NLEVL) go to 8000
              endif
  170     continue
          ilev   = (i-1) / 10 * 10 + 11
          inc    = 1
  190 continue
  195 continue
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
c   SUBROUTINE:  mcfgor (kinc,cbuf)
c
c   FUNCTION:  Formats a string containing the initial orientation of a
c              rotary axis when it does not rotate about a major axis.
c
c   INPUT:  kinc    I*4  D1  -  Which rotary axis to format the rotations
c                               for.
c
c   OUTPUT: cbuf    C*n  D1  -  Formatted rotations.
c
c
c***********************************************************************
c
      subroutine mcfgor (kinc,cbuf)
c
      include 'post.inc'
c
      integer*4 kinc
c
      character*(*) cbuf
c
      equivalence (ISCWRK,KPOSMP(1453))
c
      integer*4 ISCWRK(2,4)
c
      equivalence (SCROT, POSMAP(2119))
c
      real*8 SCROT(2,4)
c
      integer*4 nc1,nc2
c
      character*1 laxs(3)
      character*20 buf1,buf2
c
      data laxs /'X','Y','Z'/
c
c...Rotary axis rotates about a major axis
c
      if (ISCWRK(1,kinc) .eq. 0) then
          cbuf = ' '
c
c...Format single rotary adjustment
c
      else if (ISCWRK(2,kinc) .eq. 0) then
          call rtoc (SCROT(1,kinc),buf1,nc1)
          cbuf = laxs(ISCWRK(1,kinc)) // buf1(1:nc1)
      else
          call rtoc (SCROT(1,kinc),buf1,nc1)
          call rtoc (SCROT(2,kinc),buf2,nc2)
          cbuf = laxs(ISCWRK(1,kinc)) // buf1(1:nc1) // ',' //
     1           laxs(ISCWRK(2,kinc)) // buf2(1:nc2)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mcfsor (kinc,cbuf,kerr)
c
c   FUNCTION:  Parses a string containing the initial orientation of a
c              rotary axis when it does not rotate about a major axis.
c
c   INPUT:  kinc    I*4  D1  -  Which rotary axis to parse the rotations
c                               for.
c
c           cbuf    C*n  D1  -  Formatted rotations to parse.
c
c   OUTPUT: kerr    I*4  D1  -  Returns an error if the string is not
c                               in the correct format.
c
c***********************************************************************
c
      subroutine mcfsor (kinc,cbuf,kerr)
c
      include 'post.inc'
c
      integer*4 kinc,kerr
c
      character*(*) cbuf
c
      equivalence (ISCWRK,KPOSMP(1453))
c
      integer*4 ISCWRK(2,4)
c
      equivalence (SCROT, POSMAP(2119))
c
      real*8 SCROT(2,4)
c
      integer*4 iaxs(2),nc,ist,ipt,index,inc
c
      real*8 rval(2)
c
      character*3 laxs
      character*80 sbuf
c
      data laxs /'XYZ'/
c
c...Initialize routine
c
      ist    = 1
      ipt    = 1
      iaxs(1) = 0
      iaxs(2) = 0
      rval(1) = 0.
      rval(2) = 0.
      call remspc (cbuf,sbuf,nc)
      call touppr (sbuf(1:nc),sbuf)
c
c...No rotations given
c
      if (nc .eq. 0) then
          ISCWRK(1,kinc) = 0
          ISCWRK(2,kinc) = 0
          go to 8000
      endif
c
c...Parse the register input
c
  100 iaxs(ipt) = index(laxs,sbuf(ist:ist))
      if (iaxs(ipt) .eq. 0) go to 9000
      ist    = ist    + 1
      inc    = index(cbuf(ist:nc),',')
      if (inc .ne. 0 .and. ipt .eq. 2) go to 8000
      if (inc .eq. 0) inc = nc + 1
      call ctor (cbuf(ist:ist+inc-2),rval(ipt),kerr)
      if (kerr .ne. 0) go to 9000
      ipt    = ipt    + 1
      ist    = ist    + inc
      if (ist .le. nc) go to 100
c
c...Assign the rotations
c
      ISCWRK(1,kinc) = iaxs(1)
      SCROT(1,kinc) = rval(1)
      ISCWRK(2,kinc) = iaxs(2)
      SCROT(2,kinc) = rval(2)
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 kerr   = 1
      go to 8000
      end
