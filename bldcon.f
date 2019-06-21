c
c***********************************************************************
c
c   FILE NAME:  bladcf.for
c   CONTAINS:
c               bladcf  bldcon  bldalc  bldalm  bldsmo
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        bldcon.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:54
c
c***********************************************************************c
c
c***********************************************************************
c
c   SUBROUTINE:  bladcf (kfl,cmsg,kerr)
c
c   FUNCTION:  Blade Rotary axis Set-up prompt handling routine.
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
      subroutine bladcf (kfl,cmsg,kerr)
c
      integer*4 kfl,kerr
      character*(*) cmsg
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 MACHTP
c
      character*20 sbuf
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
c...Display US Blade Configuration menu
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
c...Walk through Ultrasonic Blade configuration.
c
 1000 IWALK  = 1
c
c...Define Blade Cutter set-up
c
 1100 MLEVL(NLEVL-1) = 1
      call bldcon (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Blade Alignment
c
 1200 MLEVL(NLEVL-1) = 2
      call bldalc (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Blade Positioning
c
 1300 MLEVL(NLEVL-1) = 3
      call bldalm (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Smooth command set-up
c
 1400 MLEVL(NLEVL-1) = 4
      call bldsmo (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Vacuum Pods
c
 1500 MLEVL(NLEVL-1) = 5
      call mpspod (kfl,cmsg,kerr)
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
c   SUBROUTINE:  bldcon (kfl,cmsg,kerr)
c
c   FUNCTION:  Blade Rotary axis Set-up prompt handling routine.
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
      subroutine bldcon (kfl,cmsg,kerr)
c
      integer*4 kfl,kerr
      character*(*) cmsg
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IRTSCL,KPOSMP(1288))
      equivalence (IRTCLW,KPOSMP(1292)), (NROT  ,KPOSMP(1366))
      equivalence (MACHTP,KPOSMP(1201))
c
      integer*4 MOTREG(24),MACHTP,IRTSCL(4),IRTCLW(4),NROT
c
      equivalence (DUMMY ,POSMAP(0003)), (BLDVEC,POSMAP(3589))
      equivalence (AXSSTO,POSMAP(1425)), (BLDELT,POSMAP(3593))
      equivalence (LIMITS,POSMAP(1254))
c
      real*8 LIMITS(2,10),AXSSTO(10),DUMMY,BLDELT,BLDVEC(3)
c
      integer*4 nc,iwrn,inum,ilod,i,ist,inc,ipt,ityp,iscal(2),
     -          nwds
c
      real*8 r,rnum(3),rdis,rmn(3),rmx(3),pmn,pmx,dmn,dmx
c
      character*20 sbuf
c
      data dmn /0.d0/, dmx /180.0/
      data pmn /-99999999/, pmx /99999999/
      data rmn /-1.d0,-1.d0,-1.d0/, rmx /1.d0,1.d0,1.d0/
      data iscal /34,35/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      ist    = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,300,400,500,600), MLEVL(NLEVL)
c
c...Get Blade direction vector
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      rnum(1) = BLDVEC(1)
      rnum(2) = BLDVEC(2)
      rnum(3) = BLDVEC(3)
  120 nc     = 3
      call prmrel (rnum,nc,3,rmn,rmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          rdis   = dsqrt(rnum(1)**2 + rnum(2)**2 + rnum(3)**2)
          if (nc .ne. 3 .or. rdis .eq. 0.) then
              call errmsg ('INVRESP',1,2)
              iwrn   = 1
              if (IMSCAN .eq. 2) go to 8000
              go to 120
          endif
          do 170 i=1,nc,1
              BLDVEC(i) = rnum(i) / rdis
  170     continue
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get registers for active axes
c
  200 ist    = 2
  210 ipt    = ist + 19
      do 280 i=ist,4,1
          ipt    = ipt + 1
c
c......Get user's input
c
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = MOTREG(ipt)
          r      = DUMMY
          nc     = 1
          call prmcod (inum,r,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              MOTREG(ipt) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  280 continue
c
c...Blade rotary axis output is Linear/Rotary
c

  300 MLEVL(NLEVL) = 5
c
c......Get user input
c
      iwrn   = 0
      inum   = IRTSCL(4)
      if (inum .lt. 1 .or. inum .gt. 2) inum = 1
      call prmvoc (inum,iscal,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRTSCL(4) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Axes position
c
 400  MLEVL(NLEVL) = 6
      iwrn   = 0
c
      r      = AXSSTO(10)
      nc     = 1
      call prmrel (r,nc,1,pmn,pmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          AXSSTO(10) = r
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get maximal angle change allowed for Blade
c
 500  MLEVL(NLEVL) = 7
      iwrn   = 0
      r      = BLDELT
      nc     = 1
      call prmrel (r,nc,1,dmn,dmx,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BLDELT = r
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get limits for Blade rotary axis
c
  600 MLEVL(NLEVL) = 8
      iwrn   = 0
      rnum(1) = LIMITS(1,10)
      rnum(2) = LIMITS(2,10)
      call getr8s (rnum,2,sbuf,nc)
  620 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -1) go to 8000
          if (nc .eq. -2) go to 7000
c
c......Check validity of answer
c
          call getr8n (sbuf,nc,rnum,nwds,iwrn)
          if (iwrn .eq. 1 .or. nwds .gt. 2 .or. rnum(1) .gt. rnum(2))
     1        then
                  iwrn   = 1
                  call errmsg ('INVRSP',1,2)
                  if (IMSCAN .eq. 2) go to 8000
                  go to 620
              endif
c
c......Answer is valid
c
          LIMITS(1,10) = rnum(1)
          LIMITS(2,10) = rnum(2)
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
c   SUBROUTINE:  bldalc (kfl,cmsg,kerr)
c
c   FUNCTION:  Blade align parameters prompt handling routine.
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
      subroutine bldalc (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
      character*(*) cmsg
c
      equivalence (IBLDIR,KPOSMP(0834))
      equivalence (MODBLD,KPOSMP(3984)), (ALNCUT,KPOSMP(4040))
      equivalence (ALNCSK,KPOSMP(4041)), (ALNMOV,KPOSMP(4045))
c
      integer*4 MODBLD,ALNCUT,ALNCSK,ALNMOV,IBLDIR
c
      equivalence (ALNDIS,POSMAP(4444))
c
      real*8 ALNDIS(3)
c
      integer*4 iwrn,inum,ist,ilod,i,nc,inc,ionoff(2),ix(3),idir(3)
c
      real*8 rnum,rmx
c
      data ionoff /47,48/, ix /2,3,1/, idir /45,46,70/
c
      data rmx /99999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210,500), MLEVL(NLEVL)

c...Automatic blade alignment mode
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = 2 - ALNCUT
      call prmvoc (inum,ionoff,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ALNCUT = 2 - inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Align parameters
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,4,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum   = ALNDIS(ix(inc))
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ALNDIS(ix(inc)) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
c
c...Blade direction
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = IBLDIR
      if (inum .eq. 0) inum = 3
      call prmvoc (inum,idir,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IBLDIR = inum
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
c   SUBROUTINE:  bldalm (kfl,cmsg,kerr)
c
c   FUNCTION:  Blade align parameters prompt handling routine.
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
      subroutine bldalm (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
      character*(*) cmsg
c
      equivalence (MODBLD,KPOSMP(3984)), (ALNCUT,KPOSMP(4040))
      equivalence (ALNMOV,KPOSMP(4045)), (ALNMSK,KPOSMP(4046))
c
      integer*4 MODBLD,ALNCUT,ALNMSK,ALNMOV
c
      equivalence (ALNMPS,POSMAP(4461))
c
      real*8 ALNMPS(3)
c
      integer*4 iwrn,inum,ist,ilod,i,inc,nc,ionoff(2),ix(3)
c
      real*8 rnum,rmx
c
      data ionoff /47,48/, ix /1,2,3/
c
      data rmx /99999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (100,210,210,210), MLEVL(NLEVL)
c
c...Automatic blade alignment mode
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = 2 - ALNMOV
      call prmvoc (inum,ionoff,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ALNMOV = 2 - inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Align parameters
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,4,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum   = ALNMPS(ix(inc))
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              ALNMPS(ix(inc)) = rnum
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
c   SUBROUTINE:  bldsmo (kfl,cmsg,kerr)
c
c   FUNCTION:  Smooth command parameters prompt handling routine.
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
      subroutine bldsmo (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
      character*(*) cmsg
c
      equivalence (SMOKON,KPOSMP(4071)), (SMOKAN,KPOSMP(4072))
      equivalence (SMOCCD,KPOSMP(4073)), (SMONRN,KPOSMP(4075))
      equivalence (SMONCD,KPOSMP(4076)), (SMOORN,KPOSMP(4079))
      equivalence (SMOOCD,KPOSMP(4080)), (SMOSEL,KPOSMP(4084))
c
      integer*4 SMOKON,SMOKAN,SMOCCD(2),SMONCD(3),SMONRN,SMOORN,
     -          SMOOCD(3),SMOSEL
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (SMOCVL,POSMAP(4448)), (SMONVL,POSMAP(4450))
      equivalence (SMOOVL,POSMAP(4453)), (SMORAN,POSMAP(4464))
      equivalence (SMODIS,POSMAP(4465))
c
      real*8 SMORAN,SMODIS,SMOCVL(2),DUMMY,SMONVL(3),SMOOVL(3)
c
      integer*4 iwrn,inum,ist,ilod,i,nc,inc,iary(3),jindex
      real*8 r,rnum,rmx,parm(2),rary(3)
c
      equivalence (SMORAN,parm(1))
c
      data rmx /999999.d0/
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      goto (110,110,300,400,500,610,610), MLEVL(NLEVL)
c
c...SMOOTH/ON & OFF codes
c
  100 ist    = 1
  110 inc    = ist - 1
      do 190 i=ist,2,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = SMOCCD(inc)
          rnum   = SMOCVL(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              SMOCCD(inc) = inum
              SMOCVL(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Registers for SMOOTH/AA  ON command
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      nc     = SMONRN
      call copynk (SMONCD,iary,nc)
      call copyn (SMONVL,rary,nc)
      call prmcod (iary,rary,nc,2,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Answer is valid
c
          call copyn (rary,SMONVL,nc)
          call copynk (iary,SMONCD,nc)
          SMONRN = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Registers for SMOOTH/AA  OFF command
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      nc     = SMOORN
      call copynk (SMOOCD,iary,nc)
      call copyn (SMOOVL,rary,nc)
      call prmcod (iary,rary,nc,2,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
c
c......Answer is valid
c
          call copyn (rary,SMOOVL,nc)
          call copynk (iary,SMOOCD,nc)
          SMOORN = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Register for maximum angle change
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = SMONCD(3)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          SMONCD(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      end if
c
c...SMOOTH/ATANGL angle and distance
c
  600 ist    = 6
  610 inc    = ist    - 6
      do 690 i=ist,7,1
          MLEVL(NLEVL) = i
          inc    = inc    + 1
          iwrn   = 0
          rnum   = parm(inc)
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              parm(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
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
