c
c***********************************************************************
c
c   FILE NAME:  mscpst.for
c   CONTAINS:
c               mscpst  mpsmsg  mpstop  mpsdly  mpshom  mpspsn  mpspod
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mscpst.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:57
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  mscpst (kfl,cmsg,kerr)
c
c   FUNCTION:  Miscellaneous post commands menu handling routine.
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
      subroutine mscpst (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (NSPRG ,KPOSMP(3101)), (SPNTYP,KPOSMP(3102))
c
      integer*4 NSPRG,SPNTYP,MACHTP
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
c...Display Miscellaneous post command menu
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
c...Walk through Miscellaneous post commands
c
 1000 IWALK  = 1
c
c...Message & INSERT
c
 1100 MLEVL(NLEVL-1) = 1
      call mpsmsg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...OPSKIP, OPSTOP & STOP
c
 1200 MLEVL(NLEVL-1) = 2
      call mpstop (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...DELAY
c
 1300 MLEVL(NLEVL-1) = 3
      call mpsdly (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...GOHOME
c
 1400 MLEVL(NLEVL-1) = 4
      call mpshom (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...POSTN
c
 1500 MLEVL(NLEVL-1) = 5
      call mpspsn (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Vacuum Pods
c
c1600 if (MACHTP .ne. 3) then
c         if (IWALK .eq. 1) go to 8000
c         if (MOTIF .eq. 1) IMENDT(1,NLEVL-1) = 1
c         call errmsg ('NOTAPPLY',1,2)
c         go to 8000
c     endif
c     MLEVL(NLEVL-1) = 6
c     call mpspod (kfl,cmsg,kerr)
c     if (kerr .ne. 0) go to 8000
c     if (IWALK .eq. 0) go to 7000
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
c   SUBROUTINE:  mpsmsg (kfl,cmsg,kerr)
c
c   FUNCTION:  Message & INSERT prompt handling routine.
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
      subroutine mpsmsg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (PPRIPV,KPOSMP(0175))
      equivalence (MSGSEQ,KPOSMP(0301)), (MSGEOB,KPOSMP(0302))
      equivalence (MSGTAB,KPOSMP(0303)), (INSSEQ,KPOSMP(0304))
      equivalence (INSEOB,KPOSMP(0305)), (MSGALN,KPOSMP(0349))
c
      integer*4 MSGSEQ,MSGEOB,MSGTAB,INSSEQ,INSEOB,MSGALN,PPRIPV
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum
c
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (100,200,300,400,500,600,700), MLEVL(NLEVL)
c
c...Add seqnos to Message blocks
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = MSGSEQ
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MSGSEQ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Add seqnos to INSERT blocks
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = INSSEQ
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          INSSEQ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Add EOB to Message block
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = MSGEOB
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MSGEOB = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Add EOB to INSERT block
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = INSEOB
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          INSEOB = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Convert message spaces to tabs
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = MSGTAB
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MSGTAB = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Align message blocks
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = MSGALN
      nc     = 1
      call prmint (inum,nc,1,0,66,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MSGALN  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Convert message spaces to tabs
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = PPRIPV
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PPRIPV = inum
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
c   SUBROUTINE:  mpstop (kfl,cmsg,kerr)
c
c   FUNCTION:  OPSKIP, OPSTOP & STOP prompt handling routine.
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
      subroutine mpstop (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (OPSSEQ,KPOSMP(0306)), (STPSEQ,KPOSMP(0307))
      equivalence (OPSTCD,KPOSMP(1075)), (STOPCD,KPOSMP(1076))
      equivalence (MUSRBK,KPOSMP(3022))
      equivalence (OPKBLK,KPOSMP(3046)), (OPSBLK,KPOSMP(3047))
      equivalence (STPBLK,KPOSMP(3048))
c
      integer*4 OPSSEQ,STPSEQ,OPSTCD,STOPCD,OPKBLK,OPSBLK,STPBLK,
     1          MUSRBK
c
      equivalence (OPSTVL,POSMAP(1168)), (STOPVL,POSMAP(1169))
c
      real*8 OPSTVL,STOPVL
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum
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
      go to (100,200,300,400,500,600,700), MLEVL(NLEVL)
c
c...OPSTOP code
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = OPSTCD
      rnum   = OPSTVL
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          OPSTCD = inum
          OPSTVL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...STOP code
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = STOPCD
      rnum   = STOPVL
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          STOPCD = inum
          STOPVL = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...User defined OPSKIP/OFF block
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = OPKBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          OPKBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...User defined OPSKIP/OFF block
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = OPSBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          OPSBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...User defined STOP block
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = STPBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          STPBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is OPSTOP an End-of-Sequence
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = OPSSEQ
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          OPSSEQ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Is STOP an End-of-Sequence
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = STPSEQ
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          STPSEQ = inum
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
c   SUBROUTINE:  mpsdly (kfl,cmsg,kerr)
c
c   FUNCTION:  DELAY prompt handling routine.
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
      subroutine mpsdly (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DELYCD,KPOSMP(3351)), (DELYFL,KPOSMP(3356))
c
      integer*4 DELYCD(5),DELYFL(5)
c
      equivalence (DUMMY ,POSMAP(0003))
      equivalence (DELYVL,POSMAP(0165)), (DELYTM,POSMAP(0166))
c
      real*8 DUMMY,DELYVL,DELYTM
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,iyesno(2),inum,inc,ifl,i
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
      go to (110,110,110,400,500,600,700,800), MLEVL(NLEVL)
c
c...DELAY codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,3,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = DELYCD(inc)
          if (inc .eq. 1) then
              ifl    = 1
              rnum   = DELYVL
          else
              ifl    = 2
              rnum   = DUMMY
          endif
          nc     = 1
          call prmcod (inum,rnum,nc,1,ifl,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              DELYCD(inc) = inum
              if (inc .eq. 1) DELYVL = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Output format for delay blocks
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = DELYFL(1)
      nc     = -1
      call prmint (inum,nc,1,1,6,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DELYFL(1) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Time unit to use for delay blocks
c
  500 MLEVL(NLEVL) = 5
      if (DELYFL(1) .eq. 2 .or. DELYFL(1) .eq. 5) then
          if (kfl .lt. NLEVL) go to 590
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      inum   = DELYFL(2)
      nc     = 1
      call prmint (inum,nc,1,1,3,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DELYFL(2) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  590 continue
c
c...Delay block time
c
  600 MLEVL(NLEVL) = 6
      if (DELYFL(1) .ne. 5) then
          if (kfl .lt. NLEVL) go to 690
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      iwrn   = 0
      rnum   = DELYTM
      nc     = 1
      call prmrel (rnum,nc,3,0.d0,100.d0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DELYTM = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
  690 continue
c
c...Output the X-axis with a delay
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = DELYFL(3)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DELYFL(3) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Reset feedrate after delay
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = DELYFL(4)
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          DELYFL(4) = inum
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
c   SUBROUTINE:  mpshom (kfl,cmsg,kerr)
c
c   FUNCTION:  GOHOME prompt handling routine.
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
      subroutine mpshom (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (HOMCOD,KPOSMP(3361)), (HOMREG,KPOSMP(3366))
c
      integer*4 HOMCOD(5),HOMREG(5),NUMLIN(3),IRTNUM
c
      equivalence (DUMMY ,POSMAP(0003)), (HOMCDV,POSMAP(0167))
c
      real*8 DUMMY,HOMCDV(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,inum,inc,i,ipt
c
      real*8 rnum
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,510,510,510,510,510,510,510,510,510,
     1       510), MLEVL(NLEVL)
c
c...GOHOME codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = HOMCOD(inc)
          rnum   = HOMCDV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              HOMCOD(inc) = inum
              HOMCDV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...GOHOME registers
c
  500 ist    = 5
  510 inc    = ist    - 5
      if (HOMCOD(1) .eq. 0 .and. HOMCOD(2) .eq. 0 .and.
     1    HOMCOD(3) .eq. 0 .and. HOMCOD(4) .eq. 0) then
          if (kfl .lt. NLEVL) go to 595
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 590 i=ist,14,1
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
              if (kfl .lt. NLEVL) go to 590
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          inum   = HOMREG(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              HOMREG(inc) = inum
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
c   SUBROUTINE:  mpspsn (kfl,cmsg,kerr)
c
c   FUNCTION:  POSTN prompt handling routine.
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
      subroutine mpspsn (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NUMLIN,KPOSMP(1202)), (IRTNUM,KPOSMP(1243))
      equivalence (PSTNCD,KPOSMP(3376)), (PSTNRG,KPOSMP(3381))
      equivalence (PSTNOF,KPOSMP(3396))
c
      integer*4 PSTNCD(5),PSTNRG(15),PSTNOF,NUMLIN(3),IRTNUM
c
      equivalence (DUMMY ,POSMAP(0003)), (PSTNCV,POSMAP(0172))
c
      real*8 DUMMY,PSTNCV(5)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,inum,inc,iyesno(2),i,ipt
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
      go to (100,210,210,210,500,610,610,610,610,610,610,610,610,
     1       610,610), MLEVL(NLEVL)
c
c...POSTN program register
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = PSTNRG(11)
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PSTNRG(11) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...POSTN codes
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,4,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PSTNCD(inc)
          rnum   = PSTNCV(inc)
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PSTNCD(inc) = inum
              PSTNCV(inc) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
c
c...POSTN/OFF on block by itself
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = PSTNOF
      nc     = 1
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PSTNOF = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...POSTN axis registers
c
  600 ist    = 6
  610 inc    = ist    - 6
      if (PSTNCD(1) .eq. 0 .and. PSTNCD(2) .eq. 0) then
          if (kfl .lt. NLEVL) go to 695
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 690 i=ist,15,1
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
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              go to 8000
          endif
c
c......This axis is present
c......Set up default answer & get user's input
c
          iwrn   = 0
          inum   = PSTNRG(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PSTNRG(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  690 continue
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
c   SUBROUTINE:  mpspod (kfl,cmsg,kerr)
c
c   FUNCTION:  Vacuum pod prompt handling routine.
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
      subroutine mpspod (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (PODACD,KPOSMP(0372)), (PODCCD,KPOSMP(0379))
      equivalence (PODSCD,KPOSMP(0818)), (MACHTP,KPOSMP(1201))
c
      integer*4 PODACD(7),PODCCD(2),PODSCD(6),MACHTP
c
      equivalence (DUMMY ,POSMAP(0003)), (PODAVL,POSMAP(2494))
      equivalence (PODSVL,POSMAP(3594))
c
      real*8 DUMMY,PODAVL(7),PODSVL(6)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,ist,nc,inum,inc,i,ipt,is1(7),is2(6)
c
      real*8 rnum
c
      data is1 /6, 5, 7, 3, 4, 1, 2/
      data is2 /1, 5, 4, 6, 2, 3/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist    = MLEVL(NLEVL)
      go to (110,110,110,110,110,110,110,810,810,1010,1010,1010,1010,
     1       1010,1010), MLEVL(NLEVL)
c
c...All pods' codes
c
  100 ist    = 1
  110 inc    = ist    - 1
      do 190 i=ist,7,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PODACD(is1(inc))
          rnum   = PODAVL(is1(inc))
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PODACD(is1(inc)) = inum
              PODAVL(is1(inc)) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
  190 continue
c
c...Pod address registers
c
  800 ist    = 8
  810 inc    = ist    - 8
      do 890 i=ist,9,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PODCCD(inc)
          rnum   = DUMMY
          nc     = 1
          call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PODCCD(inc) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  890 continue
c
c...Addressable pod codes
c
 1000 ist    = 10
 1010 inc    = ist    - 10
      if (PODCCD(1) .eq. 0 .and. PODCCD(2) .eq. 0) then
          if (kfl .lt. NLEVL) go to 1095
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      do 1090 i=ist,15,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          inum   = PODSCD(is2(inc))
          rnum   = PODSVL(is2(inc))
          nc     = 1
          call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .ne. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              PODSCD(is2(inc)) = inum
              PODSVL(is2(inc)) = rnum
              if (kfl .ge. NLEVL) go to 8000
          endif
 1090 continue
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
