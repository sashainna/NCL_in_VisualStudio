c
c***********************************************************************
c
c   FILE NAME:  tapfmt.for
c   CONTAINS:
c               tapfmt  blkfmt  pu1fil  strten  tbreak
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tapfmt.f , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        03/14/16 , 13:05:14
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  tapfmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Control (punch) tape format menu handling routine.
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
      subroutine tapfmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
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
      go to (1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     1       8000), igo
c
c...Walk through Control Tape Format
c
 1000 IWALK  = 1
c
c...Tape register format
c
 1100 MLEVL(NLEVL-1) = 1
      call regfmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...G-code group definition
c
 1200 MLEVL(NLEVL-1) = 2
      call ggroup (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...M-code group definition
c
 1300 MLEVL(NLEVL-1) = 3
      call mgroup (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Store register values
c
 1400 MLEVL(NLEVL-1) = 4
      call setreg (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Word address order
c
 1500 MLEVL(NLEVL-1) = 5
      call wrdord (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...User defined blocks
c
 1600 MLEVL(NLEVL-1) = 6
      call usrblk (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Tape block format
c
 1700 MLEVL(NLEVL-1) = 7
      call blkfmt (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      if (IWALK .eq. 0) go to 7000
c
c...Punch file format
c
 1800 MLEVL(NLEVL-1) = 8
      call pu1fil (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Starting/ending sequences
c
 1900 MLEVL(NLEVL-1) = 9
      call strten (kfl,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
      go to 7000
c
c...Tape breaks
c
 2000 MLEVL(NLEVL-1) = 10
      call tbreak (kfl,cmsg,kerr)
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
c   SUBROUTINE:  blkfmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Tape block format prompt handling routine.
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
      subroutine blkfmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MULTGC,KPOSMP(0838)), (MULTMC,KPOSMP(0839))
      equivalence (MGCD  ,KPOSMP(0840)), (NEOB  ,KPOSMP(0841))
      equivalence (TABSEQ,KPOSMP(0842)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
      equivalence (ICSMRG,KPOSMP(1132)), (ICSMFL,KPOSMP(1133))
      equivalence (MXCKSM,KPOSMP(1134))
c
      integer*4 MULTGC,MULTMC,MGCD,NEOB,TABSEQ,NCBMSG,NCEMSG,NOPSKP,
     1          ICSMRG,ICSMFL,MXCKSM
c
      equivalence (DUMMY ,POSMAP(0003))
c
      real*8 DUMMY
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (LEDCHR,CPOSMP(0986)), (MSGEN ,CPOSMP(1500))
      equivalence (TABCHR,CPOSMP(0987)), (LOPSKP,CPOSMP(1648))
c
      character*1 LEDCHR,TABCHR
      character*5 LEOB,LOPSKP
      character*10 MSGST,MSGEN
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,nc,inum,iyesno(2),ionoff(2)
c
      real*8 rnum
c
      character*80 sbuf
c
      data iyesno /13,12/, ionoff /47,48/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 go to (100,200,300,400,500,600,700,800,900,1000,1100,1200,1300),
     1           MLEVL(NLEVL)
c
c...Multiple G-codes
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = MULTGC
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MULTGC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Multiple M-codes
c
  200 MLEVL(NLEVL) = 2
      inum   = MULTMC
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MULTMC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...G-code & M-code on same block
c
  300 MLEVL(NLEVL) = 3
      inum   = MGCD
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MGCD   = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Get Word Address/Tab Sequential format
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = TABSEQ
      nc     = 1
      call prmint (inum,nc,1,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          TABSEQ = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...End of Block char(s)
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      if (NEOB .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = LEOB(1:NEOB)
      endif
      nc     = NEOB
  510 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 5) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 510
          endif
c
c......Answer is valid
c
          LEOB   = sbuf
          NEOB   = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Start of message char(s)
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      if (NCBMSG .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = MSGST(1:NCBMSG)
      endif
      nc     = NCBMSG
  610 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 10) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 610
          endif
c
c......Answer is valid
c
          MSGST  = sbuf
          NCBMSG = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...End of message char(s)
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      if (NCEMSG .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = MSGEN(1:NCEMSG)
      endif
      nc     = NCEMSG
  710 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 10) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 710
          endif
c
c......Answer is valid
c
          MSGEN  = sbuf
          NCEMSG = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Leader character
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      sbuf   = LEDCHR
      nc     = 1
  810 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 1) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 810
          endif
c
c......Answer is valid
c
          if (nc .eq. 0) then
              LEDCHR = ' '
          else
              LEDCHR = sbuf(1:1)
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Tab character
c
  900 MLEVL(NLEVL) = 9
      iwrn   = 0
      sbuf   = TABCHR
      nc     = 1
  910 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 1) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 910
          endif
c
c......Answer is valid
c
          if (nc .eq. 0) then
              TABCHR = ' '
          else
              TABCHR = sbuf(1:1)
          endif
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Optional skip char(s)
c
 1000 MLEVL(NLEVL) = 10
      iwrn   = 0
      if (NOPSKP .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = LOPSKP(1:NOPSKP)
      endif
      nc     = NOPSKP
 1010 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 5) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 510
          endif
c
c......Answer is valid
c
          LOPSKP = sbuf
          NOPSKP = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...CHECK/LENGTH default
c
 1100 MLEVL(NLEVL) = 11
      iwrn   = 0
      inum   = ICSMFL
      call prmvoc (inum,ionoff,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICSMFL = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...CHECK/LENGTH register
c
 1200 MLEVL(NLEVL) = 12
      iwrn   = 0
      inum   = ICSMRG
      rnum   = DUMMY
      nc     = 1
      call prmcod (inum,rnum,nc,1,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          ICSMRG = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Maximum checksumming value
c
 1300 MLEVL(NLEVL) = 13
      iwrn   = 0
      inum   = MXCKSM
      nc     = 1
      call prmint (inum,nc,1,1,99999,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MXCKSM = inum
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
c   SUBROUTINE:  pu1fil (kfl,cmsg,kerr)
c
c   FUNCTION:  Punch file format prompt handling routine.
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
      subroutine pu1fil (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (IPACKD,KPOSMP(1101)), (IPARTO,KPOSMP(1102))
      equivalence (PCHSPC,KPOSMP(1103)), (MACSPC,KPOSMP(1110))
c
      integer*4 IPACKD,IPARTO,PCHSPC,MCHOPT(20),MACSPC
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,nc,inum,ipack(2),iyesno(2)
c
      data ipack /26,27/, iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 go to (100,200,300,400,500) MLEVL(NLEVL)
c
c...Packed/Unpacked punch file
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = IPACKD
      call prmvoc (inum,ipack,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IPACKD = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Punch file length
c
  200 if (IPACKD .eq. 1) then
          if (kfl .lt. NLEVL) go to 295
          call errmsg ('NOTAPPLY',1,2)
          go to 8000
      endif
      MLEVL(NLEVL) = 2
      iwrn   = 0
      inum   = MCHOPT(5)
      nc     = 1
      call prmint (inum,nc,1,10,512,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MCHOPT(5) = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
  295 continue
c
c...PARTNO card output
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      inum   = IPARTO
      nc     = 1
      call prmint (inum,nc,1,0,4,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IPARTO = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Space between registers in punch file
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      inum   = PCHSPC
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PCHSPC = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Space between Macro parameters in punch file
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = MACSPC
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          MACSPC = inum
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
c   SUBROUTINE:  strten (kfl,cmsg,kerr)
c
c   FUNCTION:  Starting ending sequence prompt handling routine.
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
      subroutine strten (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REWCOD,KPOSMP(1080)), (NRWSTP,KPOSMP(1104))
      equivalence (FINCOD,KPOSMP(1105)), (NEOT  ,KPOSMP(1106))
      equivalence (IRWEOB,KPOSMP(1135)), (NBOT  ,KPOSMP(1136))
      equivalence (MUSRBK,KPOSMP(3022)), (STBLK ,KPOSMP(3024))
c
      integer*4 NRWSTP,FINCOD,NEOT,MUSRBK,STBLK,REWCOD,IRWEOB,NBOT
c
      equivalence (DUMMY ,POSMAP(0003)), (REWCDV,POSMAP(1172))
      equivalence (LEDBEG,POSMAP(1201))
      equivalence (LEDEND,POSMAP(1202)), (FINCDV,POSMAP(1203))
c
      real*8 DUMMY,LEDBEG,LEDEND,FINCDV,REWCDV
c
      equivalence (LRWSTP,CPOSMP(2201)), (LEOT  ,CPOSMP(2211))
      equivalence (LBOT  ,CPOSMP(2515)), (PHFILE,CPOSMP(2535))
c
      character*10 LRWSTP
      character*20 LEOT,LBOT
      character*40 PHFILE
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,nc,inum,iyesno(2),strlen1
c
      real*8 rnum
c
      character*80 sbuf
c
      data iyesno /13,12/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 go to (100,200,300,400,500,600,700,800,900,1000), MLEVL(NLEVL)
c
c...Punch Header File
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      sbuf   = PHFILE
      nc     = 1
      call prmstr (nc,sbuf,0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          PHFILE  = sbuf
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Leader at start of Punch file
c
  200 MLEVL(NLEVL) = 2
      iwrn   = 0
      rnum   = LEDBEG
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,10000.D0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LEDBEG = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Leader at end of Punch file
c
  300 MLEVL(NLEVL) = 3
      iwrn   = 0
      rnum   = LEDEND
      nc     = 1
      call prmrel (rnum,nc,1,0.D0,10000.D0,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          LEDEND = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Rewind stop chars
c
  400 MLEVL(NLEVL) = 4
      iwrn   = 0
      if (NRWSTP .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = LRWSTP(1:NRWSTP)
      endif
      nc     = NRWSTP
  410 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 10) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 410
          endif
c
c......Answer is valid
c
          LRWSTP = sbuf
          NRWSTP = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Output EOB with Rewind Stop
c
  500 MLEVL(NLEVL) = 5
      iwrn   = 0
      inum   = IRWEOB
      call prmvoc (inum,iyesno,2,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          IRWEOB = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Starting block registers
c
  600 MLEVL(NLEVL) = 6
      iwrn   = 0
      inum   = STBLK
      nc     = 1
      call prmint (inum,nc,1,0,MUSRBK,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          STBLK  = inum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...REWIND code
c
  700 MLEVL(NLEVL) = 7
      iwrn   = 0
      inum   = REWCOD
      rnum   = REWCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          REWCOD = inum
          REWCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...FINI code
c
  800 MLEVL(NLEVL) = 8
      iwrn   = 0
      inum   = FINCOD
      rnum   = FINCDV
      nc     = 1
      call prmcod (inum,rnum,nc,1,1,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          FINCOD = inum
          FINCDV = rnum
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Start-of-Tape char(s)
c
  900 MLEVL(NLEVL) = 9
      iwrn   = 0
      if (NBOT .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = LBOT(1:NBOT)
      endif
      nc     = NBOT
  910 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 20) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 910
          endif
c
c......Answer is valid
c
          LBOT   = sbuf
          NBOT   = nc
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...End-of-Tape char(s)
c
 1000 MLEVL(NLEVL) = 10
      iwrn   = 0
      if (NEOT .eq. 0) then
          sbuf   = ' '
      else
          sbuf   = LEOT(1:NEOT)
      endif
      nc     = NEOT
 1010 call prompt (sbuf,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (kerr .ne. 0) go to 8000
          if (nc .eq. -2) go to 7000
          if (nc .eq. -1) go to 8000
c
c......Check validity of answer
c
          if (nc .gt. 20) then
              iwrn   = 1
              call errmsg ('INVRSP',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 1010
          endif
c
c......Answer is valid
c
          LEOT   = sbuf
          NEOT   = nc
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
c   SUBROUTINE:  tbreak (kfl,cmsg,kerr)
c
c   FUNCTION:  Tape break prompt handling routine.
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
      subroutine tbreak (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
c      equivalence (BRKOP ,KPOSMP(1137))
      equivalence (BRKOP ,KPOSMP(1137))
c
      integer*4 BRKOP(10)
c
      equivalence (BRKVR ,POSMAP(1205))
c
      real*8 BRKVR(10)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 ilod,iwrn,nc,inum,iyesno(2),imod(6),ist,i,inc,isub(6),
     1          ifmt(4)
c
      real*8 rnum,rmx
c
      data rmx /9999999999.d0/
c
      data iyesno /13,12/, imod /48,47,67,68,69,84/, isub /1,2,3,6,4,5/
      data ifmt /2,5,3,4/
c
c...Specific level was requested
c...Go directly to that level
c
      ilod   = 1
      if (kfl .lt. NLEVL) go to 100
   50 ist   = MLEVL(NLEVL)
      go to (100,210,210,210,210,610,610,610,610,610,610), MLEVL(NLEVL)
c
c...Tape break mode
c
  100 MLEVL(NLEVL) = 1
      iwrn   = 0
      inum   = BRKOP(1) + 1
      nc     = -6
      call prmvoc (inum,imod,nc,kfl,ilod,iwrn,cmsg,kerr)
      if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
          if (iwrn .eq. 2) go to 7000
          if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
          BRKOP(1) = inum   - 1
          if (kfl .ge. NLEVL) go to 8000
      endif
c
c...Tape break options
c
  200 ist    = 2
  210 inc    = ist    - 2
      do 290 i=ist,5,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          inum   = BRKOP(ifmt(inc))
          nc     = 2
          if (inc .eq. 4) nc = -2
          call prmvoc (inum,iyesno,nc,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              BRKOP(ifmt(inc)) = inum
              if (kfl .ge. NLEVL) go to 8000
          endif
  290 continue
c
c...Default tape lengths for tape breaks
c
  600 ist    = 6
  610 inc    = ist    - 6
      do 690 i=ist,11,1
          inc    = inc    + 1
          MLEVL(NLEVL) = i
          iwrn   = 0
          if (inc .eq. 1 .and. BRKOP(1) .ne. 1 .and. BRKOP(1) .ne. 2)
     1            iwrn = 1
          if (inc .eq. 2 .and. BRKOP(1) .ne. 3) iwrn = 1
          if (inc .eq. 3 .and. BRKOP(1) .ne. 4) iwrn = 1
          if (inc .eq. 4 .and. BRKOP(1) .ne. 5) iwrn = 1
          if ((inc .eq. 5 .or. inc .eq. 6).and. BRKOP(3) .ne. 1)
     1            iwrn = 1
          if (iwrn .eq. 1) then
              if (kfl .lt. NLEVL) go to 690
              call errmsg ('NOTAPPLY',1,2)
              if (IMSCAN .eq. 2) go to 8000
              go to 690
          endif
          iwrn   = 0
          rnum   = BRKVR(isub(inc))
          nc     = 1
          call prmrel (rnum,nc,1,0.d0,rmx,kfl,ilod,iwrn,cmsg,kerr)
          if (MOTIF .eq. 1 .or. IMSCAN .ne. 1) then
              if (iwrn .eq. 2) go to 7000
              if (iwrn .eq. 1 .or. kerr .ne. 0) go to 8000
              BRKVR(isub(inc)) = rnum
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
