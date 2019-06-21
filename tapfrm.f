c
c***********************************************************************
c
c   FILE NAME:  tapfrm.for
c   CONTAINS:
c               regfmt  ggroup  mgroup  setreg  wrdord  usrblk
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tapfrm.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:27:03
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  regfmt (kfl,cmsg,kerr)
c
c   FUNCTION:  Tape Register Format form handling routine.
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
      subroutine regfmt (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT)
c
      equivalence (REGST ,CPOSMP(7011)), (REGEN ,CPOSMP(9219))
c
      character*24 REGST(MAXFMT),REGEN(MAXFMT)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ist,ien,ifmt(4),nc,inum,ibeg,inc,ictrl(6),ierr,ipt1,
     1          strlen1
c
      character*4 lsgn(7)
      character*24 abuf
      character*132 sbuf,ebuf
c
      data ifmt /16,17,18,19/, ictrl /20,21,22,23,24,25/
      data lsgn /' ','-','*','+','+-','+*','PN'/
c
c...Initialize routine
c
      MLEVL(NLEVL) = 0
c
c...Define form layout
c
      FRMBEG = 6
      FRMNFD = 12
      FRMREC = MAXFMT
      FRMWID = 74
      if (IMSCAN .eq. 1) FRMWID = 4
c
      FRMST(1) = 1
      FRMST(2) = 5
      FRMST(3) = 31
      FRMST(4) = 41
      FRMST(5) = 45
      FRMST(6) = 52
      FRMST(7) = 56
      FRMST(8) = 60
      FRMST(9) = 64
      FRMST(10) = 68
      FRMST(11) = 73
      FRMST(12) = 99
c
      FRMEN(1) = 2
      FRMEN(2) = 28
      FRMEN(3) = 38
      FRMEN(4) = 42
      FRMEN(5) = 46
      FRMEN(6) = 53
      FRMEN(7) = 57
      FRMEN(8) = 61
      FRMEN(9) = 65
      FRMEN(10) = 69
      FRMEN(11) = 96
      FRMEN(12) = 110
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 3
      FRMTYP(2) = 4
      FRMTYP(3) = 2
      FRMTYP(4) = 2
      FRMTYP(5) = 4
      FRMTYP(6) = 4
      FRMTYP(7) = 4
      FRMTYP(8) = 4
      FRMTYP(9) = 4
      FRMTYP(10) = 4
      FRMTYP(11) = 4
      FRMTYP(12) = 2
c
c...Set up form records
c
      do 300 i=1,MAXFMT,1
          sbuf   = ' '
c
c......Register label
c
          ist    = FRMST(1)
          ien    = FRMST(1) + 1
          sbuf(ist:ien) = REGID(i)
          FRMFNC(1,i) = 2
c
c......Beginning string
c
          if (REGBNC(i) .ne. 0) then
              abuf   = REGST(i)
              ist    = FRMST(2)
              ien    = FRMST(2) + REGBNC(i) - 1
              sbuf(ist:ien) = abuf(1:REGBNC(i))
          endif
          FRMFNC(2,i) = REGBNC(i)
c
c......Format
c
          call getvwd (ifmt(FMTDES(1,i)),abuf,nc,0,MENWRD,MENWVL,NMENWD)
          if (nc .ne. 0) then
              ist    = FRMST(3)
              ien    = FRMST(3) + nc     - 1
              sbuf(ist:ien) = abuf(1:nc)
          endif
          FRMFNC(3,i) = nc
c
c.........Store default choice
c
          FRMDCH(3,i) = FMTDES(1,i) - 1
c
c......Sign
c
          FRMFNC(4,i) = 0
          ist    = FRMST(4)
          if (FMTDES(2,i) .eq. 3) then
              FRMDCH(4,i) = 6
              sbuf(ist:ist+1) = lsgn(7)(1:2)
              FRMFNC(4,i) = 2
          else
              FRMDCH(4,i) = FMTDES(3,i)
              if (FMTDES(2,i) .eq. 1) then
                  sbuf(ist:ist) = '+'
                  ist    = ist    + 1
                  FRMFNC(4,i) = FRMFNC(4,i) + 1
                  FRMDCH(4,i) = FMTDES(3,i) + 3
              endif
              if (FMTDES(3,i) .ne. 0) then
                  sbuf(ist:ist) = '-'
                  if (FMTDES(3,i) .eq. 2) sbuf(ist:ist) = '*'
                  FRMFNC(4,i) = FRMFNC(4,i) + 1
              endif
          endif
c
c......Left Inch
c
          do 200 j=4,9,1
              inum   = FMTDES(j,i)
              call itoc (inum,abuf,nc,0)
              ist    = FRMST(j+1)
              ien    = FRMST(j+1) + nc     - 1
              sbuf(ist:ien) = abuf(1:nc)
              FRMFNC(j+1,i) = nc
  200     continue
c
c......Ending string
c
          if (REGENC(i) .ne. 0) then
              abuf   = REGEN(i)
              ist    = FRMST(11)
              ien    = FRMST(11) + REGENC(i) - 1
              sbuf(ist:ien) = abuf(1:REGENC(i))
          endif
          FRMFNC(11,i) = REGENC(i)
c
c......Control
c
          call getvwd (ictrl(FMTDES(10,i)+1),abuf,nc,0,MENWRD,MENWVL,
     1                 NMENWD)
          if (nc .ne. 0) then
              ist    = FRMST(12)
              ien    = FRMST(12) + nc     - 1
              sbuf(ist:ien) = abuf(1:nc)
          endif
          FRMFNC(12,i) = nc
c
c.........Store default choices
c
          FRMDCH(12,i) = FMTDES(10,i)
c
c......Store this buffer in the form array
c
          FRMBUF(i) = sbuf
  300 continue
c
c...Store choices
c
      FRMNCH(3) = 4
      FRMNCH(4) = 7
      FRMNCH(12) = 6
      do 400 i=1,4,1
          call getvwd (ifmt(i),FRMCHC(i,3),nc,0,MENWRD,MENWVL,NMENWD)
  400 continue
      do 420 i=1,7,1
          FRMCHC(i,4) = lsgn(i)
  420 continue
      do 430 i=1,6,1
          call getvwd (ictrl(i),FRMCHC(i,12),nc,0,MENWRD,MENWVL,NMENWD)
  430 continue
c
c...Display the form header
c
      call getsap ('REGFMT',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
c...Store Motif header labels
c
      if (IMSCAN .eq. 1) then
          ipt1   = FRMHPT(1)
          ist    = 1
          call gtflab1 (SAPRM(ipt1),FRMLAB(1),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(2),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(3),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(4),nc,ist,1)
c
          call gtflab1 (SAPRM(ipt1),FRMLAB(5),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(5)(nc+2:),nc,ist,0)
          call gtflab1 (SAPRM(ipt1),FRMLAB(6),nc,ist,0)
c
          call gtflab1 (SAPRM(ipt1),FRMLAB(7),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(7)(nc+2:),nc,ist,0)
          call gtflab1 (SAPRM(ipt1),FRMLAB(8),nc,ist,0)
c
          call gtflab1 (SAPRM(ipt1),FRMLAB(9),nc,ist,1)
          call gtflab1 (SAPRM(ipt1),FRMLAB(9)(nc+2:),nc,ist,0)
          call gtflab1 (SAPRM(ipt1),FRMLAB(10),nc,ist,0)
c
          call gtflab1 (SAPRM(ipt1),FRMLAB(11),nc,ist,1)
c
          call gtflab1 (SAPRM(ipt1),FRMLAB(12),nc,ist,1)
          IMTYP  = 3
c
          go to 8000
      else
          call plott (3,1)
          call clreos
      endif
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 do 700 i=1,MAXFMT,1
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
c......Beginning string
c
          inc    = 2
          if (FRMFNC(inc,i) .ne. 0) then
              ist    = FRMST(inc)
              ien    = FRMST(inc) + FRMFNC(inc,i) - 1
              REGST(i) = sbuf(ist:ien)
          endif
          REGBNC(i) = FRMFNC(inc,i)
c
c......Format
c
          inc    = 3
          if (FRMFNC(inc,i) .eq. 0) go to 9000
          ist    = FRMST(inc)
          ien    = FRMST(inc) + FRMFNC(inc,i) - 1
          call getvnr (sbuf(ist:ien),ifmt,4,inum,MENWRD,MENWVL,NMENWD)
          if (inum .eq. 0) go to 9000
          FMTDES(1,i) = inum
c
c......Sign
c
          inc    = 4
          ist    = FRMST(inc)
          FMTDES(2,i) = 0
          FMTDES(3,i) = 0
          if (FRMFNC(inc,i) .gt. 2) go to 9000
          if (FRMFNC(inc,i) .ne. 0) then
              if (sbuf(ist:ist) .eq. '+') then
                  FMTDES(2,i) = 1
              else if (sbuf(ist:ist) .eq. '-') then
                  FMTDES(3,i) = 1
              else if (sbuf(ist:ist) .eq. '*') then
                  FMTDES(3,i) = 2
              else if (sbuf(ist:ist+1) .eq. 'PN') then
                  FMTDES(2,i) = 3
                  FMTDES(3,i) = 3
              else
                  go to 9000
              endif
              if (FRMFNC(inc,i) .eq. 2 .and. FMTDES(2,i) .ne. 3) then
                  ist    = ist    + 1
                  if (sbuf(ist:ist) .eq. '+') then
                      if (FMTDES(2,i) .ne. 0) go to 9000
                      FMTDES(2,i) = 1
                  else if (sbuf(ist:ist) .eq. '-') then
                      if (FMTDES(3,i) .ne. 0) go to 9000
                      FMTDES(3,i) = 1
                  else if (sbuf(ist:ist) .eq. '*') then
                      if (FMTDES(3,i) .ne. 0) go to 9000
                      FMTDES(3,i) = 2
                  else
                      go to 9000
                  endif
              endif
          endif
c
c......Left/Right/Min
c
          do 600 j=4,9,1
              inc    = inc    + 1
              if (FRMFNC(inc,i) .eq. 0) then
                  FMTDES(j,i) = 0
              else
                  ist    = FRMST(inc)
                  ien    = FRMST(inc) + FRMFNC(inc,i) - 1
                  call ctoi (sbuf(ist:ien),inum,ierr)
                  if (inum .lt. 0 .or. inum .gt. 9 .or. ierr .ne. 0)
     1                    go to 9000
                  FMTDES(j,i) = inum
              endif
  600     continue
c
c......Ending string
c
          inc    = 11
          if (FRMFNC(inc,i) .ne. 0) then
              ist    = FRMST(inc)
              ien    = FRMST(inc) + FRMFNC(inc,i) - 1
              REGEN(i) = sbuf(ist:ien)
          endif
          REGENC(i) = FRMFNC(inc,i)
c
c......Control
c
          inc    = 12
          if (FRMFNC(inc,i) .eq. 0) go to 9000
          ist    = FRMST(inc)
          ien    = FRMST(inc) + FRMFNC(inc,i) - 1
          call getvnr (sbuf(ist:ien),ictrl,6,inum,MENWRD,MENWVL,NMENWD)
          if (inum .eq. 0) go to 9000
          FMTDES(10,i) = inum   - 1
  700 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call errstr (ebuf,REGID(i),0)
      if (FRMFNC(inc,i) .eq. 0) then
          abuf   = ' '
      else
          ist    = FRMST(inc)
          ien    = FRMST(inc) + FRMFNC(inc,i) - 1
          abuf   = sbuf(ist:ien)
      endif
      call errstr (ebuf,abuf,0)
c
c......Display the error message
c
      ibeg   = i
      MFERPT = i
      call plott (FRMBEG,1)
      call clreos
      call errmsg (ebuf,2,2)
      if (IMSCAN .eq. 2) go to 8000
      go to 500
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ggroup (kfl,cmsg,kerr)
c
c   FUNCTION:  G-code group definition form handling routine.
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
      subroutine ggroup (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY, POSMAP(0003)), (GRANGE,POSMAP(0801))
c
      real*8 DUMMY,GRANGE(14,11)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*2 ifmt(10)
      integer*4 i,j,k,ist,ien,ipt1,ipt2,ibeg,inc,ierr,irg,strlen1
c
      real*8 rnum
c
      character*20 abuf
      character*80 sbuf,ebuf
c
      data ifmt /4,0,1,5,5,5,5,1,0,1/
c
c...Initialize routine
c
      MLEVL(NLEVL) = 0
c
c...Define form layout
c
      FRMBEG = 5
      FRMNFD = 8
      FRMREC = 22
      FRMWID = 75
      if (IMSCAN .eq. 1) FRMWID = 6
c
      FRMST(1) = 1
      FRMST(2) = 8
      FRMST(3) = 18
      FRMST(4) = 28
      FRMST(5) = 38
      FRMST(6) = 48
      FRMST(7) = 58
      FRMST(8) = 68
c
      FRMEN(1) = 2
      FRMEN(2) = 15
      FRMEN(3) = 25
      FRMEN(4) = 35
      FRMEN(5) = 45
      FRMEN(6) = 55
      FRMEN(7) = 65
      FRMEN(8) = 75
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 3
      FRMTYP(2) = 4
      FRMTYP(3) = 4
      FRMTYP(4) = 4
      FRMTYP(5) = 4
      FRMTYP(6) = 4
      FRMTYP(7) = 4
      FRMTYP(8) = 4
c
c...Set up form records
c
      ipt1   = 0
      do 300 i=1,FRMREC/2,1
          sbuf   = ' '
          ipt1   = ipt1   + 1
          do 80 j=2,8,1
              FRMFNC(j,ipt1) = 0
              FRMFNC(j,ipt1+1) = 0
   80     continue
c
c......Register label
c
          sbuf(1:2) = REGID(i+17)
          FRMFNC(1,ipt1) = 2
c
c......G-codes
c
          do 100 j=1,7,1
              if (GRANGE(j,i) .eq. DUMMY) go to 150
              call ftoc (GRANGE(j,i),abuf,FRMFNC(j+1,ipt1),ifmt)
              ist    = FRMST(j+1)
              ien    = FRMST(j+1) + FRMFNC(j+1,ipt1) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j+1,ipt1))
  100     continue
  150     FRMBUF(ipt1) = sbuf
          ipt1   = ipt1   + 1
          sbuf    = ' '
          if (IMSCAN .eq. 1) sbuf = REGID(i+17)
          FRMFNC(1,ipt1) = 2
          if (GRANGE(j,i) .eq. DUMMY) go to 250
          do 200 j=1,7,1
              if (GRANGE(j+7,i) .eq. DUMMY) go to 250
              call ftoc (GRANGE(j+7,i),abuf,FRMFNC(j+1,ipt1),ifmt)
              ist    = FRMST(j+1)
              ien    = FRMST(j+1) + FRMFNC(j+1,ipt1) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j+1,ipt1))
  200     continue
  250     FRMBUF(ipt1) = sbuf
  300 continue
c
c...Go get form input from Motif
c
      if (IMSCAN .eq. 1) then
          do 350 i=1,FRMNFD,1
              FRMLAB(i) = ' '
  350     continue
          IMTYP  = 3
          go to 8000
      endif
c
c...Display the form header
c
      call getsap ('GRANG1',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 irg    = 0
      do 700 i=1,FRMREC,2
          sbuf   = FRMBUF(i)
c
c......Get number of chars in each field
c......for Motif interface
c
          if (IMSCAN .eq. 2) then
              do 520 j=1,FRMNFD,1
                  FRMFNC(j,i) = strlen1(sbuf(FRMST(j):FRMEN(j)))
  520         continue
          endif
c
c......G-codes
c
          inc    = 1
          ipt1   = 0
          ipt2   = i
          irg    = irg    + 1
          do 600 j=1,14,1
              inc    = inc    + 1
              if (FRMFNC(inc,ipt2) .eq. 0) go to 550
              ist    = FRMST(inc)
              ien    = FRMST(inc) + FRMFNC(inc,ipt2)
              call ctor (sbuf(ist:ien),rnum,ierr)
              if (ierr .eq. 1) go to 9000
              ipt1   = ipt1   + 1
              GRANGE(ipt1,irg) = rnum
  550         if (j .eq. 7) then
                  ipt2   = ipt2 + 1
                  sbuf   = FRMBUF(ipt2)
c
c......Get number of chars in each field
c......for Motif interface
c
                  if (IMSCAN .eq. 2) then
                      do 580 k=1,FRMNFD,1
                          FRMFNC(k,ipt2) =
     1                        strlen1(sbuf(FRMST(k):FRMEN(k)))
  580                 continue
                  endif
                  inc    = 1
              endif
  600     continue
          if (ipt1 .ne. 14) GRANGE(ipt1+1,irg) = DUMMY
  700 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call errstr (ebuf,REGID(irg+17),0)
      if (FRMFNC(inc,ipt2) .eq. 0) then
          abuf   = ' '
      else
          ist    = FRMST(inc)
          ien    = FRMST(inc) + FRMFNC(inc,ipt2) - 1
          abuf   = sbuf(ist:ien)
      endif
      call errstr (ebuf,abuf,0)
c
c......Display the error message
c
      ibeg   = i
      MFERPT = i
      call plott (FRMBEG,1)
      call clreos
      call errmsg (ebuf,2,2)
      if (IMSCAN .eq. 2) go to 8000
      go to 500
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mgroup (kfl,cmsg,kerr)
c
c   FUNCTION:  M-code group definition form handling routine.
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
      subroutine mgroup (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (DUMMY, POSMAP(0003)), (MRANGE,POSMAP(0955))
c
      real*8 DUMMY,MRANGE(7,11)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*2 ifmt(10)
      integer*4 i,j,ist,ien,ipt1,ibeg,ierr,strlen1
c
      real*8 rnum
c
      character*20 abuf
      character*80 sbuf,ebuf
c
      data ifmt /4,0,1,5,5,5,5,1,0,1/
c
c...Initialize routine
c
      MLEVL(NLEVL) = 0
c
c...Define form layout
c
      FRMBEG = 5
      FRMNFD = 8
      FRMREC = 11
      FRMWID = 75
      if (IMSCAN .eq. 1) FRMWID = 6
c
      FRMST(1) = 1
      FRMST(2) = 8
      FRMST(3) = 18
      FRMST(4) = 28
      FRMST(5) = 38
      FRMST(6) = 48
      FRMST(7) = 58
      FRMST(8) = 68
c
      FRMEN(1) = 2
      FRMEN(2) = 15
      FRMEN(3) = 25
      FRMEN(4) = 35
      FRMEN(5) = 45
      FRMEN(6) = 55
      FRMEN(7) = 65
      FRMEN(8) = 75
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 3
      FRMTYP(2) = 4
      FRMTYP(3) = 4
      FRMTYP(4) = 4
      FRMTYP(5) = 4
      FRMTYP(6) = 4
      FRMTYP(7) = 4
      FRMTYP(8) = 4
c
c...Set up form records
c
      do 300 i=1,FRMREC,1
          sbuf   = ' '
          do 80 j=2,8,1
              FRMFNC(j,i) = 0
   80     continue
c
c......Register label
c
          sbuf(1:2) = REGID(i+36)
          FRMFNC(1,i) = 2
c
c......M-codes
c
          do 100 j=1,7,1
              if (MRANGE(j,i) .eq. DUMMY) go to 150
              call ftoc (MRANGE(j,i),abuf,FRMFNC(j+1,i),ifmt)
              ist    = FRMST(j+1)
              ien    = FRMST(j+1) + FRMFNC(j+1,i) - 1
              sbuf(ist:ien) = abuf(1:FRMFNC(j+1,i))
  100     continue
  150     FRMBUF(i) = sbuf
  300 continue
c
c...Go get form input from Motif
c
      if (IMSCAN .eq. 1) then
          do 350 i=1,FRMNFD,1
              FRMLAB(i) = ' '
  350     continue
          IMTYP  = 3
          go to 8000
      endif
c
c...Display the form header
c
      call getsap ('GRANG1',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 do 700 i=1,FRMREC,1
          sbuf   = FRMBUF(i)
c
c......Get number of chars in each field
c......for Motif interface
c
          if (IMSCAN .eq. 2) then
              do 520 j=1,FRMNFD,1
                  FRMFNC(j,i) = strlen1(sbuf(FRMST(j):FRMEN(j)))
  520         continue
          endif
          ipt1   = 0
c
c......M-codes
c
          do 600 j=2,8,1
              if (FRMFNC(j,i) .eq. 0) go to 600
              ist    = FRMST(j)
              ien    = FRMST(j) + FRMFNC(j,i)
              call ctor (sbuf(ist:ien),rnum,ierr)
              if (ierr .eq. 1) go to 9000
              ipt1   = ipt1   + 1
              MRANGE(ipt1,i) = rnum
  600     continue
          if (ipt1 .ne. 7) MRANGE(ipt1+1,i) = DUMMY
  700 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call errstr (ebuf,REGID(i+36),0)
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
      ibeg   = i
      MFERPT = i
      call plott (FRMBEG,1)
      call clreos
      call errmsg (ebuf,2,2)
      if (IMSCAN .eq. 2) go to 8000
      go to 500
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setreg (kfl,cmsg,kerr)
c
c   FUNCTION:  Register value definition form handling routine.
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
      subroutine setreg (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IUNIT ,KPOSMP(0087)), (IREGST,KPOSMP(0497))
      equivalence (FMTDES,KPOSMP(2133)), (IREGVL,KPOSMP(3614))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 IUNIT,IREGVL(MAXFMT),IREGST(MAXFMT)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
      equivalence (REGVAL,POSMAP(5000))
c
      real*8 DUMMY,REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ist,ien,ipt1,ibeg,ierr,inc,iacy,strlen1
c
      real*8 rnum
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
      FRMBEG = 5
      FRMNFD = 8
      FRMREC = 23
      FRMWID = 78
c
c...Motif - Store total number of fields in form
c
      if (IMSCAN .eq. 1) FRMWID = 184
c
      FRMST(1) = 1
      FRMST(2) = 4
      FRMST(3) = 21
      FRMST(4) = 24
      FRMST(5) = 41
      FRMST(6) = 44
      FRMST(7) = 61
      FRMST(8) = 64
c
      FRMEN(1) = 3
      FRMEN(2) = 18
      FRMEN(3) = 23
      FRMEN(4) = 38
      FRMEN(5) = 43
      FRMEN(6) = 58
      FRMEN(7) = 63
      FRMEN(8) = 78
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 3
      FRMTYP(2) = 4
      FRMTYP(3) = 3
      FRMTYP(4) = 4
      FRMTYP(5) = 3
      FRMTYP(6) = 4
      FRMTYP(7) = 3
      FRMTYP(8) = 4
c
c...Set up form records
c
      ipt1   = 0
      do 300 i=1,FRMREC,1
          sbuf   = ' '
          inc    = 1
          do 200 j=1,8,2
              if (i .le. FRMREC) then
c
c......Register label
c
                  ipt1   = ipt1   + 1
                  sbuf(inc:inc+1) = REGID(ipt1)
                  sbuf(inc+2:inc+2) = '='
                  FRMFNC(j,i) = 3
                  inc    = inc   + 20
c
c......Register value
c
                  call rtoc (REGSTO(ipt1),abuf,FRMFNC(j+1,i))
                  ist    = FRMST(j+1)
                  ien    = FRMST(j+1) + FRMFNC(j+1,i) - 1
                  sbuf(ist:ien) = abuf(1:FRMFNC(j+1,i))
              else
                  FRMFNC(j,i) = -1
                  FRMFNC(j+1,i) = -1
              endif
  200     continue
          FRMBUF(i) = sbuf
  300 continue
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
      call getsap ('SETREG',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
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
c......Register value
c
          do 600 j=2,8,2
              ipt1   = ipt1   + 1
              if (FRMFNC(j,i) .eq. 0) then
                  REGSTO(ipt1) = 0.
                  REGVAL(ipt1) = 0.
              else
                  ist    = FRMST(j)
                  ien    = FRMST(j) + FRMFNC(j,i)
                  call ctor (sbuf(ist:ien),rnum,ierr)
                  if (ierr .eq. 1) go to 9000
                  REGSTO(ipt1) = rnum
                  REGVAL(ipt1) = rnum
                  iacy   = FMTDES(5+IUNIT,ipt1)
                  call dpoint (rnum,rnum,iacy)
                  rnum   = rnum   * 10.D0**iacy
                  IREGVL(ipt1) = rnum
                  IREGST(ipt1) = rnum
              endif
  600     continue
  700 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call errstr (ebuf,REGID(ipt1),0)
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
c   SUBROUTINE:  wrdord (kfl,cmsg,kerr)
c
c   FUNCTION:  Order of registers (words) in output block form handling
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
      subroutine wrdord (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NRGORD,KPOSMP(0837)), (REGORD,KPOSMP(3707))
c
      integer*4 REGORD(MAXFMT),NRGORD
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      real*8 r
c
      integer*4 i,j,ist,ien,ipt1,ibeg,ierr,inc,k,ireg(MAXFMT),nreg,ie,
     1          strlen1,iist,ijst
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
      FRMBEG = 5
      FRMNFD = 19
      FRMREC = 5
      FRMWID = 74
c
c...Motif - Store total number of fields in form
c
      if (IMSCAN .eq. 1) FRMWID = MAXFMT
c
      FRMST(1) = 1
      FRMST(2) = 5
      FRMST(3) = 9
      FRMST(4) = 13
      FRMST(5) = 17
      FRMST(6) = 21
      FRMST(7) = 25
      FRMST(8) = 29
      FRMST(9) = 33
      FRMST(10) = 37
      FRMST(11) = 41
      FRMST(12) = 45
      FRMST(13) = 49
      FRMST(14) = 53
      FRMST(15) = 57
      FRMST(16) = 61
      FRMST(17) = 65
      FRMST(18) = 69
      FRMST(19) = 73
c
      FRMEN(1) = 2
      FRMEN(2) = 6
      FRMEN(3) = 10
      FRMEN(4) = 14
      FRMEN(5) = 18
      FRMEN(6) = 22
      FRMEN(7) = 26
      FRMEN(8) = 30
      FRMEN(9) = 34
      FRMEN(10) = 38
      FRMEN(11) = 42
      FRMEN(12) = 46
      FRMEN(13) = 50
      FRMEN(14) = 54
      FRMEN(15) = 58
      FRMEN(16) = 62
      FRMEN(17) = 66
      FRMEN(18) = 70
      FRMEN(19) = 74
c
c...Returning answers from Motif
c...Go parse them
c
      if (IMSCAN .eq. 2) go to 510
c
      FRMTYP(1) = 4
      FRMTYP(2) = 4
      FRMTYP(3) = 4
      FRMTYP(4) = 4
      FRMTYP(5) = 4
      FRMTYP(6) = 4
      FRMTYP(7) = 4
      FRMTYP(8) = 4
      FRMTYP(9) = 4
      FRMTYP(10) = 4
      FRMTYP(11) = 4
      FRMTYP(12) = 4
      FRMTYP(13) = 4
      FRMTYP(14) = 4
      FRMTYP(15) = 4
      FRMTYP(16) = 4
      FRMTYP(17) = 4
      FRMTYP(18) = 4
      FRMTYP(19) = 4
c
c...Set up form records
c
      ipt1   = 0
      do 300 i=1,FRMREC,1
          sbuf   = ' '
          inc    = 1
          do 200 j=1,19,1
              ipt1   = ipt1   + 1
              if (ipt1 .gt. NRGORD) go to 350
              sbuf(inc:inc+1) = REGID(REGORD(ipt1))
              inc    = inc    + 4
              FRMFNC(j,i) = 2
  200     continue
          FRMBUF(i) = sbuf
  300 continue
      go to 400
c
c......Zero out leftover fields
c
  350 do 380 i=i,FRMREC,1
          do 360 j=j,19,1
              FRMFNC(j,i) = 0
  360     continue
          FRMBUF(i) = sbuf
          sbuf   = ' '
          j      = 1
  380 continue
c
c...Go get form input from Motif
c
      if (IMSCAN .eq. 1) then
          IMTYP  = 5
          go to 8000
      endif
c
c...Display the form header
c
  400 call getsap ('WRDORD',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 NRGORD = 0
      do 520 i=1,MAXFMT,1
          ireg(i) = 0
  520 continue
      iist = 1
      ijst = 1
c
  530 do 700 i=iist,FRMREC,1
          call touppr (FRMBUF(i),sbuf)
          ie     = 19
          if (i .eq. FRMREC) ie = 9
          do 600 j=ijst,19,1
              if (IMSCAN .eq. 2)
     1            FRMFNC(j,i) = strlen1(sbuf(FRMST(j):FRMEN(j)))
              if (FRMFNC(j,i) .ne. 0) then
                  ist    = FRMST(j)
                  ien    = FRMST(j) + FRMFNC(j,i) - 1
                  call ctocd (sbuf(ist:ien),ien-ist+1,k,r,ierr)
                  if (k .le. 0 .or. ierr .ne. 0) go to 9000
  540             inc    = k
                  do 560 k=1,NRGORD,1
                      if (inc .eq. REGORD(k)) go to 9000
  560             continue
                  NRGORD = NRGORD + 1
                  REGORD(NRGORD) = inc
                  ireg(inc) = 1
              endif
  600     continue
          ijst   = 1
  700 continue
c
c...Add rest of registers to array
c
      nreg   = NRGORD
      do 1000 i=1,MAXFMT,1
          if (ireg(i) .eq. 0) then
              nreg   = nreg   + 1
              REGORD(nreg) = i
          endif
 1000 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call errstr (ebuf,' ',0)
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
      if (IMSCAN .eq. 2) then
          iist   = i
          ijst   = j      + 1
          go to 530
      else
          go to 500
      endif
      end
c
c***********************************************************************
c
c   SUBROUTINE:  usrblk (kfl,cmsg,kerr)
c
c   FUNCTION:  User defined blocks form handling routine.
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
      subroutine usrblk (kfl,cmsg,kerr)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (USRBRG,KPOSMP(2600)), (MUSRBK,KPOSMP(3022))
      equivalence (NCUSRB,KPOSMP(3025))
c
      integer*4 USRBRG(20,20),MUSRBK,NCUSRB(20)
c
      equivalence (DUMMY, POSMAP(0003)), (USRBVL,POSMAP(2501))
c
      real*8 DUMMY,USRBVL(20,20)
c
      integer*4 kfl,kerr
c
      character*(*) cmsg
c
      integer*4 i,j,ist,ien,nc,ipt,ibeg,inc,ierr,nca,inum,iary(20),
     1          strlen1
      logical*4 lnum,lreg
c
      real*8 rnum,rary(20)
c
      character*80 abuf,sbuf,ebuf
c
c...Initialize routine
c
      MLEVL(NLEVL) = 0
c
c...Define form layout
c
      FRMBEG = 5
      FRMNFD = 2
      FRMREC = MUSRBK
      FRMWID = 79
c
c...Motif - Store total number of fields in form
c
      if (IMSCAN .eq. 1) FRMWID = MUSRBK * 2
c
      FRMST(1) = 1
      FRMST(2) = 6
c
      FRMEN(1) = 5
      FRMEN(2) = 79
      if (IMSCAN .eq. 2) go to 510
c
c...Set the type of form input
c
      FRMTYP(1) = 3
      FRMTYP(2) = 4
c
c...Set up form records
c
      do 300 i=1,FRMREC,1
c
c......Block number
c
          call itoc (i,abuf,nc,0)
          sbuf   = '#' // abuf(1:nc)
          nca    = 5
          FRMFNC(1,i) = 5
c
c......Block registers
c
          if (NCUSRB(i) .eq. 0) then
              FRMBUF(i) = sbuf
              FRMFNC(2,i) = 0
          else
              do 100 j=1,NCUSRB(i),1
                  if (USRBRG(j,i) .gt. 1000) then
                      inum   = USRBRG(j,i) - 1000
                      call itoc (inum,sbuf(nca+1:80),nc,0)
                  else
                      call cdtoc (USRBRG(j,i),USRBVL(j,i),
     1                            sbuf(nca+1:80),nc)
                  endif
                  nca    = nca    + nc
                  if (j .ne. NCUSRB(i)) then
                      nca    = nca    + 1
                      sbuf(nca:nca) = ','
                  endif
  100         continue
              FRMBUF(i) = sbuf
              FRMFNC(2,i) = nca    - 5
          endif
  300 continue
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
      call getsap ('USRBLK1',FRMHPT(1),IPRMDS,SALABL)
      NFRMHD = 1
c
      call plott (3,1)
      call clreos
c      call dmpbuf (SAPRM(ipt1),SAPNC(ipt1))
c
c...Activate the form
c
      ibeg   = 1
  500 call form (ibeg,ierr)
      if (ierr .ne. 0) go to 8000
c
c...Parse form input
c
  510 do 800 i=1,FRMREC,1
c
c......Get number of chars in each field
c......for Motif interface
c
          if (IMSCAN .eq. 2) then
              do 550 j=1,FRMNFD,1
                  FRMFNC(j,i) = strlen1(FRMBUF(i)(FRMST(j):FRMEN(j)))
  550         continue
          endif
c
c......Block registers
c
          if (FRMFNC(2,i) .eq. 0) then
              NCUSRB(i) = 0
          else
              sbuf   = FRMBUF(i)
              nc     = FRMFNC(2,i) + FRMST(2) - 1
              inc    = 0
              ipt    = FRMST(2)
              lreg   = .false.
              lnum   = .false.
  600         nca    = index(sbuf(ipt:nc),',')
              if (nca .eq. 0) then
                  nca    = nc     + 1
              else
                  nca    = ipt    + nca    - 1
              endif
              ierr   = 0
              call ctocd (sbuf(ipt:nca-1),nca-ipt,inum,rnum,ierr)
              if (ierr .eq. 1) then
                  call ctoi (sbuf(ipt:nca-1),inum,ierr)
                  if (ierr .eq. 1 .or. inum .lt. 1 .or. inum .gt. 30)
     1                go to 9000
                  if (lreg) go to 9000
                  lnum   = .true.
                  inum   = inum   + 1000
                  rnum   = DUMMY
              else
                  if (lnum) go to 9000
                  lreg   = .true.
              endif
c
              if (inc .eq. 20) go to 9000
c
              inc    = inc    + 1
              iary(inc) = inum
              rary(inc) = rnum
              ipt    = nca     + 1
              if (ipt .le. nc) go to 600
c
              NCUSRB(i) = inc
              do 700 j=1,inc,1
                  USRBRG(j,i) = iary(j)
                  USRBVL(j,i) = rary(j)
  700         continue
          endif
  800 continue
c
c...End of routine
c
 8000 return
c
c...Error parsing input
c
 9000 call errtxt ('INVFRMI',ebuf)
      call itoc (i,abuf,nc,0)
      call errstr (ebuf,abuf,0)
      if (FRMFNC(1,i) .eq. 0) then
          abuf   = ' '
      else
          ist    = ipt
          ien    = nca    - 1
          abuf   = sbuf(ist:ien)
      endif
      call errstr (ebuf,abuf,0)
c
c......Display the error message
c
      ibeg   = i
      call plott (FRMBEG,1)
      call clreos
      call errmsg (ebuf,2,2)
      if (IMSCAN .eq. 2) go to 8000
      go to 500
      end
