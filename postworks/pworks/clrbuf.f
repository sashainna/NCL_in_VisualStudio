c
c***********************************************************************
c
c   FILE NAME: clrbuf.for
c   CONTAINS:
c               blkout  chksum  clrbuf  clrreg  fmtspc  frcblk  mbkout
c               oskout  pshblk  sequnc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        clrbuf.f , 24.4
c     DATE AND TIME OF LAST  MODIFICATION
c        05/29/14 , 16:05:20
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  blkout (cbuf,knc,kfl)
c
c   FUNCTION:  This routine builds the Control Tape output buffers for
c              the print and punch file.  It also does the final format-
c              ting for tab sequential output.  This routine should be
c              called with a single formatted register string.
c
c   INPUT:  cbuf    C*n  D1  -  Text string to append to Control Tape
c                               buffers.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c           kfl     I*4  D1  -  1 = Last string to append to Control
c                               Tape buffers. -1 = Clear current output
c                               buffers and do not output anything, also
c                               resets the previous sequence number.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine blkout (cbuf,knc,kfl)
c
      include 'post.inc'
c
      equivalence (IOPSKP,KPOSMP(0092))
      equivalence (TABSEQ,KPOSMP(0842)), (ISEQBK,KPOSMP(0852))
      equivalence (ISQBKV,KPOSMP(0853)), (NOPSKP,KPOSMP(1077))
      equivalence (PCHSPC,KPOSMP(1103)), (PCHBNC,KPOSMP(1109))
      equivalence (PRTBNC,KPOSMP(1153)), (NOPSON,KPOSMP(4050))
c
      integer*4 TABSEQ,PCHSPC,PCHBNC,PRTBNC,IOPSKP,NOPSKP,ISEQBK,
     -          NOPSON,ISQBKV
c
      equivalence (ISEQ  ,POSMAP(1164)), (ISEQSV,POSMAP(1182))
c
      real*8 ISEQ,ISEQSV
c
      equivalence (LEDCHR,CPOSMP(0986)), (TABCHR,CPOSMP(0987))
      equivalence (LSTBLK,CPOSMP(0988))
      equivalence (LOPSKP,CPOSMP(1648)), (PRTBLK,CPOSMP(1653))
      equivalence (PCHBLK,CPOSMP(3341))
c
      character*1 LEDCHR,TABCHR
      character*5 LOPSKP
      character*512 PCHBLK,LSTBLK,PRTBLK
c
      integer*4 knc,kfl
c
      character*(*) cbuf
c
      integer*4 i
c
      character*512 obuf,sbuf
c
c...Clear output buffers only
c
      if (kfl .eq. -1 .and. PCHBNC .ne. 0) then
          PCHBNC = 0
          PRTBNC = 0
          ISEQBK = ISQBKV
          ISEQ   = ISEQSV
          go to 8000
      endif
c
c...OPSKIP/ON
c...Add optional skip character(s)
c
      if (IOPSKP .eq. 1 .and. PCHBNC .eq. 0 .and. NOPSKP .gt. 0) then
          if (NOPSON .eq. 0) then
             PRTBLK = LOPSKP(1:NOPSKP)
             PCHBLK = LOPSKP(1:NOPSKP)
             PCHBNC = NOPSKP
             PRTBNC = NOPSKP
          else
             call oskout
          end if
      endif
c
c...Separate registers with a space
c
      if (knc .eq. 0) go to 1000
      if (kfl .eq. 0 .and. PCHBNC .ne. 0) then
          if (PRTBLK(PRTBNC:PRTBNC) .ne. TABCHR .and.
     1        PRTBLK(PRTBNC:PRTBNC) .ne. ' ') then
              obuf   = PRTBLK(1:PRTBNC) // ' '
              PRTBNC = PRTBNC + 1
          else
              obuf   = PRTBLK(1:PRTBNC)
          endif
          if (PCHSPC .eq. 1 .and. PCHBLK(PCHBNC:PCHBNC) .ne. TABCHR
     1        .and. PCHBLK(PCHBNC:PCHBNC) .ne. LEDCHR) then
              sbuf   = PCHBLK(1:PCHBNC) // LEDCHR
              PCHBNC = PCHBNC + 1
          else
              sbuf   = PCHBLK(1:PCHBNC)
          endif
      else
          obuf   = PRTBLK
          sbuf   = PCHBLK
      endif
c
c...Standard Tab sequential
c...Get rid of extra tabs
c
 1000 if (kfl .eq. 0) go to 2000
      if (TABSEQ .ne. 1) then
          do 1100 i=PRTBNC,1,-1
              if (PRTBLK(i:i) .ne. TABCHR) go to 1150
 1100     continue
          i      = 0
 1150     PRTBNC = i
c
          do 1200 i=PCHBNC,1,-1
              if (PCHBLK(i:i) .ne. TABCHR) go to 1250
 1200     continue
          i      = 0
 1250     PCHBNC = i
      endif
c
c...Append register string to output buffers
c
 2000 if (knc .eq. 0) go to 8000
      if (PRTBNC .eq. 0) then
          PRTBLK = cbuf(1:knc)
      else
          PRTBLK = obuf(1:PRTBNC) // cbuf(1:knc)
      endif
      PRTBNC = PRTBNC + knc
      if (PCHBNC .eq. 0) then
          PCHBLK = cbuf(1:knc)
      else
          PCHBLK = sbuf(1:PCHBNC) // cbuf(1:knc)
      endif
      PCHBNC = PCHBNC + knc
c
c...End of routine
c
 8000 LSTBLK = PCHBLK
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  chksum
c
c   FUNCTION:  This routine calculates the checksum code (CHECK/LENGTH)
c              and appends it to the output block.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine chksum
c
      include 'post.inc'
c
      equivalence (PCHBNC,KPOSMP(1109)), (ICSMRG,KPOSMP(1132))
      equivalence (MXCKSM,KPOSMP(1134))
c
      integer*4 PCHBNC,ICSMRG,MXCKSM
c
      equivalence (ISEQ  ,POSMAP(1164)), (ISEQSV,POSMAP(1182))
c
      real*8 ISEQ,ISEQSV
c
      equivalence (PCHBLK,CPOSMP(3341))
c
      character*512 PCHBLK
c
      integer*4 i,inum,nc
c
      real*8 rnum
c
      character*60 sbuf
c
c...Calculate checksum for this block
c
      if (ICSMRG .eq. 0) go to 8000
      inum   = 0
      do 100 i=1,PCHBNC,1
          if (PCHBLK(i:i) .ne. ' ') inum = inum + ichar(PCHBLK(i:i))
  100 continue
      inum   = inum - ((inum/MXCKSM) * MXCKSM)
      if (inum .eq. 0) inum = MXCKSM
c
c...Format register for output
c
      rnum   = inum
      call fmtcod (ICSMRG,rnum,sbuf,nc)
c
c....Output checksum register
c
      call blkout (sbuf,nc,0)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clrbuf
c
c   FUNCTION:  This routine makes sure the following conditions are met
c              before outputting a tape block.
c
c              1.  First block output contains Starting registers.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrbuf
c
      include 'post.inc'
c
      equivalence (MOTFLG,KPOSMP(1073)), (NOUTSW,KPOSMP(1074))
      equivalence (IFIRST,KPOSMP(1131)), (MOT1ST,KPOSMP(1349))
      equivalence (MOBLK ,KPOSMP(3023)), (STBLK ,KPOSMP(3024))
      equivalence (NMOBLK,KPOSMP(3045)), (USRSTK,KPOSMP(3049))
      equivalence (NUSRST,KPOSMP(3059))
c
      integer*4 NOUTSW,IFIRST,MOT1ST,MOTFLG,MOBLK,STBLK,NMOBLK,NUSRST,
     1          USRSTK(10)
c
      integer*4 ifrc,nc,i
c
      character*60 ldat
c
      if (NOUTSW .eq. 0) go to 8000
      ifrc   = 2
c
c...Make sure start up codes are included
c
      if (IFIRST .eq. 1) then
          call frcblk (STBLK,ifrc,1)
          IFIRST = 0
      endif
c
c...First motion block output
c...Make sure start up codes are included
c
      if (MOT1ST .eq. 1 .and. MOTFLG .eq. 1) then
          call frcblk (MOBLK,ifrc,1)
          MOT1ST = 0
      else if (MOTFLG .ne. 1) then
          call frcblk (NMOBLK,ifrc,1)
      endif
c
c...Force all User defined blocks
c...that are currently on the stack
c
      do 300 i=1,NUSRST,1
          call frcblk (USRSTK(i),ifrc,1)
  300 continue
      NUSRST = 0
c
c...Output sequence number
c
      if (ifrc .ne. 1) then
          call sequnc (1,ldat,nc,ifrc)
c
c...Clear output registers
c
          call clrreg
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clrreg
c
c   FUNCTION:  This routine sets up an entire tape block for output.  It
c              also determines which registers will be output depending
c              on which have been set and their control code.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrreg
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405))
      equivalence (IREGST,KPOSMP(0497)), (REGFRC,KPOSMP(0603))
      equivalence (IRGOUT,KPOSMP(0695))
      equivalence (NEOB  ,KPOSMP(0841)), (TABSEQ,KPOSMP(0842))
      equivalence (NCAPPN,KPOSMP(0856)), (LSTREG,KPOSMP(0857))
      equivalence (MACPRG,KPOSMP(0858))
      equivalence (MOTFLG,KPOSMP(1073)), (NOUTSW,KPOSMP(1074))
      equivalence (ICSMFL,KPOSMP(1133)), (REGBNC,KPOSMP(2001))
      equivalence (FMTDES,KPOSMP(2133)), (REGENC,KPOSMP(3522))
      equivalence (IREGVL,KPOSMP(3614)), (REGORD,KPOSMP(3707))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGBNC(MAXFMT),REGENC(MAXFMT),REGORD(MAXFMT),NEOB,
     1          TABSEQ,REGSW(MAXFMT),IREGVL(MAXFMT),IREGST(MAXFMT),
     2          REGFRC(MAXFMT),MOTFLG,NOUTSW,ICSMFL,IRGOUT(MAXFMT),
     3          NCAPPN,LSTREG,MACPRG
c
      equivalence (REGSTO,POSMAP(1032)), (REGVAL,POSMAP(5000))
c
      real*8 REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      equivalence (LEOB  ,CPOSMP(0971)), (TABCHR,CPOSMP(0987))
      equivalence (APPSTR,CPOSMP(5987))
c
      character*1 TABCHR
      character*5 LEOB
      character*512 APPSTR
c
      integer*4 i,inc,irinc,irsav(MAXFMT),nc,iout,isq
c
      character*30 sbuf
c
c...Loop through register arrays
c...to determine which ones need
c...to be output
c
      if (NOUTSW .eq. 0) go to 8000
      iout  = 0
      irinc = 0
      isq   = 0
      do 1000 i=1,MAXFMT,1
          inc    = REGORD(i)
          IRGOUT(inc) = 0
c
c......NOT USED
c
          if (FMTDES(10,inc) .eq. 0) then
              REGFRC(inc) = 0
              go to 1000
          endif
c
c......ALWAYS
c
          if (FMTDES(10,inc) .eq. 1) REGSW(inc) = 1
c
c......CHANGED
c
          if (REGSW(inc) .eq. 1) then
              if (FMTDES(10,inc) .eq. 3) then
                  if (IREGVL(inc) .eq. IREGST(inc)) REGSW(inc) = 0
c
c......NON ZERO
c
              else if (FMTDES(10,inc) .eq. 4) then
                  if (IREGVL(inc) .eq. 0) REGSW(inc) = 0
c
c......MOTION
c
              else if (FMTDES(10,inc) .eq. 5 .and. MOTFLG .eq. 0) then
                  irinc  = irinc  + 1
                  irsav(irinc) = inc
                  REGSW(inc) = 0
              endif
          endif
c
c......Register is to be output
c......Add Tab character
c
          if (TABSEQ .ne. 1) call blkout (TABCHR,1,0)
c
c......Force register output
c
          if (REGSW(inc) .eq. 0) then
              if (REGFRC(inc) .eq. 3) then
                  if (MOTFLG .eq. 1) then
                      call codint (inc,REGVAL(inc),REGVAL(inc),
     1                             IREGVL(inc))
                      REGSW(inc) = 1
                  endif
              else if (REGFRC(inc) .gt. 0) then
                  if (FMTDES(10,inc) .eq. 5 .and. MOTFLG .eq. 0)
     1                    go to 1000
                  call codint (inc,REGVAL(inc),REGVAL(inc),IREGVL(inc))
                  REGSW(inc) = 1
              endif
c
c......Suppress register output
c
          else
              if (REGFRC(inc) .eq. -1 .or. REGFRC(inc) .eq. -2)
     1                REGSW(inc) = 0
          endif
          if (REGFRC(inc) .eq. -1) REGFRC(inc) = 0
          if (REGSW(inc) .eq. 0) go to 1000
c
c......Do not output block if
c......only parameter definition
c......codes (SEQ #'s, etc) are being output
c
          if (REGFRC(inc) .ne. 4) isq = 1
c
c......Clear register force flag
c
          if (REGFRC(inc) .eq. 1 .or. REGFRC(inc) .eq. 3 .or.
     1        REGFRC(inc) .eq. 4 .or. REGFRC(inc) .eq. -1)
     2            REGFRC(inc) = 0
c
c......Format register for output and
c......Add this register to Control Tape
c
          call fmtspc (inc,REGVAL(inc))
c
c......Store this registers values
c
          REGSTO(inc) = REGVAL(inc)
          IREGST(inc) = IREGVL(inc)
          REGSW(inc) = 0
          IRGOUT(inc) = 1
          iout   = 1
c
c......Output Macro Parameters if a
c......Macro call is being output and
c......this is the correct position
c
          if (inc .eq. MACPRG) call mbkout
 1000 continue
c
c...Add APPEND string to the block if set
c
      if (NCAPPN .gt. 0) then
          call blkout (APPSTR,NCAPPN,0)
          NCAPPN = 0
      end if
c
c...Output checksum code
c
      if (iout .eq. 0) go to 1200
      if (ICSMFL .eq. 1) call chksum
c
c...Add end of block
c
      call blkout (LEOB,NEOB,1)
c
c...Output Tape Block
c
      if (isq .eq. 1) then
          call output
c
c...Sequence number is the only code being output
c...Suppress the output and reset the sequence number
c
      else
          call blkout (sbuf,nc,-1)
      endif
c
c...Reset Motion only registers
c
 1200 do 1300 i=1,irinc,1
          REGSW(irsav(i)) = 1
 1300 continue
      NOUTSW = irinc
      MOTFLG = 0
      MACPRG = -1
      LSTREG = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frcblk (kblk,kfrc,kfl)
c
c   FUNCTION:  This routine generates a user defined block for output.
c              It will force out all of requested registers and option-
c              ally their values in the next output block.
c
c   INPUT:  kblk    I*4  D1   -  User defined block to output.
c
c           kfl     I*4  D1   -  1 = It is ok to call 'sequnc' to output
c                                a sequence number.
c
c   OUTPUT: kfrc    I*4  D1   -  1 = Output an EOB with this user de-
c                                fined block.  Otherwise does not change
c                                the value.
c
c***********************************************************************
c
      subroutine frcblk (kblk,kfrc,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405)), (REGFRC,KPOSMP(0603))
      equivalence (IFBACT,KPOSMP(0810))
      equivalence (NOUTSW,KPOSMP(1074)), (USRBRG,KPOSMP(2600))
      equivalence (MUSRBK,KPOSMP(3022)), (NCUSRB,KPOSMP(3025))
      equivalence (IREGVL,KPOSMP(3614))
c
      integer*4 REGSW(MAXFMT),IREGVL(MAXFMT),REGFRC(MAXFMT),NOUTSW,
     1          USRBRG(20,20),MUSRBK,NCUSRB(20),IFBACT
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
      equivalence (USRBVL,POSMAP(2501)), (REGVAL,POSMAP(5000))
c
      real*8 DUMMY,REGSTO(MAXFMT),REGVAL(MAXFMT),USRBVL(20,20)
c
      integer*4 kblk,kfrc,kfl
c
      integer*4 i,j,inc,nc,fsteob, nxteob, savforce(MAXFMT)
c
      real*8 rnum
c
      character*60 ldat
c
c...Initialize routine
c
      IFBACT = IFBACT + 1
      if (kblk .le. 0 .or. kblk .gt. MUSRBK) go to 8000
c
c...Set the register force flag to omit (-1) for any registers
c...following the first end of block character (register -3).
c...This means that any register waiting to be output will not be
c...output twice.  Only save the register force flags on the first
c...entry to this routine during a recursive call.
c
      fsteob = 0
      if (IFBACT .eq. 1) then
          do 50 i=1,MAXFMT,1
              savforce(i) = REGFRC(i)
   50     continue
      endif
c
      do 100 i=1,NCUSRB(kblk),1
          inc    = USRBRG(i,kblk)
          rnum   = USRBVL(i,kblk)
          call regtyp (inc,rnum)
          if (inc .gt. 0 .and. inc .le. MAXFMT .and. fsteob .eq. 1)
     1        REGFRC(inc) = -1
          if (inc .eq. -3) fsteob = 1
  100 continue
c
      do 200 i=1,NCUSRB(kblk),1
          inc    = USRBRG(i,kblk)
          rnum   = USRBVL(i,kblk)
c
c......Get physical register
c
          call regtyp (inc,rnum)
c
c......End-of-Block
c
          if (inc .eq. -3) then
              kfrc   = 1
              if (kfl .eq. 1) call sequnc (1,ldat,nc,kfrc)
              call clrreg
c
c...Set the register force flag to omit (-1) for any registers
c...following the next end of block character (register -3).
c
              if (fsteob .eq. 1) then
                  nxteob = 0
                  do j=i+1,NCUSRB(kblk),1
                      inc    = USRBRG(j,kblk)
                      rnum   = USRBVL(j,kblk)
                      call regtyp (inc,rnum)
                      if (inc .gt. 0 .and. inc .le. MAXFMT .and.
     1                    nxteob .eq. 1) REGFRC(inc) = -1
                      if (inc .eq. -3) nxteob = 1
                  enddo
              endif
c
c......Force register output
c
          else if (inc.gt.0 .and. inc.le.MAXFMT) then
              if (REGSW(inc) .eq. 0) then
                  REGSW(inc) = 1
                  if (rnum.eq. DUMMY) then
                      call codint (inc,REGSTO(inc),REGVAL(inc),
     1                              IREGVL(inc))
c
                  else
                      call codint (inc,rnum,REGVAL(inc),IREGVL(inc))
                  endif
                  NOUTSW = NOUTSW + 1
              endif
              if (savforce(inc) .eq. 0) REGFRC(inc) = 1
          endif
  200 continue
c
c...Restore the register force flags.
c
      if (fsteob .eq. 1 .and. IFBACT .eq. 1) then
          do 300 i=1,MAXFMT,1
              if (REGFRC(i) .ne. 1) REGFRC(i) = savforce(i)
  300     continue
      endif
c
c...End of routine
c
 8000 IFBACT = IFBACT - 1
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  subroutine fmtspc (kreg,gval,cdat,knc)
c
c   FUNCTION:  This routine determines where a code is a duplicate
c              rotary axis code and formats it for output accordingly.
c              It then outputs all necessary codes.
c
c   INPUT:  kreg    I*4  D1   -  Register to format for output.  For
c                                kreg < 0 register value is not output
c
c           gval    R*8  D1   -  Register value.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fmtspc (kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (IRTDUP,KPOSMP(1413))
      equivalence (IRTDAC,KPOSMP(1417)), (IRTDEF,KPOSMP(1485))
      equivalence (REGBNC,KPOSMP(2001))
c
      integer*4 MOTREG(24),REGBNC(MAXFMT),IRTDUP(4),IRTDAC(9,4),IRTDEF
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 i,j,ipt(2,4),nc,inc,index
c
      character*1 lnum(9)
      character*60 sbuf
c
      data ipt /14,15,17,18,20,21,23,24/
c
      data lnum /'1','2,','3,','4','5','6','7','8','9'/
c
c...Determine if this code is a
c...duplicate rotary axis
c
      do 100 i=1,IRTDEF,1
          if (IRTDUP(i) .gt. 1) then
              if (kreg .eq. MOTREG(ipt(1,i)) .or.
     1            kreg .eq. MOTREG(ipt(2,i))) go to 500
          endif
  100 continue
c
c...Format regular code for output
c
      call fmtcod (kreg,gval,sbuf,nc)
      call blkout (sbuf,nc,0)
      go to 8000
c
c...Format duplicate rotary axis code
c...for output
c
  500 inc    = index(REGST(kreg)(1:REGBNC(kreg)),'#')
      if (inc .ne. 0) then
          do 600 j=1,IRTDUP(i),1
              if (IRTDAC(j,i) .eq. 1) then
                  REGST(kreg)(inc:inc) = lnum(j)
                  call fmtcod (kreg,gval,sbuf,nc)
                  call blkout (sbuf,nc,0)
              endif
  600     continue
          REGST(kreg)(inc:inc) = '#'
      else
          call fmtcod (kreg,gval,sbuf,nc)
          call blkout (sbuf,nc,0)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c
c   SUBROUTINE:  subroutine mbkout
c
c   FUNCTION:  This routine outputs the Macro Parameter values enclosed
c              in parentheses.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine mbkout
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MACPSW,KPOSMP(0859)), (MACBLK,KPOSMP(0889))
      equivalence (PCHSPC,KPOSMP(1103)), (MACSPC,KPOSMP(1110))
      equivalence (USRBRG,KPOSMP(2600)), (NCUSRB,KPOSMP(3025))
c
      integer*4 MACPSW(30),MACBLK,USRBRG(20,20),NCUSRB(20),MACSPC,
     1          PCHSPC
c
      equivalence (MACPRM,POSMAP(4300))
c
      real*8 MACPRM(30)
c
      integer*2 ifmt(10)
      integer*4 i,inc,ireg(30),iblk(30),nc,iend,iout,irinc,isq,isav
c
      character*1 paren(2)
      character*2 comma
      character*30 sbuf
c
      data ifmt /4,0,1,6,6,4,3,1,0,0/
      data paren /'(',')'/, comma /', '/
c
c...Determine block order
c
      do 100 i=1,30,1
          ireg(i) = i
  100 continue
c
c......Use User defined block
c
      if (MACBLK .ne. 0 .and. NCUSRB(MACBLK) .ne. 0) then
          inc    = 0
          do 200 i=1,NCUSRB(MACBLK),1
              if (USRBRG(i,MACBLK) .gt. 1000) then
                  inc    = inc   + 1
                  iblk(inc) = USRBRG(i,MACBLK) - 1000
                  ireg(iblk(inc)) = 0
              endif
  200     continue
          do 300 i=1,30,1
              if (ireg(i) .ne. 0) then
                  inc    = inc    + 1
                  iblk(inc) = ireg(i)
              endif
  300     continue
c
c......Use standard Macro Parameter order
c
      else
          do 500 i=1,30,1
              iblk(i) = ireg(i)
  500     continue
      endif
c
c...Determine last output register
c
      iend   = 0
      do 700 i=30,1,-1
          if (MACPSW(iblk(i)) .eq. 1) then
              iend   = i
              go to 720
          endif
  700 continue
  720 continue
c
c...Output opening '('
c
      call blkout (paren(1),1,0)
c
c...Loop through Macro Parameters
c
      if (iend .eq. 0) go to 8000
      iout  = 0
      irinc = 0
      isq   = 0
      isav   = PCHSPC
		PCHSPC = 2
      do 1000 i=1,iend,1
          inc    = iblk(i)
c
c......Output comma
c
          if (i .ne. 1) call blkout (comma,3-MACSPC,0)
c
c......Macro Parameter is being output
c
          if (MACPSW(inc) .eq. 1) then
              call ftoc (MACPRM(inc),sbuf,nc,ifmt)
              call blkout (sbuf,nc,0)
cc              MACPSW(inc) = 0
          endif
 1000 continue
c
c...Output trailing ')'
c
      call blkout (paren(2),1,0)
		PCHSPC = isav
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  oskout
c
c   FUNCTION:  This routine outputs OPSKIP programmed codes.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine oskout
c
      include 'post.inc'
c
      equivalence (IOPSKP,KPOSMP(0092)), (NOPSKP,KPOSMP(1077))
      equivalence (PCHBNC,KPOSMP(1109)), (PRTBNC,KPOSMP(1153))
      equivalence (IOPSON,KPOSMP(4051)), (NOPSON,KPOSMP(4050))
c
      integer*4 PRTBNC,PCHBNC,IOPSON(10),IOPSKP,NOPSKP,NOPSON
c
      equivalence (LOPSKP,CPOSMP(1648))
      equivalence (PRTBLK,CPOSMP(1653)), (PCHBLK,CPOSMP(3341))
c
      character*5 LOPSKP
      character*512 PCHBLK,PRTBLK
c
      integer*4 i,nc
      character*20 lbuf
c
c...Output active OPSKIP/ON codes
c
      do 125 i=1,NOPSON,1
         call itoc (IOPSON(i),lbuf,nc,0)
         PRTBLK(PRTBNC+1:) = LOPSKP(1:NOPSKP) // lbuf(1:nc)
         PCHBLK(PCHBNC+1:) = LOPSKP(1:NOPSKP) // lbuf(1:nc)
         PCHBNC = PCHBNC + nc + NOPSKP
         PRTBNC = PRTBNC + nc + NOPSKP
  125 continue
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pshblk (kblk)
c
c   FUNCTION:  This routine pushes a user defined block onto the stack,
c              which will be output in the 'clrbuf' subroutine.
c
c   INPUT:  kblk    I*4  D1   -  User defined block to output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pshblk (kblk)
c
      include 'post.inc'
c
      equivalence (USRSTK,KPOSMP(3049)), (NUSRST,KPOSMP(3059))
c
      integer*4 USRSTK(10),NUSRST
c
      integer*4 kblk
c
c...Push User defined block number
c...onto stack
c
      if (kblk .eq. 0 .or. NUSRST .ge. 10) go to 8000
      NUSRST = NUSRST + 1
      USRSTK(NUSRST) = kblk
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  sequnc (kfl,cdat,knc,kfrc)
c
c   FUNCTION:  This routine outputs sequence numbers and alignment block
c              codes.
c
c   INPUT:  kfl     I*4  D1   -  1 = Set up sequence number for output,
c                                but do not output it yet.  2 = Set up
c                                sequence number buffer but do not out-
c                                put it.
c
c   OUTPUT: cdat    C*n  D1   -  Returns sequence number buffer when
c                                kfl = 2.  The sequence buffer will also
c                                contain the Optional skip characters if
c                                OPSKIP/ON is in effect.
c
c           knc     I*4  D1   -  Number of characters in 'cdat'.
c
c           kfrc    I*4  D1   -  1 = Output an EOB with an alignment
c                                block.  Otherwise does not change the
c                                value.
c
c***********************************************************************
c
      subroutine sequnc (kfl,cdat,knc,kfrc)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IOPSKP,KPOSMP(0092)), (ICYCSW,KPOSMP(0271))
      equivalence (REGFRC,KPOSMP(0603)), (ISEQSW,KPOSMP(0845))
      equivalence (SEQFRQ,KPOSMP(0846)), (SEQCOD,KPOSMP(0847))
      equivalence (ALNCOD,KPOSMP(0848)), (AL1BLK,KPOSMP(0849))
      equivalence (AL2BLK,KPOSMP(0850)), (IHDID ,KPOSMP(0851))
      equivalence (ISEQBK,KPOSMP(0852)), (ISQBKV,KPOSMP(0853))
      equivalence (AL3BLK,KPOSMP(0854)), (ALNFLG,KPOSMP(0855))
      equivalence (MOTFLG,KPOSMP(1073)), (NOPSKP,KPOSMP(1077))
c
      integer*4 REGFRC(MAXFMT),ISEQSW,SEQFRQ,SEQCOD,ALNCOD,IHDID,ISEQBK,
     1          MOTFLG,AL1BLK,AL2BLK,IOPSKP,NOPSKP,ISQBKV,AL3BLK,
     2          ICYCSW(5),ALNFLG
c
      equivalence (ISEQBG,POSMAP(1163))
      equivalence (ISEQ  ,POSMAP(1164)), (SEQINC,POSMAP(1165))
      equivalence (ALNVAL,POSMAP(1166)), (SEQMAX,POSMAP(1167))
      equivalence (ISEQSV,POSMAP(1182)), (SEQCUR,POSMAP(1183))
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 ISEQ,SEQINC,ALNVAL,SEQMAX,ISEQSV,SEQCUR,RCYCDO(20),ISEQBG
c
      equivalence (LOPSKP,CPOSMP(1648))
c
      character*5 LOPSKP
c
      integer*4 kfl,kfrc,knc
c
      character*(*) cdat
c
      character*60 sbuf
c
      knc    = 0
      ISQBKV = ISEQBK
      ISEQSV = ISEQ
c
c...Alignment block
c
      if (IHDID .ne. 0) then
c
c......Set alignment code
c
          if (ALNFLG .eq. 1) then
              ALNVAL = ISEQ
              ISEQ   = ISEQ   + SEQINC
          else if (ALNFLG .eq. 3) then
              ALNVAL = RCYCDO(14)
              RCYCDO(14) = RCYCDO(14) + RCYCDO(15)
          endif
          if (ALNVAL .gt. SEQMAX) ALNVAL = SEQMAX
c
          if (kfl .eq. 2) then
              call fmtcod (ALNCOD,ALNVAL,cdat,knc)
c
          else
              if (ALNCOD .ne. 0) then
                  call setcod (ALNCOD,ALNVAL)
                  if (REGFRC(ALNCOD) .eq. 0) REGFRC(ALNCOD) = 1
              endif
          endif
          SEQCUR = ALNCOD
c
          if (ICYCSW(1) .eq. 1) then
              call frcblk (AL3BLK,kfrc,0)
          else if (MOTFLG .eq. 1) then
              call frcblk (AL2BLK,kfrc,0)
          else
              call frcblk (AL1BLK,kfrc,0)
          endif
          IHDID  = 0
c
c...Sequence number
c
      else
          if (ISEQSW .eq. 1 .and. SEQCOD .ne. 0) then
              ISEQBK = ISEQBK + 1
              if (ISEQBK .gt. SEQFRQ) then
                  if (ISEQ .gt. SEQMAX) ISEQ = ISEQBG
c
                  if (kfl .eq. 2) then
                      call setcod (SEQCOD,ISEQ)
                      call fmtcod (SEQCOD,ISEQ,cdat,knc)
                  else
                      call setcod (SEQCOD,ISEQ)
                      if (REGFRC(SEQCOD) .eq. 0) REGFRC(SEQCOD) = 4
                  endif
c
                  SEQCUR = ISEQ
                  ISEQ   = ISEQ   + SEQINC
                  ISEQBK = 1
              endif
          endif
      endif
c
c...OPSKIP/ON
c...Add optional skip character(s)
c
      if (kfl .eq. 2 .and. IOPSKP .eq. 1 .and. NOPSKP .ne. 0) then
          sbuf   = LOPSKP(1:NOPSKP) // cdat(1:knc)
          cdat   = sbuf
          knc    = knc    + NOPSKP
      endif
c
c...End of routine
c
 8000 return
      end
