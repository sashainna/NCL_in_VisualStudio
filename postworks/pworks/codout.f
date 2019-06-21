c
c***********************************************************************
c
c   FILE NAME: codout.for
c   CONTAINS:
c               cmpcod  codint  codout  increg  ledout  msgout  rwstop
c               regtyp  setcod  getcod
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        codout.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/19/15 , 09:01:36
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cmpcod (kreg,gval,gtol,kchg,kfl)
c
c   FUNCTION:  This routine determines whether a register would be out-
c              put with a given value and within a certain tolerance.
c
c   INPUT:  kreg    I*4  D1  -  Register to check against, in the range
c                               of 0-MAXFMT.
c
c           gval    R*8  D1  -  Given value to compare against current
c                               register value.
c
c           gtol    R*8  D1  -  Tolerance to use when comparing values.
c
c           kchg    I*4  D1  -  1 = SET and CHANGED control should both
c                               be treated as CHANGED.
c
c   OUTPUT: kfl     I*4  D1  -  Returns 0 if the register would not be
c                               output and 1 if the register will be
c                               output.
c
c***********************************************************************
c
      subroutine cmpcod (kreg,gval,gtol,kchg,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (MOTFLG,KPOSMP(1073))
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 REGFRC(MAXFMT),MOTFLG
c
      equivalence (REGSTO,POSMAP(1032))
c
      real*8 REGSTO(MAXFMT)
c
      integer*4 kreg,kfl,kchg
c
      real*8 gval,gtol
c
      integer*4 inum,itol,isto
c
      real*8 rnum
c
c...Determine if this register
c...will be output
c
      kfl    = 0
      if (kreg .le. 0) go to 8000
      call codint (kreg,gtol,rnum,itol)
      if (itol .eq. 0) itol = 1
c
c......Get register value
c
      call codint (kreg,gval,rnum,inum)
      call codint (kreg,REGSTO(kreg),rnum,isto)
c
c......NOT USED
c
      if (FMTDES(10,kreg) .eq. 0) then
          go to 8000
c
c......ALWAYS
c
      else if (FMTDES(10,kreg) .eq. 1) then
          kfl    = 1
c
c......SET
c......CHANGED
c
      else if (FMTDES(10,kreg) .eq. 2 .or. FMTDES(10,kreg) .eq. 3) then
          if (abs(inum-isto) .ge. itol) kfl = 1
          if (FMTDES(10,kreg) .eq. 2 .and. kchg .ne. 1) kfl = 1
c
c......NON ZERO
c
      else if (FMTDES(10,kreg) .eq. 4) then
          if (abs(inum) .ge. itol) kfl = 1
c
c......MOTION
c
      else if (FMTDES(10,kreg) .eq. 5) then
          if (abs(inum-isto) .ge. itol .and. MOTFLG .eq. 1)
     1            kfl = 1
      endif
c
c......Force register output
c
      if (REGFRC(kreg) .eq. 3) then
          if (MOTFLG .eq. 1) kfl = 1
      else if (REGFRC(kreg) .gt. 0) then
          if (FMTDES(10,kreg) .ne. 5 .or. MOTFLG .eq. 1) kfl = 1
      else if (REGFRC(kreg) .lt. 0) then
          kfl = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  codint (kreg,gval,gout,kout)
c
c   FUNCTION:  This routine converts a real number to the both the real
c              and integer value it will have when it's stored in a
c              register.
c
c   INPUT:  kreg    I*4  D1  -  Register that the value will be stored
c                               in, in the range of 1-MAXFMT.
c
c           gval    R*8  D1  -  Value of register.
c
c   OUTPUT: gout    R*8  D1  -  Register value rounded off according to
c                               the register's predefined accuracy.
c
c           kout    I*4  D1  -  Integer value of 'gval', multiplied by
c                               the register's predefined accuracy.
c
c***********************************************************************
c
      subroutine codint (kreg,gval,gout,kout)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308)), (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MCHOPT(20)
c
      integer*4 kreg,kout
c
      real*8 gval,gout
c
      integer*4 iacy
c
      real*8 rnum
c
c...Calculate the register's actual value
c
      if (kreg .le. 0) then
          gout   = 0.
          kout   = 0
      else
          iacy   = FMTDES(5+MCHOPT(2),kreg)
          call dpoint (gval,gout,iacy)
          rnum   = dnint (gout*10.D0**iacy)
c         rnum   = gout   * 10.D0**iacy
          do while (rnum .gt. 2147483647.)
              rnum = rnum - 2147483647.
          enddo
          do while (rnum .lt. -2147483648.)
              rnum = rnum + 2147483648.
          enddo
          kout   = rnum
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  codout (kreg,gval)
c
c   FUNCTION:  This routine is used to set up a register for output.  It
c              sets the registers value and also sets a flag (REGSW)
c              that states that the register needs to be output.  The
c              register is actually output in the 'clrbuf' subroutine.
c
c   INPUT:  kreg    I*4  D1  -  Register to output.  A value of 0 spe-
c                               cifies no register.  A value in the
c                               range of 1-MAXFMT specifies the actual
c                               register.  -1 specifies a G-code and -2
c                               specifies an M-code.  This routine will
c                               which group the G or M code belongs to.
c                               -3 = End of Block.
c
c           gval    R*8  D1  -  Value to store in register.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine codout (kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSW ,KPOSMP(0405)), (REGFRC,KPOSMP(0603))
      equivalence (MULTGC,KPOSMP(0838)), (MULTMC,KPOSMP(0839))
      equivalence (MGCD  ,KPOSMP(0840)), (LSTREG,KPOSMP(0857))
      equivalence (MACPRG,KPOSMP(0858)), (MACPSW,KPOSMP(0859))
      equivalence (NOUTSW,KPOSMP(1074)), (FMTDES,KPOSMP(2133))
      equivalence (IREGVL,KPOSMP(3614))
c
      integer*2 FMTDES(10,MAXFMT)
      integer*4 MULTGC,MULTMC,MGCD,REGSW(MAXFMT),IREGVL(MAXFMT),NOUTSW,
     1          REGFRC(MAXFMT),LSTREG,MACPRG,MACPSW(30)
c
      equivalence (DUMMY ,POSMAP(0003)), (REGSTO,POSMAP(1032))
      equivalence (MACPRM,POSMAP(4300)), (REGVAL,POSMAP(5000))
c
      real*8 DUMMY,REGSTO(MAXFMT),REGVAL(MAXFMT),MACPRM(30)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 ireg,isw1,isw2,i,ifl
c
      real*8 rval
c
c...Get physical register number
c
      ireg   = kreg
      call regtyp (ireg,gval)
      if (ireg .eq. 0) go to 8000
c
c...G-code
c
      isw1   = 0
      isw2   = 0
      if (ireg .ge. 18 .and. ireg .le. 28) then
          if (MULTGC .eq. 2) isw1 = 1
          if (MGCD .eq. 2) isw2 = 1
c
c...M-code
c
      else if (ireg .ge. 37 .and. ireg .le. 47) then
          if (MULTMC .eq. 2) isw2 = 1
          if (MGCD .eq. 2) isw1 = 1
c
c...End-of-BLock
c
      else if (ireg .eq. -3) then
          call clrbuf
          go to 8000
c
c...Macro parameter
c
      else if (ireg .gt. 1000) then
          if (MACPRG .eq. -1) MACPRG = LSTREG
          MACPRM(ireg-1000) = gval
          MACPSW(ireg-1000) = 1
          go to 8000
      endif
c
c...Set register value
c
      LSTREG = ireg
      rval   = gval
      if (rval .eq. DUMMY) rval = REGSTO(ireg)
c
c...This register is currently
c...waiting to be output.
c...Clear the register arrays
c
      if (REGSW(ireg) .eq. 1) then
          call cmpcod (ireg,REGVAL(ireg),0.D0,1,REGSW(ireg))
          if (REGSW(ireg) .eq. 1) then
              call clrbuf
              go to 7000
          endif
      endif
c
      if (isw1 .eq. 1) then
          call cmpcod (ireg,rval,0.D0,1,ifl)
          if (ifl .eq. 0 .and. FMTDES(10,ireg) .ne. 2) go to 8000
          do 100 i=18,28,1
              if (REGSW(i) .eq. 1) then
                  if (FMTDES(10,ireg) .ne. 2)
     1                    call cmpcod (i,REGVAL(i),0.D0,1,REGSW(i))
                  if (REGSW(i) .eq. 1) then
                      call clrbuf
                      go to 7000
                  endif
              endif
  100     continue
      endif
c
      if (isw2 .eq. 1) then
          call cmpcod (ireg,rval,0.D0,1,ifl)
          if (ifl .eq. 0 .and. FMTDES(10,ireg) .ne. 2) go to 8000
          do 200 i=37,47,1
              if (REGSW(i) .eq. 1) then
                  if (FMTDES(10,ireg) .ne. 2)
     1                    call cmpcod (i,REGVAL(i),0.D0,1,REGSW(i))
                  if (REGSW(i) .eq. 1) then
                      call clrbuf
                      go to 7000
                  endif
              endif
  200     continue
      endif
c
c...Store this register
c...for output
c
 7000 call codint (ireg,rval,REGVAL(ireg),IREGVL(ireg))
      if (REGFRC(ireg) .ge. 0) then
          REGSW(ireg) = 1
          NOUTSW = NOUTSW + 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  increg (kreg,gval,gout)
c
c   FUNCTION:  This routine calculates the incremental difference be-
c              tween a given value and a value stored in a register.
c
c   INPUT:  kreg    I*4  D1  -  Register to use in calculation in the
c                               range 0-MAXFMT.
c
c           gval    R*8  D1  -  Given value to use in calculation.
c
c   OUTPUT: gout    R*8  D1  -  Incremental difference between 'gval'
c                               and value currently in register.
c
c***********************************************************************
c
      subroutine increg (kreg,gval,gout)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGSTO,POSMAP(1032))
c
      real*8 REGSTO(MAXFMT)
c
      integer*4 kreg
c
      real*8 gval,gout
c
      integer*4 inum
c
      real*8 rnum
c
c...Get difference between
c...input value and register value
c
      if (kreg .le. 0) then
          gout   = 0
      else
          call codint (kreg,gval,rnum,inum)
          gout   = rnum   - REGSTO(kreg)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ledout (gnum)
c
c   FUNCTION:  This routine outputs leader to the punch file and a mes-
c              sage to the print file.
c
c   INPUT:  gnum    R*8  D1  -  Amount of leader in current units to
c                               output.
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ledout (gnum)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308)), (NEOB  ,KPOSMP(0841))
c
      integer*4 MCHOPT(20),NEOB
c
      equivalence (LEOB  ,CPOSMP(0971)), (LEDCHR,CPOSMP(0986))
c
      character*1 LEDCHR
      character*5 LEOB
c
      real*8 gnum
c
      integer*4 nc,i,strlen1,ipt,nled
c
      real*8 rled
c
      character*512 abuf,sbuf
c
c...Build leader block
c
      if (gnum .eq. 0.) go to 8000
      do 100 i=1,MCHOPT(5),1
          sbuf(i:i) = LEDCHR
  100 continue
c
c...Clear output buffer
c
      call clrbuf
c
c...Output leader
c
      rled   = gnum
      if (MCHOPT(2) .eq. 2) rled = rled / 25.4
      nled   = rled   * 10
  200 if (nled .lt. MCHOPT(5)) go to 500
      call pakout (sbuf,MCHOPT(5),0)
      nled   = nled   - MCHOPT(5)
      go to 200
c
c...Add End-of-block character
c
  500 if (NEOB .eq. 0) then
          abuf   = sbuf(1:nled)
          nc     = nled
      else
          abuf   = sbuf(1:nled) // LEOB(1:NEOB)
          nc     = nled   + NEOB
      endif
      if (nc .ne. 0) call pakout (abuf,nc,0)
c
c...Output print file message
c
      if (MCHOPT(2) .eq. 1) then
          call getsap ('LEDINCH',ipt,IPRMDS,SALABL)
      else
          call getsap ('LEDMET',ipt,IPRMDS,SALABL)
      endif
      call rtoc (gnum,sbuf,nc)
      abuf   = SAPRM(ipt)
      call errstr (abuf,sbuf,1)
      nc     = strlen1(abuf)
      call prtout (abuf,nc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  msgout (cmsg,knc)
c
c   FUNCTION:  This routine outputs a message block to the punch file.
c
c   INPUT:  cmsg    C*n  D1  -  Text of message to output.
c
c           knc     I*4  D1  -  Number of characters in 'cmsg'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine msgout (cmsg,knc)
c
      include 'post.inc'
c
      equivalence (IOPSKP,KPOSMP(0092))
      equivalence (MSGSEQ,KPOSMP(0301)), (MSGEOB,KPOSMP(0302))
      equivalence (MSGTAB,KPOSMP(0303)), (MSGALN,KPOSMP(0349))
      equivalence (NEOB  ,KPOSMP(0841)), (NCBMSG,KPOSMP(0843))
      equivalence (NCEMSG,KPOSMP(0844)), (NOPSKP,KPOSMP(1077))
c
      integer*4 NEOB,NCBMSG,NCEMSG,MSGSEQ,MSGEOB,MSGTAB,IOPSKP,NOPSKP,
     1          MSGALN
c
      equivalence (LEOB  ,CPOSMP(0971)), (MSGST ,CPOSMP(0976))
      equivalence (TABCHR,CPOSMP(0987)), (MSGEN ,CPOSMP(1500))
      equivalence (LOPSKP,CPOSMP(1648))
c
      character*1 TABCHR
      character*5 LEOB,LOPSKP
      character*10 MSGST,MSGEN
c
      integer*4 knc
c
      character*(*) cmsg
c
      integer*4 nc,ifrc,inc,mnc
c
      character*512 abuf,sbuf
c
c...Initialize routine
c
      mnc    = knc
      if (mnc .lt. MSGALN) mnc = MSGALN
c
c...Generate sequence number
c
      call clrbuf
      if (MSGSEQ .eq. 1) then
          call sequnc (2,sbuf,nc,ifrc)
      else
          nc     = 0
          if (IOPSKP .eq. 1 .and. NOPSKP .ne. 0) then
              sbuf   = LOPSKP(1:NOPSKP)
              nc     = NOPSKP
          endif
      endif
c
c...Build message block
c
      if (NCBMSG .eq. 0) then
          abuf   = sbuf(1:nc) // cmsg(1:knc)
          nc     = nc     + mnc
      else
          abuf   = sbuf(1:nc) // MSGST(1:NCBMSG) // cmsg(1:knc)
          nc     = nc     + NCBMSG + mnc
      endif
c
      if (NCEMSG .eq. 0) then
          sbuf   = abuf
      else
          sbuf   = abuf(1:nc) // MSGEN(1:NCEMSG)
          nc     = nc      + NCEMSG
      endif
c
c...Convert spaces to tabs
c
      if (MSGTAB .eq. 1) then
  100     inc    = index(sbuf(1:nc),' ')
          if (inc .ne. 0) then
              sbuf(inc:inc) = TABCHR
              go to 100
          endif
      endif
c
c...Append End-of-Block
c
      if (MSGEOB .ne. 1 .or. NEOB .eq. 0) then
          abuf   = sbuf
          if (MSGEOB .ne. 1) nc = -nc
      else
          abuf   = sbuf(1:nc) // LEOB(1:NEOB)
          nc     = nc     + NEOB
      endif
c
c...Output message to punch file
c
      call pakout (abuf,nc,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  regtyp (kreg,gval)
c
c   FUNCTION:  This routine gets the actual register of a logical regis-
c              ter, depending on the current mode.  The following lo-
c              gical registers are supported.
c
c                   -1 = G-code.   -2 = M-code.  -3 = End-of-block.
c                   -4 = Primary X-axis.  -5 = Secondary X-axis.
c                   -6 = Primary Y-axis.  -7 = Secondary Y-axis.
c                   -8 = Primary Z-axis.  -9 = Secondary Z-axis.
c                   -10 = Rotary Axis #1.  -11 = Rotary axis #2.
c                   -12 = Rotary Axis #3.  -13 = Rotary axis #4.
c                   -14 = Feedrate.
c
c   INPUT:  kreg    I*4  D1  -  Logical register number.
c
c           gval    R*8  D1  -  Register value.
c
c   OUTPUT: kreg    I*4  D1  -  Physical register number.
c
c***********************************************************************
c
      subroutine regtyp (kreg,gval)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (INCR  ,KPOSMP(1226))
      equivalence (IFOTYP,KPOSMP(3151)), (FEDCD ,KPOSMP(3162))
c
      integer*4 MOTREG(24),INCR,IFOTYP,FEDCD(8)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 inc,isub(10)
c
      data isub /1,3,5,7,9,11,14,17,20,23/
c
c...G-code
c
      if (kreg .eq. -1) then
          call gcdgrp (kreg,gval)
c
c...M-code
c
      else if (kreg .eq. -2) then
          call mcdgrp (kreg,gval)
c
c...XUYVZWABC@
c
      else if (kreg .le. -4 .and. kreg .ge. -13) then
          inc    = -kreg - 3
          if (INCR .eq. 1) then
              kreg   = MOTREG(isub(inc))
          else
              kreg   = MOTREG(isub(inc)+1)
          endif
c
c...Feed rate
c
      else if (kreg .eq. -14) then
          kreg   = FEDCD(IFOTYP)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rwstop
c
c   FUNCTION:  This routine outputs a rewind stop code to the punch file
c              and a rewind stop message to the print file.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rwstop
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (NEOB  ,KPOSMP(0841)), (NRWSTP,KPOSMP(1104))
      equivalence (IRWEOB,KPOSMP(1135))
c
      integer*4 NEOB,NRWSTP,IRWEOB
c
      equivalence (LEOB  ,CPOSMP(0971)), (LRWSTP,CPOSMP(2201))
c
      character*5 LEOB
      character*10 LRWSTP
c
      integer*4 nc,ipt
c
      character*512 sbuf
c
c...Build rewind stop block
c
      if (NRWSTP .eq. 0) go to 8000
      if (NEOB .eq. 0 .or. IRWEOB .eq. 2) then
          sbuf   = LRWSTP
          nc     = NRWSTP
      else
          sbuf   = LRWSTP(1:NRWSTP) // LEOB(1:NEOB)
          nc     = NRWSTP + NEOB
      endif
c
c...Output rewind stop code &
c...Print file message
c
      call clrbuf
      call pakout (sbuf,nc,0)
      call getsap ('RWSTOP',ipt,IPRMDS,SALABL)
      sbuf   = SAPRM(ipt)(1:SAPNC(ipt)) // ' ' // LRWSTP(1:NRWSTP)
      nc     = SAPNC(ipt) + 1 + NRWSTP
      call prtout (sbuf,nc)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setcod (kreg,gval)
c
c   FUNCTION:  This routine is used to stores a register's value.  It
c              stores both the current value and previous value to the
c              same number.
c
c   INPUT:  kreg    I*4  D1  -  Register to define.
c
c           gval    R*8  D1  -  Value to store in register.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine setcod (kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IREGST,KPOSMP(0497)), (IREGVL,KPOSMP(3614))
c
      integer*4 IREGVL(MAXFMT),IREGST(MAXFMT)
c
      equivalence (REGSTO,POSMAP(1032)), (REGVAL,POSMAP(5000))
c
      real*8 REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 ireg
c
c...No register for this code
c
      if (kreg .eq. 0) go to 8000
      ireg   = kreg
c
c...Get physical register number
c
      call regtyp (ireg,gval)
      if (ireg .eq. 0) go to 8000
      if (ireg .eq. -3) go to 8000
c
c...Store the register's value
c
      call codint (ireg,gval,REGVAL(ireg),IREGVL(ireg))
      REGSTO(ireg) = REGVAL(ireg)
      IREGST(ireg) = IREGVL(ireg)
c
c...End of routine
c
 8000 return
      end



c
c***********************************************************************
c
c   SUBROUTINE:  getcod (kreg,gval)
c
c   FUNCTION:  This routine is used to report the value in a register.
c
c   INPUT:  kreg    I*4  D1  -  Register to define.
c
c   OUTPUT: gval    R*8  D1  -  Value stored in register.
c
c***********************************************************************
c
      subroutine getcod (kreg,gval)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IREGST,KPOSMP(0497)), (IREGVL,KPOSMP(3614))
c
      integer*4 IREGVL(MAXFMT),IREGST(MAXFMT)
c
      equivalence (REGSTO,POSMAP(1032)), (REGVAL,POSMAP(5000))
c
      real*8 REGSTO(MAXFMT),REGVAL(MAXFMT)
c
      integer*4 kreg
c
      real*8 gval
c
      integer*4 ireg
c
c...No register for this code
c
      if (kreg .eq. 0) go to 8000
      ireg   = kreg
c
c...Get physical register number
c
      call regtyp (ireg,gval)
      if (ireg .eq. 0) go to 8000
      if (ireg .eq. -3) go to 8000
c
c...Get the register's value
c
      gval = REGSTO(ireg)
c
c...End of routine
c
 8000 return
      end
