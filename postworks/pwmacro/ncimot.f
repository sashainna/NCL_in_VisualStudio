c
c***********************************************************************
c
c   FILE NAME:  ncimot
c   CONTAINS:
c               ncimot  ncigto  ncicir  ncicyc  ncicym  ncicys  ncicyo
c               ncicyf  ncicfl  ncithp  ncithr
c
c     COPYRIGHT 2009 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        ncimot.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        03/10/16 , 10:22:31
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  ncimot (ktyp,kpos,kpt,kcont,gpt,cmsg,kerr)
c
c   FUNCTION:  This routine stores motion records in the clfile.
c
c   INPUT:  ktyp    I*4  D1  1 = 3-axis motion, 11 = 5-axis motion.
c
c           kpos    I*4  D1  Previous positioning mode.  Should be
c                            initialized to 0 prior to first call.  A
c                            value of -1 will flush the active motion
c                            record.
c
c           kpt     I*4  D1  Pointer into clfile record.  Should be
c                            initialized to 0 prior to first call.
c
c           kcont   I*4  D1  Motion type of current clfile record.
c                            Should be initialized to 5 prior to first
c                            call.
c
c   OUTPUT: kpos    I*4  D1  Updated positioning mode.
c
c           kpt     I*4  D1  Updated clfile record pointer.
c
c           kcont   I*4  D1  Motion type of current clfile record.
c
c           gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncimot (ktyp,kpos,kpt,kcont,gpt,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 ktyp,kpos,kpt,kcont,kerr
c
      real*8 gpt(6)
c
      character*(*) cmsg
c
      equivalence (ISN   ,KPOSMP(0001)), (ITYPE ,KPOSMP(0005))
      equivalence (MACHTP,KPOSMP(1201)), (ICUTDO,KPOSMP(3301))
c
      integer*4 ISN,ITYPE,ICUTDO(15),MACHTP
c
      equivalence (FEED  ,POSMAP(3547))
c
      real*8 FEED(4)
c
      integer*4 i,ifl(7),ipos,isav,ip(3),np,inum(4)
c
      real*8 rnum
c
      equivalence (rnum,inum)
c
c...Initialize routine
c
      inum(is4) = 0
      if (ktyp .eq. 0 .or. ktyp .eq. 1) then
          ip(1) = 2
          ip(2) = 5
          ip(3) = 6
          np    = 3
      else
          ip(1) = 1
          ip(2) = 7
          ip(3) = 9
          np    = 21
      endif
c
c...Get type of cut
c
      if (kpos .eq. -1) then
          ipos   = 1
      else if (kpos .ne. -1) then
          call ncicfl (RCSUB(ip(3)),ifl)
          ipos   = ifl(3)
          if (ipos .ge. 2) ipos = ipos - 2
          if (ifl(4) .ne. 0) then
              ipos = ifl(4) * 10
              if (ipos .ge. 20) ipos = ipos - 20
          endif
      endif
c
c...Determine if we should output stored motion record
c
      if ((ipos .ne. 0 .and. ipos .ne. kpos) .or.
     1    (RCSUB(ip(2)) .ne. -1 .and. kpos .ne. -1) .or.
     2    ktyp .ne. ITYPE .or. ifl(6) .ne. 0 .or. ifl(3) .ge. 2 .or.
     3    ifl(4) .ge. 2) then
          if (kpt .ne. 0) then
              isav   = INBUF(2)
              INBUF(2) = ISN
              MXC    = kpt
              call wrclrc (JBUF,kpt,ICR,ICPT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
              kpt    = 0
              INBUF(2) = isav
          endif
          kcont  = 5
      endif
      if (kpos .eq. -1) go to 8000
      ITYPE  = ktyp
      if (kpt .eq. 0) then
c
c...Output MULTAX if necessary
c
cc          if ((MULT .eq. 0 .and. ktyp .eq. 11) .or.
cc     1        (MULT .eq. 1 .and. ktyp .ne. 11)) then
          if (MULT .eq. 0 .and. MACHTP .ne. 2) then
              MULT   = 1
              if (ktyp .eq. 11) MULT = 1
              INBUF(3) = 9000
              INBUF(4) = MULT
              MXC    = 0
              call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...Check for coolant
c
          if (ifl(6) .ne. 0) then
              call ncicol (ifl(6)-1,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...Check for cutcom
c......3-axis
c
          if (ktyp .ne. 11 .and. RCSUB(1) .ne. 0) then
              i      = RCSUB(1)
              call ncicut (i,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
c......5-axis cutcom
c
          else if (ktyp .eq. 11 .and. (ifl(3) .ge. 2  .or.
     1             ifl(4) .ge. 2)) then
              if (ifl(4) .ge. 2) then
                  i      = ICUTDO(3)
              else
                  i      = 40
              endif
              call ncicut (i,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...Check for RAPID
c
          if ((MACHTP .ne. 2 .and. RCSUB(ip(2)) .eq. -2) .or.
     1        (MACHTP .eq. 2 .and. ktyp .eq. 0)) then
              INBUF(3) = 2000
              INBUF(4) = 5
              MXC    = 0
              call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
c
c...Lathe feed rate
c
          else if (MACHTP .eq. 2) then
              if (RCSUB(ip(2)) .ne. FEED(4)) then
                  FEED(4) = RCSUB(ip(2))
                  INBUF(3) = 2000
                  INBUF(4) = 1009
                  if (RCSUB(ip(2)) .lt. 0) then
                      inum(is1) = 74
                      RCSUB(ip(2)) = RCSUB(ip(2)) * (-1.)
                  else
                      inum(is1) = 73
                  endif
                  ROBUF(1) = rnum
                  ROBUF(2) = RCSUB(ip(2))
                  MXC    = 2
                  call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
                  if (kerr .ne. 0) go to 8000
              endif

c
c...Check for Mill feed rate change
c
          else if (RCSUB(ip(2)) .gt. 0.) then
              INBUF(3) = 2000
              INBUF(4) = 1009
              ROBUF(1) = RCSUB(ip(2))
              MXC    = 1
              call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
          endif
c
c...Setup record
c
          if (kcont .ne. 6) ISN = INBUF(2)
          INBUF(3) = 5000
          if (ktyp .eq. 11) INBUF(3) = 5200
          INBUF(4) = kcont
      endif
c
c...Store clfile data
c
      do 500 i=1,3,1
          ROBUF(kpt+i) = RCSUB(ip(1)+i-1)
          gpt(i) = ROBUF(kpt+i)
          if (ktyp .eq. 11) then
              ROBUF(kpt+i+3) = RCSUB(i+3) - RCSUB(i)
              gpt(i+3) = ROBUF(kpt+i+3)
              ROBUF(kpt+i+18) = RCSUB(i+9)
          endif
  500 continue
      if (ktyp .eq. 11) then
          call unitvc (gpt(4),gpt(4))
c
c...Zero out the other tool axis components
c   if one component tool axis vector equal to abs(1).
c
          call fxvec (gpt(4))
          call unitvc (gpt(4),ROBUF(kpt+4))
c
          do 510 i=7,18,1
              ROBUF(kpt+i) = 0.
  510     continue
      else if (MACHTP .ne. 2) then
          ROBUF(kpt+4) = 0.
          ROBUF(kpt+5) = 0.
          ROBUF(kpt+6) = 1.
          gpt(4) = 0.
          gpt(5) = 0.
          gpt(6) = 1.
      endif
      if (ktyp .eq. 11) then
          kpt    = kpt    + 21
      else if (MACHTP .eq. 2) then
          kpt    = kpt    + 3
      else
          kpt    = kpt    + 6
      endif
      kpos   = ipos
c
c...Output record if full
c
      if (kpt .gt. 240-np-1) then
          INBUF(2) = ISN
          MXC    = kpt
          call wrclrc (JBUF,kpt,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          kcont  = 6
          kpt    = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncigto (gpt,kmult,cmsg,kerr)
c
c   FUNCTION:  This routine stores a single point motion record in the clfile.
c
c   INPUT:  gpt     R*8  D6  Tool position and vector.
c
c           kmult   I*4  D1  -1 = Leave MULTAX setting as is, 0 = Turn
c                            MULTAX off, 1 = Turn MULTAX on.
c
c   OUTPUT: cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncigto (gpt,kmult,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kmult,kerr
c
      real*8 gpt(6)
c
      character*(*) cmsg
c
      integer*4 i
c
c...Output MULTAX if necessary
c
      if (kmult .ne. -1 .and. kmult .ne. MULT) then
          MULT   = kmult
          INBUF(3) = 9000
          INBUF(4) = MULT
          MXC    = 0
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Output motion record
c
      INBUF(3) = 5000
      INBUF(4) = 5
      MXC    = 3
      if (MULT .eq. 1) MXC = 6
      do 100 i=1,MXC,1
          ROBUF(i) = gpt(i)
  100 continue
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicir (kdir,gpt,cmsg,kerr)
c
c   FUNCTION:  This routine stores circular motion records in the clfile.
c
c   INPUT:  kdir    I*4  D1  2 = Clockwise circular, 3 = Counter-clockwise.
c
c           gpt     R*8  D1  Previous tool position and vector.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicir (kdir,gpt,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kdir,kerr
c
      character*(*) cmsg
c
      real*8 gpt(6)
c
      equivalence (MCHPLN,KPOSMP(1261))
c
      integer*4 MCHPLN
c
      integer*4 i,ifl(7),inum(4),jsn,ix1,ix2,ix3
c
      real*8 rnum,cir(7),ndist,ept(3),rdir
c
      equivalence (rnum,inum)
c
c...Initialize routine
c
      inum(is4) = 0
c
c...Get type of cut
c
      call ncicfl (RCSUB(9),ifl)
c
c...Check for coolant
c
      if (ifl(6) .ne. 0) then
          call ncicol (ifl(6)-1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Check for cutcom
c
      if (RCSUB(2) .ne. 0) then
          i      = RCSUB(2)
          call ncicut (i,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Check for RAPID
c
      if (RCSUB(8) .eq. -2) then
          INBUF(3) = 2000
          INBUF(4) = 5
          MXC    = 0
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
c
c...Check for feed rate change
c
      else if (RCSUB(8) .gt. 0.) then
          INBUF(3) = 2000
          INBUF(4) = 1009
          ROBUF(1) = RCSUB(8)
          MXC    = 1
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Set machining plane
c
      if (RCSUB(1) .eq. 0) then
          MCHPLN = 3
          ix1    = 1
          ix2    = 2
          ix3    = 3
      else if (RCSUB(1) .eq. 1) then
          MCHPLN = 1
          ix1    = 2
          ix2    = 3
          ix3    = 1
      else
          MCHPLN = 2
          ix1    = 1
          ix2    = 3
          ix3    = 2
      endif
c
c...Setup circle parameters
c
      cir(ix1) = RCSUB(5)
      cir(ix2) = RCSUB(6)
      cir(ix3) = RCSUB(7)
      cir(ix1+3) = 0.
      cir(ix2+3) = 0.
      cir(ix3+3) = 1.
      cir(7) = ndist(gpt,cir)
c
c...Setup end point
c
      ept(ix1) = RCSUB(3)
      ept(ix2) = RCSUB(4)
      ept(ix3) = RCSUB(7)
c
c...Determine if helical move
c
      if (gpt(ix3) .ne. ept(ix3)) then
c
c......Output COUPLE command
c
          INBUF(3) = 2000
          INBUF(4) = 1049
          ROBUF(1) = gpt(ix3) - ept(ix3)
          inum(is1) = 1
          ROBUF(2) = rnum
          ROBUF(3) = 0.
          MXC    = 3
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          ept(ix3) = gpt(ix3)
      endif
c
c...Store circle record
c
      jsn    = inbuf(2)
cc      call outcir (cir,jsn,ICR,ICPT,cmsg,kerr)
cc      if (kerr .ne. 0) go to 8000
c
c...Output circular points
c
      rdir = 1.
      if (kdir .eq. 2) rdir = -1
      call genci2 (cir,gpt,ept,rdir,MULT,jsn,ICR,ICPT,0,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Store final point
c
      gpt(ix1) = ept(ix1)
      gpt(ix2) = ept(ix2)
      gpt(ix3) = RCSUB(7)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicyc (gpt,cmsg,kerr)
c
c   FUNCTION:  This routine stores milling cycle records in the clfile.
c
c   INPUT:  gpt     R*8  D6  Previous tool position and vector.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicyc (gpt,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCSW(5),ICYCDO(15)
c
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20)
c
      integer*4 kerr
c
      real*8 gpt(6)
c
      character*(*) cmsg
c
      integer*2 BORE,BORE7,BORE8,BORE9,DEEP,DRILL,FACE,MILL,REAM,REVERS,
     1          SHIFT,TAP,THRU,ityp
      integer*4 ifl(7),inum(4),i
c
      real*8 rnum,dwel,rdep,ndist
c
      equivalence (rnum,inum)
c
      parameter (BORE   = 82)
      parameter (BORE7  = 211)
      parameter (BORE8  = 212)
      parameter (BORE9  = 213)
      parameter (DEEP   = 153)
      parameter (DRILL  = 163)
      parameter (FACE   = 81)
      parameter (MILL   = 151)
      parameter (REAM   = 262)
      parameter (REVERS = 1008)
      parameter (SHIFT  = 249)
      parameter (TAP    = 168)
      parameter (THRU   = 152)
c
c...Reverse top and bottom of hole
c
      if (RCSUB(19) .eq. 1) then
          do 100 i=1,3,1
              rnum   = RCSUB(i+1)
              RCSUB(i+1) = RCSUB(i+14)
              RCSUB(i+14) = rnum
  100     continue
      endif
c
c...Initialize routine
c
      inum(is4) = 0
      ICYCDO(1) = RCSUB(1)
      dwel   = RCSUB(5)
      ICYCSW(1) = 1
c
c...Get type of cut
c
      call ncicfl (RCSUB(18),ifl)
c
c...Check for coolant
c
      if (ifl(6) .ne. 0) then
          call ncicol (ifl(6)-1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Determine cycle type
c
      ityp   = DRILL
      if (ICYCDO(1) .eq. 0) then
          ityp   = DRILL
          if (dwel .ne. 0.) ityp = FACE
      else if (ICYCDO(1) .eq. 1) then
          ityp   = THRU
      else if (ICYCDO(1) .eq. 2) then
          ityp   = DEEP
      else if (ICYCDO(1) .eq. 3) then
          ityp   = TAP
      else if (ICYCDO(1) .eq. 4) then
          ityp   = REAM
          if (dwel .ne. 0.) ityp = BORE9
      else if (ICYCDO(1) .eq. 5) then
          ityp   = BORE
      else if (ICYCDO(1) .eq. 6) then
          ityp   = SHIFT
      else if (ICYCDO(1) .eq. 7) then
          ityp   = BORE8
      else if (ICYCDO(1) .eq. 8) then
          ityp   = BORE7
      else if (ICYCDO(1) .eq. 9) then
          ityp   = MILL
      else if (ICYCDO(1) .eq. 10) then
          ityp   = REVERS
      endif
c
c...Initialize clfile record
c
      INBUF(3) = 2000
      INBUF(4) = 1054
      inum(is1) = ityp
      ROBUF(1) = rnum
      MXC    = 1
c
c...FEDTO
c
      rdep   = ndist(RCSUB(2),RCSUB(15)) + RCSUB(13) - RCSUB(11)
      inum(is1) = 281
      ROBUF(MXC+1) = rnum
      ROBUF(MXC+2) = rdep
      MXC    = MXC    + 2
      RCYCDO(1) = rdep
      RCYCDO(13) = RCSUB(13)
c
c...IPM
c
      inum(is1) = 73
      ROBUF(MXC+1) = rnum
      ROBUF(MXC+2) = RCSUB(6)
      MXC    = MXC    + 2
      RCYCDO(3) = RCSUB(6)
c
c...RAPTO
c
      inum(is1) = 280
      ROBUF(MXC+1) = rnum
      ROBUF(MXC+2) = dabs(RCSUB(13)-RCSUB(12))
      RCYCDO(2) = ROBUF(MXC+2)
      MXC    = MXC    + 2
c
c...DWELL
c
      if (dwel .ne. 0.) then
          inum(is1) = 279
          ROBUF(MXC+1) = rnum
          ROBUF(MXC+2) = dwel
          MXC    = MXC    + 2
      endif
      RCYCDO(5) = dwel
c
c...STEP
c
      if (RCSUB(7) .ne. 0. .and. (ityp .eq. DEEP .or. ityp .eq. THRU))
     1        then
          inum(is1) = 92
          ROBUF(MXC+1) = rnum
          ROBUF(MXC+2) = RCSUB(7)
          MXC    = MXC    + 2
          if (RCSUB(8) .ne. 0.) then
              ROBUF(MXC+1) = RCSUB(8)
              MXC    = MXC    + 1
          endif
      endif
      RCYCDO(7) = RCSUB(7)
      RCYCDO(8) = RCSUB(8)
c
c...OFFSET
c
      if (RCSUB(14) .ne. 0. .and. (ityp .eq. BORE .or.
     1        ityp .eq. BORE7 .or. ityp .eq. BORE8 .or.
     2        ityp .eq. BORE9 .or. ityp .eq. SHIFT)) then
          inum(is1) = 705
          ROBUF(MXC+1) = rnum
          ROBUF(MXC+2) = RCSUB(14)
          MXC    = MXC    + 2
      endif
      RCYCDO(9) = RCSUB(14)
c
c...RTRCTO
c
      if (RCSUB(9) .ne. 0. .or. RCSUB(10) .ne. 0.) then
          inum(is1) = 295
          ROBUF(MXC+1) = rnum
          ROBUF(MXC+2) = RCSUB(11) - RCSUB(13)
          MXC    = MXC    + 2
          if (RCSUB(9) .ne. 0. .and. (ityp .eq. DEEP .or.
     1            ityp .eq. THRU)) then
              ROBUF(MXC+1) = RCSUB(9)
              MXC    = MXC    + 1
          endif
      endif
      RCYCDO(11) = RCSUB(9)
      RCYCDO(12) = RCSUB(10)
c
c...Output CYCLE command
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Output GOTO point
c
      call ncicyo (gpt,RCSUB(2),RCSUB(15),rdep,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicym (gpt,cmsg,kerr)
c
c   FUNCTION:  This routine processes the Canned Cycle Repeat record.
c
c   INPUT:  gpt     R*8  D6  Previous tool position and vector.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicym (gpt,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (ICYCSW,KPOSMP(0271))
c
      integer*4 ICYCSW(5)
c
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20)
c
      integer*4 kerr
c
      real*8 gpt(6)
c
      character*(*) cmsg
c
      integer*4 ifl(7),inum(4),i
c
      real*8 rnum,rdep,ndist
c
      equivalence (rnum,inum)
c
c...Reverse top and bottom of hole
c
      if (RCSUB(13) .eq. 1) then
          do 100 i=1,3,1
              rnum   = RCSUB(i+1)
              RCSUB(i+1) = RCSUB(i+8)
              RCSUB(i+8) = rnum
  100     continue
      endif
c
c...Calculate cycle depth
c
      rdep   = ndist(RCSUB(2),RCSUB(9)) + RCYCDO(13) - RCSUB(5)
c
c...Determine if an updated CYCLE command
c...has to be output
c
      if (dabs(rdep-RCYCDO(1)) .gt. .0001 .or.
     1    dabs(RCSUB(7)-RCYCDO(5)) .gt. .0001) then
          call ncicys (gpt,cmsg,kerr)
          go to 8000
      endif
c
c...Get type of cut
c
      call ncicfl (RCSUB(12),ifl)
c
c...Check for coolant
c
      if (ifl(6) .ne. 0) then
          call ncicol (ifl(6)-1,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Check for feed rate change
c
      if (RCSUB(8) .ne. RCYCDO(3)) then
          INBUF(3) = 2000
          INBUF(4) = 1009
          ROBUF(1) = RCSUB(8)
          MXC    = 1
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
          RCYCDO(3) = RCSUB(8)
      endif
c
c...Output GOTO point
c
      call ncicyo (gpt,RCSUB(2),RCSUB(9),rdep,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicys (gpt,cmsg,kerr)
c
c   FUNCTION:  This routine converts a Canned Cycle Repeat record to a
c              Start Drill Cycle record and processes it.
c
c   INPUT:  gpt     R*8  D6  Previous tool position and vector.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicys (gpt,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCSW(5),ICYCDO(15)
c
      equivalence (RCYCDO,POSMAP(2931))
c
      real*8 RCYCDO(20)
c
      integer*4 kerr
c
      real*8 gpt(6)
c
      character*(*) cmsg
c
      integer*4 i
c
      real*8 rval(20)
c
c...Setup Canned Cycle record
c
      rval(1) = ICYCDO(1)
      rval(2) = RCSUB(2)
      rval(3) = RCSUB(3)
      rval(4) = RCSUB(4)
      rval(5) = RCSUB(7)
      rval(6) = RCSUB(8)
      rval(7) = RCYCDO(7)
      rval(8) = RCYCDO(8)
      rval(9) = RCYCDO(11)
      rval(10) = RCYCDO(12)
      rval(11) = RCSUB(5)
      rval(12) = RCSUB(6)
      rval(13) = RCYCDO(13)
      rval(14) = RCYCDO(9)
      rval(15) = RCSUB(9)
      rval(16) = RCSUB(10)
      rval(17) = RCSUB(11)
      rval(18) = RCSUB(12)
      rval(19) = 0
c
      do 100 i=1,19,1
          RCSUB(i) = rval(i)
  100 continue
c
c...Output CYCLE command
c
      call ncicyc (gpt,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicyo (gpt,gbot,gtop,gdep,cmsg,kerr)
c
c   FUNCTION:  This routine calculates the tool position and axis for
c              a cycle point and outputs it.
c
c   INPUT:  gpt     R*8  D6  Previous tool position and vector.
c
c           gbot    R*8  D3  Position at bottom of hole.
c
c           gtop    R*8  D3  Initial top position of cycle.
c
c           gdep    R*8  D1  Depth of cut.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicyo (gpt,gbot,gtop,gdep,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      real*8 gpt(6),gbot(3),gtop(3),gdep
c
      character*(*) cmsg
c
      integer*4 imult
c
      real*8 tax(3),ndist
c
c...Calculate tool axis
c
      call copyn (gpt(4),tax,3)
      call vcmnvc (gtop,gbot,gpt(4))
      call unitvc (gpt(4),gpt(4))
      call fxvec (gpt(4))
c
c...Calculate top of part
c
      call vcplvc (gbot,gpt(4),gpt,gdep)
c
c...Determine if multax should be turned on
c
      imult  = -1
      if (ndist(tax,gpt) .ne. 0.) imult = 1
c
c...Output GOTO point
c
      call ncigto (gpt,imult,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicyf (cmsg,kerr)
c
c   FUNCTION:  This routine stores the CYCLE/OFF command in the clfile.
c
c   INPUT:  none
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncicyf (cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      integer*4 kerr
c
      character*(*) cmsg
c
      equivalence (ICYCSW,KPOSMP(0271))
c
      integer*4 ICYCSW(5)
c
      integer*4 inum(4)
c
      real*8 rnum
c
      equivalence (rnum,inum)
c
c...Initialize routine
c
      inum(is4) = 0
      ICYCSW(1) = 0
c
c...CYCLE/OFF
c
      INBUF(3) = 2000
      INBUF(4) = 1054
      inum(is1) = 72
      ROBUF(1) = rnum
      MXC    = 1
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncicfl (gval,kfl)
c
c   FUNCTION:  This routine parses the motion Control Flags parameter.
c
c   INPUT:  gval    R*8  D1  Control flags parameter from source file.
c
c   OUTPUT: kfl     I*4  D7  Control flag settings.  Refer to MasterCam
c                            documentation for a description.
c
c***********************************************************************
c
      subroutine ncicfl (gval,kfl)
c
      integer*4 kfl(7)
c
      real*8 gval
c
      integer*4 it1,i
c
c...Calculate individual flags
c...from input value
c
      it1    = gval
      do 100 i=1,7,1
          kfl(i) = it1 - (it1/10*10)
          it1    = it1 / 10
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncithp (kprm,gprm)
c
c   FUNCTION:  This routine parses the Lathe Parameters One record and
c              returns the appropriate values.
c
c   INPUT:  none.
c
c   OUTPUT: kprm    I*4  D3  Threading parameters as follows.
c                               1 = Repeat value.
c                               2 = Equal depth cuts.
c                               3 = Number of cuts.
c
c           gprm    R*8  D1  Clearance perpendicular to cuts.
c
c***********************************************************************
c
      subroutine ncithp (kprm,gprm)
c
      include 'compile.inc'
c
      integer*4 kprm(3)
c
      real*8 gprm
c
c...Return relevant parameters
c
      kprm(1) = RCSUB(1)
      kprm(2) = RCSUB(7)
      kprm(3) = RCSUB(8)
c
      gprm   = RCSUB(5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ncithr (gpt,kprm,gprm,cmsg,kerr)
c
c   FUNCTION:  This routine stores threading cycle records in the clfile.
c
c   INPUT:  gpt     R*8  D6  Previous tool position and vector.
c
c           kprm    I*4  D3  Threading parameters.
c
c           gprm    R*8  D1  Clearance perpendicular to cuts.
c
c   OUTPUT: gpt     R*8  D6  Current tool position and vector.
c
c           cmsg    C*n  D1  Text of error message.
c
c           kerr    I*4  D1  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine ncithr (gpt,kprm,gprm,cmsg,kerr)
c
      include 'clnrd.inc'
      include 'ncicom.inc'
      include 'post.inc'
      include 'compile.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ICYCDO,KPOSMP(0276))
c
      integer*4 ICYCSW(5),ICYCDO(15)
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kprm(5),kerr
c
      real*8 gpt(6),gprm
c
      character*(*) cmsg
c
      integer*2 THREAD,FEDTO,OFF,OFFSET,ON,REPEAT,STEP,TOOL,TPI,XCOORD,
     1          ZCOORD,AUTO
      integer*4 inum(4),i,lnum,minc
c
      real*8 rnum,fdep,farea,fstep,fnum,ft,sgn,sarea
c
      equivalence (rnum,inum)
c
      parameter (AUTO   = 88)
      parameter (FEDTO  = 281)
      parameter (OFF    = 72)
      parameter (OFFSET = 705)
      parameter (ON     = 71)
      parameter (REPEAT = 1083)
      parameter (STEP   = 92)
      parameter (THREAD = 1036)
      parameter (TOOL   = 617)
      parameter (TPI    = 143)
      parameter (XCOORD = 116)
      parameter (ZCOORD = 118)
c
c...Initialize routine
c
      inum(is4) = 0
      INBUF(3) = 2000
      INBUF(4) = 1054
      sgn    = 1.
      if (RCSUB(1) .lt. RCSUB(2)) sgn = -1.
c
c...Output manual cycle
c
      if (RCSUB(11) .eq. 0) then
          inum(is1) = AUTO
          ROBUF(1) = rnum
          inum(is1) = OFF
          ROBUF(2) = rnum
          MXC    = 2
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...Initialize clfile record
c
      inum(is1) = THREAD
      ROBUF(1) = rnum
      MXC    = 1
c
c...LEAD
c
      MXC    = MXC    + 1
      inum(is1) = TPI
      ROBUF(MXC) = rnum
      MXC    = MXC    + 1
      ROBUF(MXC) = RCSUB(5)
      if (RCSUB(5) .lt. 0.) ROBUF(MXC) = 1. / dabs(RCSUB(5))
c
c...OFFSET
c
      MXC    = MXC    + 1
      inum(is1) = OFFSET
      ROBUF(MXC) = rnum
      MXC    = MXC    + 1
      fdep    = RCSUB(1) - RCSUB(2)
      ROBUF(MXC) = fdep
c
c...ZCOORD
c
      MXC    = MXC    + 1
      inum(is1) = ZCOORD
      ROBUF(MXC) = rnum
      MXC    = MXC    + 1
      ROBUF(MXC) = RCSUB(4)
c
c...XCOORD
c
      MXC    = MXC    + 1
      inum(is1) = XCOORD
      ROBUF(MXC) = rnum
      MXC    = MXC    + 1
c
c......G92 longhand cycle
c
      if (RCSUB(11) .eq. 2) then
          minc   = MXC
          ROBUF(MXC) = RCSUB(1) - RCSUB(6)*sgn

c
c......Manual or automatic cycle
c
      else
          ROBUF(MXC) = RCSUB(2)
      endif
c
c...FEDTO
c
      if (RCSUB(12) .ne. RCSUB(2)) then
          MXC    = MXC    + 1
          inum(is1) = OFFSET
          ROBUF(MXC) = rnum
          MXC    = MXC    + 1
          ROBUF(MXC) = RCSUB(2) - RCSUB(12)
      endif
c
c...STEP
c
      if (RCSUB(11) .ne. 2) then
          MXC    = MXC    + 1
          inum(is1) = STEP
          ROBUF(MXC) = rnum
          MXC    = MXC    + 1
          ROBUF(MXC) = RCSUB(6)
c
c......Calculate step based on # of cuts
c
          if (kprm(2) .eq. 1) then
              if (RCSUB(7) .ne. 0. .and. RCSUB(7) .ne. RCSUB(6)) then
                  MXC    = MXC    + 1
                  ROBUF(MXC) = RCSUB(7)
              endif
c
c......Calculate step based on equal area
c
          else
              lnum   = kprm(3) - 1
              ft     = dtan(RCSUB(9))
              fnum   = RCSUB(6)**2 * ft
              farea  = fdep**2 * ft
              farea  = (farea-fnum) / lnum * (lnum-1) + fnum
              fstep  = dsqrt(farea/ft)
              MXC    = MXC    + 1
              ROBUF(MXC) = fdep*sgn - fstep
          endif
      endif
c
c...TOOL
c
      if (RCSUB(10) .ne. 0) then
          MXC    = MXC    + 1
          inum(is1) = TOOL
          ROBUF(MXC) = rnum
          MXC    = MXC    + 1
          ROBUF(MXC) = RCSUB(10) * RAD
          call dpoint (ROBUF(MXC),ROBUF(MXC),4)
      endif
c
c...REPEAT
c
      if (kprm(1) .gt. 1 .and. RCSUB(11) .ne. 2) then
          MXC    = MXC    + 1
          inum(is1) = REPEAT
          ROBUF(MXC) = rnum
          MXC    = MXC    + 1
          ROBUF(MXC) = kprm(1)
      endif
c
c...Output CYCLE command
c
      call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
      if (kerr .ne. 0) go to 8000
c
c...Output remaining CYCLE commands
c...For Long cycle G92 mode
c
      if (RCSUB(11) .eq. 2) then
          lnum   = kprm(3) - 1
c
c......Base depths from equal area
c
          if (kprm(2) .eq. 0) then
              ft     = dtan(RCSUB(9))
              sarea   = RCSUB(6)**2 * ft
              farea  = fdep**2 * ft - sarea
              fnum   = farea / lnum
c
c......Base depths from equal number of cuts
c
          else
              fnum   = RCSUB(1) - RCSUB(6)*sgn
              farea  = (fdep*sgn - RCSUB(6)) / lnum
          endif
          do 500 i=1,lnum,1
              if (kprm(2) .eq. 0) then
                  sarea  = sarea  + fnum
                  ROBUF(minc)  = RCSUB(1) - dsqrt(sarea/ft)*sgn
              else
                  ROBUF(minc)  = fnum   - farea*sgn*i
              endif
              call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
              if (kerr .ne. 0) go to 8000
  500     continue
      endif
c
c...Cancel manual cycle
c
      if (RCSUB(11) .eq. 0) then
          inum(is1) = AUTO
          ROBUF(1) = rnum
          inum(is1) = ON
          ROBUF(2) = rnum
          MXC    = 2
          call wrclrc (JBUF,MXC,ICR,ICPT,cmsg,kerr)
          if (kerr .ne. 0) go to 8000
      endif
c
c...End of routine
c
 8000 return
      end
c

c
c***********************************************************************
c
c   SUBROUTINE:  fxvec (gvec)
c
c   FUNCTION:  This routine truncates tool axis vectors that are close to
c              0,0,1 in nature.
c
c   INPUT:  gvec    R*8  D3  Vector to test.
c
c   OUTPUT: gvec    R*8  D3  Modified vector.
c
c***********************************************************************
c
      subroutine fxvec (gvec)
c
      real*8 gvec(3)
c
      if (dabs(gvec(1)) .gt. .99999999d0) then
          gvec(1) = gvec(1) / dabs(gvec(1))
          gvec(2) = 0.
          gvec(3) = 0.
      else if (dabs(gvec(2)) .gt. .99999999d0) then
          gvec(2) = gvec(2) / dabs(gvec(2))
          gvec(1) = 0.
          gvec(3) = 0.
      else if (dabs(gvec(3)) .gt. .99999999d0) then
          gvec(3) = gvec(3) / dabs(gvec(3))
          gvec(1) = 0.
          gvec(2) = 0.
      endif
c
c...End of routine
c
 8000 return
      end
