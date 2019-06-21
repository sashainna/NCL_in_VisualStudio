c
c***********************************************************************
c
c   FILE NAME:  posmsc
c   CONTAINS:
c               movpos  pnout  retpln  setpos  setend
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        posmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:10
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  movpos
c
c   FUNCTION:  This routine outputs a motion block containing the position
c              specified by 'AXSOUT' and is usually called by post
c              generated move routines (POSITN, RETRCT, etc).
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine movpos
c
      include 'post.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),ROTANG(20,2),
     1       ROTBAS(4)
c
      integer*4 iary(10),icnt
c
c...Calculate all levels of current position
c
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
      call whchax (AXSOUT,iary,icnt)
c
c...No axes move
c...Set current position
c
      if (icnt .eq. 0) then
          call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
c
c...Output motion block
c
      else
          call motion (iary,icnt)
          call clrmot (0)
      endif
c
c...Calculate new base angles
c
      call linrot (ROTANG,ROTBAS,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pnout (kmod,greg,gax,kfl)
c
c   FUNCTION:  This routine outputs a preset axis registers block
c              (POSTN).
c
c   INPUT:  kmod    I*4  D1  -  1 = Absolute preset axis registers, 2 =
c                               Incremental preset axis registers.
c
c           greg    R*8  D1  -  Controller register number that con-
c                               tains offset values.
c
c           gax     R*8  D10 -  Actual machine axis positions.
c
c           kfl     I*4  D1  -  1 = Output this axis in this block.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine pnout (kmod,greg,gax,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603)), (PSTNCD,KPOSMP(3376))
      equivalence (PSTNRG,KPOSMP(3381))
c
      integer*4 REGFRC(MAXFMT),PSTNCD(5),PSTNRG(15)
c
      equivalence (PSTNCV,POSMAP(0172)), (AXSSTO,POSMAP(1425))
c
      real*8 AXSSTO(10),PSTNCV(5)
c
      integer*4 kmod,kfl(10)
c
      real*8 greg,gax(10)
c
      integer*4 i,ierr
c
      character*80 lmsg
c
c...Output POSTN definition codes
c
      call codout (PSTNCD(kmod),PSTNCV(kmod))
      call codout (PSTNRG(12),greg)
c
c...Output axes positions
c
      do 100 i=1,10,1
          if (kfl(i) .eq. 1) then
              if (kmod .ne. 2) AXSSTO(i) = gax(i)
              if (PSTNRG(i) .ne. 0) REGFRC(PSTNRG(i)) = 1
              call codout (PSTNRG(i),gax(i))
          endif
  100 continue
c
c...Clear output buffer
c
      call clrbuf
c
c...Save current position
c
      if (kmod .ne. 2) then
          call setpos
          call rotbeg
      endif
c
c...Output Simulation record
c
      call simmot (5,lmsg,ierr)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  retpln (kmod,gaxs,kerr)
c
c   FUNCTION:  This routine calculates a retract position on the defined
c              clearance plane from the current location.
c
c   INPUT:  kmod    I*4  D2  -  (1) 1 = Retract pivot point to clearance
c                               plane.  2 = Retract tool tip. (2) 1 =
c                               Retract in X, 2 = Y, 3 = Z, 4 = Along
c                               tool axis.
c
c   OUTPUT: gaxs    R*8  D10 -  Calculated position on clearance plane.
c
c           kerr    I*4  D1  -  Returns 1 if the current pivot axis
c                               vector does not intersect the clearance
c                               plane.
c
c***********************************************************************
c
      subroutine retpln (kmod,gaxs,kerr)
c
      include 'post.inc'
c
      equivalence (IFHEAD,KPOSMP(1281)), (ITP   ,KPOSMP(1801))
c
      integer*4 ITP,IFHEAD
c
      equivalence (DUMMY ,POSMAP(0003)), (CLRPLN,POSMAP(0161))
      equivalence (STONUM,POSMAP(1387))
      equivalence (SPIVEC,POSMAP(3583)), (TL    ,POSMAP(3601))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 CLRPLN(4),STONUM(3,4),ROTSTO(20,2),TL(120),DUMMY,SPIVEC(3)
c
      integer*4 kerr,kmod(2)
c
      real*8 gaxs(10)
c
      real*8 tvec(3),rmch(3,4),rlin(6),rnum(3)
c
c...Calculate Retract tool axis vector
c
      kerr   = 0
      if (kmod(2) .lt. 1) kmod(2) = 4
c
c......Based on Axis
c
      if (kmod(2) .ne. 4) then
          tvec(1) = 0.
          tvec(2) = 0.
          tvec(3) = 0.
          tvec(kmod(2)) = 1.
c
c......Based on Pivot
c
      else
          call pivvec (ROTSTO,tvec)
      endif
c
c...Calculate Retract position
c...based on clearance plane
c
      if (CLRPLN(1) .ne. DUMMY) then
c
c......Use pivot point
c
          if (kmod(1) .eq. 1) then
              call plnint (STONUM(1,4),tvec,CLRPLN,rmch(1,4),kerr)
c
c......Use tool end point
c
          else
              if (IFHEAD .eq. 1) then
                  call pivadj (STONUM(1,3),rnum,ROTSTO,1)
              else
                  rnum(1) = STONUM(1,3) - TL(ITP) * SPIVEC(1)
                  rnum(2) = STONUM(2,3) - TL(ITP) * SPIVEC(2)
                  rnum(3) = STONUM(3,3) - TL(ITP) * SPIVEC(3)
              endif
              call plnint (rnum,tvec,CLRPLN,rnum,kerr)
              if (IFHEAD .eq. 1) then
                  call pivadj (rnum,rmch(1,3),ROTSTO,0)
              else
                  rmch(1,3) = rnum(1) + TL(ITP) * SPIVEC(1)
                  rmch(2,3) = rnum(2) + TL(ITP) * SPIVEC(2)
                  rmch(3,3) = rnum(3) + TL(ITP) * SPIVEC(3)
              endif
              call alladj (rmch,rlin,gaxs,ROTSTO,3,4)
          endif
c
c...Calculate Retract position
c...based on tool length
c
      else
          rmch(1,4) = STONUM(1,4) + (TL(ITP)+CLRPLN(2)) * tvec(1)
          rmch(2,4) = STONUM(2,4) + (TL(ITP)+CLRPLN(2)) * tvec(2)
          rmch(3,4) = STONUM(3,4) + (TL(ITP)+CLRPLN(2)) * tvec(3)
      endif
c
c...Adjust points to machine system
c
      call alladj (rmch,rlin,gaxs,ROTSTO,4,5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setpos
c
c   FUNCTION:  This routine calculates all intermediate axes positions
c              from the current output position.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine setpos
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381))
c
      integer*4 MOTREG(24)
c
      equivalence (CLSAV ,POSMAP(0201))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387)), (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425)), (ROTBAS,POSMAP(1435))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),VECSAV(3),STONUM(3,4),
     1       LINSTO(6),ROTANG(20,2),ROTSTO(20,2),AXSSTO(10),ROTBAS(4),
     2       TLVEC(3),CLSAV(21)
c
      integer*4 i,j,isub(6)
c
      data isub /1,3,5,7,9,11/
c
c...Set up initial axes position
c
      call alladr (AXSSTO,LINSTO,STONUM,ROTSTO,VECSAV,5,1)
      call linrot (ROTSTO,ROTBAS,1)
c
c...Store as current and previous position
c
      do 100 i=1,3,1
          CLSAV(i) = STONUM(i,2)
          CLSAV(i+3) = VECSAV(i)
  100 continue
      call cpyrot (ROTSTO,ROTANG)
      do 500 i=1,4,1
          do 300 j=1,3,1
              MCHNUM(j,i) = STONUM(j,i)
  300     continue
  500 continue
      do 600 i=1,10,1
          AXSOUT(i) = AXSSTO(i)
  600 continue
      TLVEC(1) = VECSAV(1)
      TLVEC(2) = VECSAV(2)
      TLVEC(3) = VECSAV(3)
c
c...Set output registers
c
      do 700 i=1,6,1
          LINAXS(i) = LINSTO(i)
          call setcod (MOTREG(isub(i)),AXSOUT(i))
  700 continue
c
      do 800 i=1,4,1
          call rotout (AXSOUT(i+6),i,2)
  800 continue
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setend (gpt)
c
c   FUNCTION:  This routine calculates all intermediate axes positions
c              from the current tool end point position.  These positions
c              are stored in the global arrays (MCHNUM, AXSOUT, etc.).
c
c   INPUT:  gpt     R*8  D6  -  Current tool end point position.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine setend (gpt)
c
      include 'post.inc'
c
      equivalence (FRMFLG,KPOSMP(1211))
c
      integer*4 FRMFLG
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),ROTANG(20,2),
     1       ROTBAS(4)
c
      real*8 gpt(6)
c
      integer*4 isav,ierr
c
c...Calculate all positions
c
      isav   = FRMFLG
      FRMFLG = -2
      call copyn (gpt,MCHNUM(1,2),3)
      call unitvc (gpt(4),TLVEC)
      call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,1,0,ierr)
c
c...Store positions
c
      call from (AXSOUT,ierr)
      call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
      FRMFLG = isav
c
c...End of routine
c
 8000 return
      end
