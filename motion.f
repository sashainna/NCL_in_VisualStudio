c
c***********************************************************************
c
c   FILE NAME:  motion
c   CONTAINS:
c               clrmot  frcmot  from    hldmot  motion  savmot  whchax
c               whchmv
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        motion.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 09:22:13
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  clrmot (kfl)
c
c   FUNCTION:  This routine outputs held back motion.
c
c   INPUT:  kfl     I*4  D1  -  0 = Just clear motion, do not push any
c                               thing on to the stack.
c
c                               1 = Motion is currently active.  Push
c                               current position on stack prior to
c                               clearing motion.
c
c                               2 = Post word is currently active. Push
c                               post command on to stack prior to
c                               clearing motion.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine clrmot (kfl)
c
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (ICLOUT,KPOSMP(0084)), (ISCIRC,KPOSMP(1238))
      equivalence (NEXCIR,KPOSMP(1275))
      equivalence (HLDFLG,KPOSMP(1622)), (HLDSW ,KPOSMP(1623))
      equivalence (HLDISN,KPOSMP(1625)), (HLDCLI,KPOSMP(1626))
      equivalence (HLDCLO,KPOSMP(1627))
      equivalence (HLDRPN,KPOSMP(3985)), (HLDFDN,KPOSMP(3994))
c
      integer*4 HLDFLG,HLDSW,ISN,ICLREC,ICLOUT,HLDISN,HLDCLI,HLDCLO,
     1          ISCIRC,NEXCIR,HLDRPN(8),HLDFDN(5)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (HLDMCH,POSMAP(2141)), (HLDLIN,POSMAP(2153))
      equivalence (HLDAXS,POSMAP(2167)), (HLDVEC,POSMAP(2177))
      equivalence (HLDRPR,POSMAP(4550)), (HLDFDR,POSMAP(4560))
      equivalence (ROTANG,POSMAP(5173)), (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),AXSOUT(10),HLDMCH(3,4),
     1       HLDLIN(6),HLDROT(20,2),HLDAXS(10),HLDVEC(3),TLVEC(3),
     2       HLDRPR(5),HLDFDR(6)
c
      integer*4 kfl
c
      integer*4 i,j,iax(10),inum,isav(3),nrp(8),nfd(5)
      real*8 rrp(2),rfd(5),tmptol(3)
c
c...Determine if motion is being held back
c
      if (HLDFLG .eq. 0) go to 8000
      HLDFLG = 0
c
      call hldrap (nrp,rrp)
      call hldfed (nfd,rfd)
c
c......Save current position
c
      if (kfl .eq. 1) then
          call pshaxs (AXSOUT,TLVEC)
          call copyn(TLVEC,tmptol(1),3)
c
c......Save post command
c
      else if (kfl .eq. 2) then
          call pshcmd
      endif
c
c......Reset motion positions
c
      call cpyrot (HLDROT,ROTANG)
      do 200 i=1,4,1
          do 100 j=1,3,1
              MCHNUM(j,i) = HLDMCH(j,i)
  100     continue
  200 continue
c
      do 300 i=1,6,1
          LINAXS(i) = HLDLIN(i)
  300 continue
c
      do 400 i=1,10,1
          iax(i) = 0
          AXSOUT(i) = HLDAXS(i)
  400 continue
      iax(HLDSW) = 1
c
      TLVEC(1) = HLDVEC(1)
      TLVEC(2) = HLDVEC(2)
      TLVEC(3) = HLDVEC(3)
c
      isav(1) = ISN
      isav(2) = ICLREC
      isav(3) = ICLOUT
      ISN    = HLDISN
      ICLREC = HLDCLI
      ICLOUT = HLDCLO
      NEXCIR = ISCIRC
      ISCIRC = 0
c
      call hlbrap (HLDRPN,HLDRPR)
      call hlbfed (HLDFDN,HLDFDR)
c
c......Output motion block
c
      call motion (iax,1)
c
c......Restore current position
c
      if (kfl .eq. 1) then
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iax,inum)
          call copyn(tmptol(1),TLVEC,3)
      else if (kfl .eq. 2) then
          call popcmd
      endif
c
      call hlbrap (nrp,rrp)
      call hlbfed (nfd,rfd)
      ISN    = isav(1)
      ICLREC = isav(2)
      ICLOUT = isav(3)
      ISCIRC = NEXCIR
      NEXCIR = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  frcmot (kwhen,kfl,kaxs)
c
c   FUNCTION:  This routine forces out a motion block or forces certain
c              axes out on the next motion block.
c
c   INPUT:  kwhen   I*4  D1  -  When to force output of axes positions.
c                               1 = Immediately.  2 = On next motion
c                               block.
c
c           kfl     I*4  D1  -  Which axis positions to force.  1 =
c                               force all positions.  2 = Force active
c                               positions.  3 = Force selected posi-
c                               tions.  4 = Force selected XYZ positions
c                               (this routine will determine which of
c                               the linear axis are active and uses only
c                               'kaxs(1:3)').
c
c           kaxs    I*4  D10 -  Determines which axes to output when
c                               'kfl' = 3.  A value of 1 forces this
c                                axis.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine frcmot (kwhen,kfl,kaxs)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (REGFRC,KPOSMP(0603))
      equivalence (IRTOUT,KPOSMP(0813)), (IJKREG,KPOSMP(0827))
      equivalence (NUMLIN,KPOSMP(1202))
      equivalence (ACTLIN,KPOSMP(1205)), (INCR  ,KPOSMP(1226))
      equivalence (IRTACT,KPOSMP(1256)), (NROT  ,KPOSMP(1366))
      equivalence (IJKROT,KPOSMP(1739))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 REGFRC(MAXFMT),NUMLIN(3),ACTLIN(3),INCR,IRTACT(2),NROT,
     1          MOTREG(24),IRTOUT(4),MTPDYN,IJKREG(3),IJKROT
c
      equivalence (AXSSTO,POSMAP(1425))
c
      real*8 AXSSTO(10)
c
      integer*4 kwhen,kfl,kaxs(10)
c
      integer*4 i,inc,icnt,iax(10),ix
c
c...Set up positions that will
c...be forced out
c
      do 100 i=1,10,1
          iax(i) = 0
  100 continue
c
c......All positions
c
      if (kfl .eq. 1) then
          inc    = 1
          do 200 i=1,6,2
              if (NUMLIN(inc) .ge. 1) iax(i) = 1
              if (NUMLIN(inc) .ge. 2) iax(i+1) = 1
              inc    = inc    + 1
  200     continue
c
          if (IJKROT .eq. 1) then
              iax(7) = 1
              iax(8) = 1
              iax(9) = 1
          else
              do 250 i=1,4,1
                  if (IRTOUT(i) .eq. 1) iax(i+6) = 1
  250         continue
          endif
c
c......Active positions
c
      else if (kfl .eq. 2) then
          inc    = 1
          do 300 i=1,6,2
              if (NUMLIN(inc) .ne. 0) then
                  if (ACTLIN(inc) .eq. 1) then
                      iax(i) = 1
                  else if (ACTLIN(inc) .eq. 2) then
                      iax(i+1) = 1
                  endif
              endif
              inc    = inc    + 1
  300     continue
c
          if (IJKROT .eq. 1) then
              iax(7) = 1
              iax(8) = 1
              iax(9) = 1
          else
              if (NROT .ge. 1) iax(6+IRTACT(1)) = 1
              if (NROT .eq. 2) iax(6+IRTACT(2)) = 1
              if (MTPDYN .eq. 3) iax(10) = 1
          endif
c
c......Selected positions
c
      else if (kfl .eq. 3) then
          do 400 i=1,10,1
              iax(i) = kaxs(i)
  400     continue
c
c......Selected XYZ positions
c
      else
          inc    = 1
          do 450 i=1,6,2
              if (NUMLIN(inc) .ne. 0 .and. kaxs(inc) .eq. 1) then
                  if (ACTLIN(inc) .eq. 1) then
                      iax(i) = 1
                  else if (ACTLIN(inc) .eq. 2) then
                      iax(i+1) = 1
                  endif
              endif
              inc    = inc    + 1
  450     continue
      endif
c
c...Force output of axis
c
      icnt   = 0
      inc    = 0
      ix     = 1
      if (INCR .eq. 1) ix = 0
      do 500 i=1,12,2
          inc    = inc    + 1
          if (iax(inc) .eq. 1) then
              if (REGFRC(MOTREG(i+ix)) .eq. 0)
     -            REGFRC(MOTREG(i+ix)) = 3
              icnt   = icnt   + 1
          endif
  500 continue
c
      if (IJKROT .eq. 1) then
          do 5200 i=1,3,1
              if (iax(i+6) .eq. 1) then
                  if (REGFRC(IJKREG(i)) .eq. 0) REGFRC(IJKREG(i)) = 3
                  icnt   = icnt   + 1
               endif
 5200     continue
c
      else
          do 550 i=14,24,3
              inc    = inc    + 1
              if (iax(inc) .eq. 1) then
                  if (REGFRC(MOTREG(i+ix)) .eq. 0)
     -                REGFRC(MOTREG(i+ix)) = 3
                  icnt   = icnt   + 1
              endif
  550     continue
      endif
c
c......Output motion block now
c
      if (kwhen .eq. 1) call motout (AXSSTO,iax,icnt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  from (glin,kfl)
c
c   FUNCTION:  This routine handles the FROM statement.
c
c   INPUT:  gmch    R*8  D10 - Current machine axes position.
c
c   OUTPUT: kfl     I*4  D1  - 0 = Process FROM the same as a GOTO com-
c                              mand.  1 = Ignore FROM statement, save
c                              current position only.
c
c***********************************************************************
c
      subroutine from (gmch,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (NUMLIN,KPOSMP(1202))
      equivalence (FRMFLG,KPOSMP(1211))
      equivalence (MACHTP,KPOSMP(1201)), (HLDFLG,KPOSMP(1622))
      equivalence (IPSTNF,KPOSMP(3397)), (IRTOUT,KPOSMP(0813))
      equivalence (IJKROT,KPOSMP(1739)), (FMTDES,KPOSMP(2133))
c
      integer*4 FRMFLG,MOTREG(24),NUMLIN(3),IPSTNF,IRTOUT(4)
      integer*4 HLDFLG,MACHTP,IJKROT
      integer*2 FMTDES(10,MAXFMT)
c
      integer*4 kfl,nst
c
      real*8 gmch(10)
c
      integer*4 i,iaxs(10),inc,irsav(4),ierr
c
      character*80 lmsg
c
c
c...Issue POSTN statement
c
      if (IPSTNF .eq. 1) then
          if (HLDFLG .eq. 1) call clrmot (1)
          do 100 i=1,10,1
              iaxs(i) = 0
  100     continue
          inc    = 1
          do 150 i=1,3,1
              if (NUMLIN(i) .ge. 1) iaxs(inc) = 1
              if (NUMLIN(i) .eq. 2) iaxs(inc+1) = 1
              inc    = inc    + 2
  150     continue
          do 180 i=1,4,1
              if (IRTOUT(i) .eq. 1) iaxs(i+6) = 1
  180     continue
          call pnout (1,0,gmch,iaxs)
          kfl    = 1
          IPSTNF = 2
c
c...FROM forces output of all axes
c
      else if (FRMFLG .eq. 1) then
          if (HLDFLG .eq. 1) call clrmot (1)
          kfl    = 1
          call frcmot (2,2,iaxs)
          call simmot (1,lmsg,ierr)
c
c...FROM sets current value for all axes
c
      else if (FRMFLG .eq. 2 .or. FRMFLG .eq. -2) then
          if (HLDFLG .eq. 1) call clrmot (1)
          kfl    = 1
          inc    = 0
          do 500 i=1,12,2
              inc    = inc    + 1
              call setcod (MOTREG(i),gmch(inc))
  500     continue
c
          nst    = 1
          if (IJKROT .eq. 1) then
             call ijkout (gmch(inc),2)
             nst = 4
             inc = 9
             if (MACHTP .ne. 3) nst = 5
          end if
          do 550 i=nst,4,1
             inc    = inc    + 1
             irsav(i) = FMTDES(3,MOTREG(11+3*i))
             call rotout (gmch(inc),i,2)
             FMTDES(3,MOTREG(11+3*i)) = irsav(i)
  550     continue
          if (frmflg .ne. -2) call simmot (1,lmsg,ierr)
c
c...FROM is treated the same as GOTO
c
      else
          kfl    = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  hldmot (kax)
c
c   FUNCTION:  This routine saves the current position when only 1 axis
c              moves and is being held back.  It uses the global axes
c              position arrays.
c
c   INPUT:  kax     I*4  D6  -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine hldmot (kax)
c
      include 'post.inc'
c
      equivalence (ISN   ,KPOSMP(0001)), (ICLREC,KPOSMP(0002))
      equivalence (NPT   ,KPOSMP(0059))
      equivalence (ICLOUT,KPOSMP(0084)), (MOTREG,KPOSMP(0381))
      equivalence (HLDFLG,KPOSMP(1622)), (HLDSW ,KPOSMP(1623))
      equivalence (HLDDIR,KPOSMP(1624))
      equivalence (HLDISN,KPOSMP(1625)), (HLDCLI,KPOSMP(1626))
      equivalence (HLDCLO,KPOSMP(1627))
      equivalence (HLDRPN,KPOSMP(3985)), (HLDFDN,KPOSMP(3994))
c
      integer*4 MOTREG(24),HLDFLG,HLDSW,ISN,ICLREC,ICLOUT,HLDISN,HLDCLI,
     1          HLDCLO,HLDDIR,HLDRPN(8),HLDFDN(5),NPT
c
      equivalence (CLSAV ,POSMAP(0201))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (HLDMCH,POSMAP(2141)), (HLDLIN,POSMAP(2153))
      equivalence (HLDAXS,POSMAP(2167)), (HLDVEC,POSMAP(2177))
      equivalence (HLDFDR,POSMAP(4560)), (HLDRPR,POSMAP(4550))
      equivalence (ROTANG,POSMAP(5173)), (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),LINAXS(6),ROTANG(20,2),AXSOUT(10),HLDMCH(3,4),
     1       HLDLIN(6),HLDROT(20,2),HLDAXS(10),TLVEC(3),HLDVEC(3),
     2       HLDRPR(5),HLDFDR(5),CLSAV(21)
c
      integer*4 kax(10)
c
      integer*4 i,j,idir,isub(10),isw
c
      real*8 dir
c
      data isub /1,3,5,7,9,11,13,16,19,22/
c
c...Calculate direction of current axis
c
      do 100 i=1,10,1
          if (kax(i) .eq. 1) then
              isw    = i
              if (HLDFLG .eq. 1) then
                  dir    = AXSOUT(i) - HLDAXS(i)
              else
                  call increg (MOTREG(isub(i)),AXSOUT(i),dir)
              endif
              if (dir .ne. 0.) dir = dir / dabs(dir)
              idir   = dir
              go to 200
           endif
  100 continue
c
c...Axis is currently held back
c...Check to see if we need to output
c...it now
c
  200 if (HLDFLG .eq. 1) then
c
c......Same singular axis moves
c......Make sure it doesn't change direction
c
          if (isw .eq. HLDSW) then
              if (idir .ne. 0 .and. HLDDIR .ne. 0 .and.
     1            idir .ne. HLDDIR) call clrmot (1)
c
c......Different singular axis moves
c......Clear held back motion
c
          else
              call clrmot (1)
          endif
      endif
c
c...Save current position
c
      HLDSW  = isw
      call cpyrot (ROTANG,HLDROT)
      do 600 i=1,4,1
          do 500 j=1,3,1
              HLDMCH(j,i) = MCHNUM(j,i)
  500     continue
  600 continue
c
      do 700 i=1,6,1
          HLDLIN(i) = LINAXS(i)
  700 continue
c
      do 800 i=1,10,1
          HLDAXS(i) = AXSOUT(i)
  800 continue
c
      HLDVEC(1) = TLVEC(1)
      HLDVEC(2) = TLVEC(2)
      HLDVEC(3) = TLVEC(3)
c     call copyn (CLSAV,HLDCLS,NPT)
c
      call hldrap (HLDRPN,HLDRPR)
      call hldfed (HLDFDN,HLDFDR)
c
c...Set held back flag
c
      if (HLDFLG .eq. 0 .or. idir .ne. 0) HLDDIR = idir
      HLDFLG = 1
      HLDISN = ISN
      HLDCLI = ICLREC
      HLDCLO = ICLOUT
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  motion (kfl,kcnt)
c
c   FUNCTION:  This routine processes a single motion block for output.
c              All axes positions must be set up in the global arrays.
c
c   INPUT:  kfl     I*4  D10  -  1 = This axis is waiting to be output.
c
c           kcnt    I*4  D1   -  Number of linear axes waiting to be
c                                output.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine motion (kfl,kcnt)
c
      include 'post.inc'
c
      equivalence (SIMACT,KPOSMP(0174))
      equivalence (ICYCSW,KPOSMP(0271)), (IHDID ,KPOSMP(0851))
      equivalence (XFMFL ,KPOSMP(0969))
      equivalence (INCR  ,KPOSMP(1226)), (LTHDIA,KPOSMP(1228))
      equivalence (ISCIRC,KPOSMP(1238)), (NEXCIR,KPOSMP(1275))
      equivalence (IROTPS,KPOSMP(1350)), (ICRFMT,KPOSMP(1368))
      equivalence (IRTDEF,KPOSMP(1485)), (ISLWDN,KPOSMP(1721))
      equivalence (IRPMOD,KPOSMP(3198)), (IRAP  ,KPOSMP(3199))
      equivalence (POSFED,KPOSMP(3209)), (IRPSMD,KPOSMP(3242))
      equivalence (ICUTDO,KPOSMP(3301)), (NUMEXC,KPOSMP(3517))
      equivalence (LTMODE,KPOSMP(4125))

c
      integer*4 LTHDIA(2),IROTPS,IRAP,IRPMOD,INCR,ISCIRC,ICRFMT,
     1          ICUTDO(15),ISLWDN,ICYCSW(5),NUMEXC,POSFED,IHDID,
     2          LTMODE,NEXCIR,SIMACT,IRPSMD,XFMFL(20),IRTDEF
c
      equivalence (CUTTER,POSMAP(0744)), (MCHNUM,POSMAP(1287))
      equivalence (LINAXS,POSMAP(1299)), (AXSOUT,POSMAP(1340))
      equivalence (TLVEC ,POSMAP(1369)), (ROTANG,POSMAP(5173))
c
      real*8 CUTTER(7),AXSOUT(10),MCHNUM(3,4),LINAXS(6),TLVEC(3),
     1       ROTANG(20,2)
c
      integer*4 kfl(10),kcnt
c
      integer*4 inum,iary(10),iaxfl(10),icnt,i,isav,iesw(10),ieary(12),
     1          iept,ielft,ipsav,ihsav,ierr
c
c...Clear output axes control flags
c
      ipsav  = POSFED
      icnt   = kcnt
      do 100 i=1,10,1
          iaxfl(i) = 0
  100 continue
c
c...Bypass certain routines
c...with circular interpolation
c
      if (ISCIRC .eq. 1) then
          if (ICUTDO(2) .eq. 1) call cutctl
          go to 2000
      endif
c
c...Bypass certain routines
c...with threadcutting
c
      if (ICYCSW(1) .eq. 4) go to 2000
c
c...Check for rotary axes change
c...with transformation block active
c
      if (XFMFL(4) .gt. 1 .and. XFMFL(8) .ne. 1) then
          ierr   = 0
          do 200 i=1,IRTDEF,1
              if (kfl(i+6) .eq. 1) ierr = 1
  200     continue
          if (ierr .eq. 1) call psterr (1,'ROTXFM',' ',-1)
      endif
c
c...Multiple motion block
c...generating routines
c......Output Rapid motion
c
  300 if (IRAP .ne. 0) call rapmot (kfl,icnt,iaxfl(1))
c
c......Check for positioning rotary axis output
c
  500 if (IROTPS .eq. 1) call rposck (kfl,icnt,iaxfl(2))
c
c......Check for mutually exclusive axes
c
  800 if (NUMEXC .ne. 0) call excaxs (kfl,icnt,iaxfl(3),iesw,ieary,
     1                                ielft,iept)
c
c......Check for maximum number
c......of axes in a motion block
c
 1000 call maxaxc (kfl,icnt,iaxfl(4))
c
c......Check for axis departure
c......too large
c
 1500 call maxdep (kfl,icnt,iaxfl(5))
      if (icnt .eq. 0) go to 7000
c
c......Cutcom
c
      if (ICUTDO(2) .eq. 1) call cutctl
c
c......Slowdown span
c
 1800 if (ISLWDN .ne. 2 .and. ISLWDN .ne. 5)
     1        call mchslw (kfl,icnt,iaxfl(6))
c
c......Lathe Mill in LMFP mode (crossing zero)
c
 1900 if (LTMODE .eq. 1) call thrzer (kfl,icnt,iaxfl(7))
c
c......Acceleration span
c
 1950 call maccel (kfl,icnt,iaxfl(8))
c
c......Simulate rapid positioning moves
c
 1980 if (IRAP .eq. 1 .and. IRPSMD .eq. 1 .and. SIMACT .eq. 1)
     1    call rapsim (kfl,icnt,iaxfl(9))
c
c...Check for
c...Polar coordinate linear move
c
      if (ICRFMT .eq. 7) call cirpol
c
c...Calculate current RPM/SFM
c
 2000 call sfmctl (AXSOUT(1),CUTTER(1),LTHDIA(2))
c
c...Calculate current feed rates
c
      call fedctl (kfl)
c
c...Calculate delta movement &
c...machining time
c
      i = 1
c
c.....  when called from cirout, update global deltas only at the end
c
      if (NEXCIR.eq.2) i = 0
      call mchdis (i)
      call mchtim (AXSOUT,kfl,icnt,1)
c
c...Check machine limits
c
      call lmtchk (AXSOUT,inum,iary,1)
c
c...Set ABS/INCR mode
c
      isav   = INCR
      if (IRAP .eq. 1 .or. IRAP .eq. 2) then
          if (IRPMOD .eq. 2) INCR = 1
          if (IRPMOD .eq. 3) INCR = 2
      endif
c
c......Do not output TMARK
c......until final move for this CL point
c
      ihsav  = 0
      if ((iaxfl(2) .ne. 0 .or. iaxfl(3) .ne. 0 .or.
     1     iaxfl(4) .ne. 0 .or. iaxfl(5) .ne. 0 .or.
     2     iaxfl(6) .ne. 0) .and. IHDID .ne. 0) then
          ihsav  = IHDID
          IHDID  = 0
      endif
c
c...Output motion block
c
      call motout (AXSOUT,kfl,icnt)
      if (ihsav .ne. 0) IHDID = isav
c
c...Reset ABS/INCR mode &
c...Initial CUTCOM switch
c
      INCR   = isav
      ICUTDO(1) = 0
c
c...Save previous positions
c
      call savmot (MCHNUM,LINAXS,TLVEC,ROTANG,AXSOUT)
c
c...Check if output axes routine
c...has control of motion
c
 7000 POSFED = ipsav
      if (iaxfl(9) .eq. 1) go to 1980
      if (iaxfl(8) .eq. 1) go to 1950
      if (iaxfl(7) .eq. 1) go to 1900
      if (iaxfl(6) .eq. 1) go to 1800
      if (iaxfl(5) .eq. 1) go to 1500
      if (iaxfl(4) .eq. 1) go to 1000
      if (iaxfl(3) .eq. 1) go to 800
      if (iaxfl(2) .eq. 1) go to 500
      if (iaxfl(1) .ne. 0) go to 300
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  savmot (gmch,glin,gvec,grot,gaxs)
c
c   FUNCTION:  This routine saves the current position in the global
c              previous position arrays.
c
c   INPUT:  gmch    R*8  D3.4 - (1) = Input cl points.  (2) = TRANS ad-
c                               justed points.  (3) = Rotary adjusted
c                               points.  (4) = TRANS/LAST adjusted
c                               points.
c
c           glin    R*8  D6   - Current machine slide positions for pri-
c                               mary and secondary linear XYZ axes.
c
c           gvec    R*8  D3   - Current tool axis vector.
c
c           grot    R*8  D4.2 - (1) = Angles on rotary scale.  (2) =
c                               Angles on linear scale.
c
c           gaxs    R*8  D10  - Output positions for all axes.
c
c   Output: none.
c
c***********************************************************************
c
      subroutine savmot (gmch,glin,gvec,grot,gaxs)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (CLSAV ,POSMAP(0201)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387)), (LINSTO,POSMAP(1399))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (PRESTO,POSMAP(2280)), (ROTSTO,POSMAP(5213))
c
      real*8 VECSAV(3),STONUM(3,4),LINSTO(6),ROTSTO(20,2),AXSSTO(10),
     1       CLSAV(21),PRESTO(6)
c
      real*8 gmch(3,4),glin(6),gvec(3),grot(20,2),gaxs(10)
c
      integer*4 i
c
c...Save current position
c
      do 100 i=1,3,1
          CLSAV(i) = gmch(i,2)
          CLSAV(i+3) = gvec(i)
          PRESTO(i) = STONUM(i,2)
          PRESTO(i+3) = VECSAV(i)
          VECSAV(i) = gvec(i)
  100 continue
c
      call cpyrot (grot,ROTSTO)
      do 200 i=1,4,1
          STONUM(1,i) = gmch(1,i)
          STONUM(2,i) = gmch(2,i)
          STONUM(3,i) = gmch(3,i)
  200 continue
c
      do 300 i=1,6,1
          LINSTO(i) = glin(i)
  300 continue
c
      do 400 i=1,10,1
          AXSSTO(i) = gaxs(i)
  400 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  whchax (gmch,kfl,kcnt)
c
c   FUNCTION:  This routine determines which axes need to be output at
c              this time.
c
c   INPUT:  gmch    R*8  D10 - Machine positions for all axes.
c
c   OUTPUT: kfl     I*4  D10 - Returns 1 when this axis is needs to be
c                              output.
c
c           kcnt    I*4  D1  - Number of axes that need to be output.
c
c***********************************************************************
c
      subroutine whchax (gmch,kfl,kcnt)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308))
      equivalence (MOTREG,KPOSMP(0381)), (IJKREG,KPOSMP(0827))
      equivalence (REGFRC,KPOSMP(0603)), (MOTFLG,KPOSMP(1073))
      equivalence (INCR  ,KPOSMP(1226)), (MLNFRC,KPOSMP(1373))
      equivalence (MRTFRC,KPOSMP(1387)), (IRTDEF,KPOSMP(1485))
      equivalence (HLDFLG,KPOSMP(1622))
      equivalence (HLDSW ,KPOSMP(1623)), (IJKROT,KPOSMP(1739))
      equivalence (FMTDES,KPOSMP(2133))
c
      integer*2 FMTDES(10,MAXFMT)
c
      integer*4 REGFRC(MAXFMT),MOTFLG,INCR,MOTREG(24),HLDFLG,HLDSW,
     1          IJKROT,IJKREG(3),MCHOPT(20),MRTFRC,IRTDEF,MLNFRC
c
      equivalence (REGSTO,POSMAP(1032)), (PPTOLR,POSMAP(1274))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (HLDAXS,POSMAP(2167))
c
      real*8 REGSTO(MAXFMT),PPTOLR(10),HLDAXS(10),TLVEC(3)
c
      integer*4 kfl(10),kcnt
c
      real*8 gmch(10)
c
      integer*4 inc,i,isav,isub1(10),isub2(10),inc1,iacy,ifrc,iax(10)
     1
c
      real*8 rnum,rsav,rtol
c
      data isub1 /1,3,5,7,9,11,14,17,20,23/
      data isub2 /1,3,5,7,9,11,13,16,19,22/
c
c...Make sure incremental register
c...is not forced out in absolute mode
c...and vice versa
c
c      do 200 i=1,10,1
c          inc    = isub1(i)
c          if (INCR .eq. 1) then
c              if (MOTREG(inc+1) .ne. 0 .and.
c     1            REGFRC(MOTREG(inc+1)) .ne. 0) then
c                  if (MOTREG(inc) .ne. 0)
c     1                    REGFRC(MOTREG(inc)) = REGFRC(MOTREG(inc+1))
c                  REGFRC(MOTREG(inc+1)) = 0
c              endif
c          else
c              if (MOTREG(inc) .ne. 0 .and. REGFRC(MOTREG(inc)) .ne. 0)
c     1                then
c                  if (MOTREG(inc+1) .ne. 0)
c     1                    REGFRC(MOTREG(inc+1)) = REGFRC(MOTREG(inc))
c                  REGFRC(MOTREG(inc)) = 0
c              endif
c          endif
c  200 continue
c
c...Assume this will be a motion block
c
      isav   = MOTFLG
      MOTFLG = 1
c
c...Compare held back axis
c...with held back value instead
c...of current register value
c
      if (HLDFLG .eq. 1) then
          rsav   = REGSTO(MOTREG(isub2(HLDSW)))
          call setcod (MOTREG(isub2(HLDSW)),HLDAXS(HLDSW))
      endif
c
c...Determine which axes registers
c...need to be output
c
      kcnt   = 0
      do 500 i=1,10,1
          inc    = isub2(i)
          kfl(i) = 0
c
c......Rotary axes
c
          if (i .gt. 6) then
             inc1 = i - 6
c
c...vp 5/4/98 for IJK support compare TLVEC instead of
c...rotary axes position.  this requires to keep TLVEC
c...always updated before calling whchax
c
             if (IJKROT .eq. 1 .and. inc1 .lt. 4) then
                iacy = FMTDES(5+MCHOPT(2),IJKREG(inc1))
                rtol = 10.d0**(-iacy)
                call cmpcod (IJKREG(inc1),TLVEC(inc1),rtol,1,kfl(i))
             else
                call cmpcod (MOTREG(inc),gmch(i),PPTOLR(i),1,kfl(i))
                inc1   = isub1(i)
                if (MOTREG(inc1) .ne. 0) then
                   if (REGFRC(MOTREG(inc1)) .gt. 0) kfl(i) = 1
                   if (REGFRC(MOTREG(inc1)) .lt. 0) kfl(i) = 0
                endif
             end if
c
c......Absolute mode
c
          else if (INCR .eq. 1) then
             call cmpcod (MOTREG(inc),gmch(i),PPTOLR(i),1,kfl(i))
c
c......Incremental mode
c
          else
             call increg (MOTREG(inc),gmch(i),rnum)
             call cmpcod (MOTREG(inc+1),rnum,PPTOLR(i),1,kfl(i))
          endif
c
          if (kfl(i) .eq. 1) kcnt = kcnt + 1
  500 continue
c
c...Force all axes on block
c
      if (MLNFRC .eq. 1) call frcmot (2,1,iax)
c
c...Force all rotary axes on block
c
      if (MRTFRC .eq. 1 .and. (IRTDEF .gt. 1 .or. IJKROT .eq. 1)) then
          ifrc   = 0
          inc   = 6 + IRTDEF
          if (IJKROT .eq. 1) inc = 9
          do 600 i=7,inc,1
              if (kfl(i) .eq. 1) ifrc = 1
  600     continue
          if (ifrc .eq. 1) then
              do 620 i=1,10,1
                  iax(i) = 0
  620         continue
              do 650 i=7,inc,1
                  kfl(i) = 1
                  iax(i) = 1
  650         continue
              call frcmot (2,3,iax)
          endif
      endif
c
c...Restore motion flag
c
      MOTFLG = isav
c
c...Restore held back axis value
c
      if (HLDFLG .eq. 1) call setcod (MOTREG(isub2(HLDSW)),rsav)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  whchmv (gfrm,gaxs,kfl)
c
c   FUNCTION:  This routine determines which of the XYZ axes will move
c              from their previous position.  It takes into account
c              both the primary and secondary axes.
c
c   INPUT:  gfrm    R*8  D10 -  Machine slide positions for primary and
c                               secondary linear XYZ axes that we are
c                               coming from.
c
c           gaxs    R*8  D10 -  Machine slide positions for primary and
c                               secondary linear XYZ axes that we are
c                               going to.
c
c   OUTPUT: kfl     I*4  D3  -  Returns a value of 1 for each of the
c                               linear XYZ axes that will move from
c                               their previous position.
c
c***********************************************************************
c
      subroutine whchmv (gfrm,gaxs,kfl)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381))
c
      integer*4 MOTREG(24)
c
      integer*4 kfl(3)
c
      real*8 gaxs(10),gfrm(10)
c
      integer*4 inc,i,ipt(6),iax2
c
      real*8 rnum
c
      data ipt /1,1, 2,2, 3,3/
c
c...Determine which axes move
c...vp 2-aug-94 now comparing real numbers before rounding
c...since almost same numbers after rounding came up as
c...different iax1 and iax2
c
      inc    = 1
      kfl(1) = 0
      kfl(2) = 0
      kfl(3) = 0
      do 500 i=1,6,1
c         call codint (MOTREG(inc),gfrm(i),rnum,iax1)
c         call codint (MOTREG(inc),gaxs(i),rnum,iax2)
c         if (iax1 .ne. iax2) kfl(ipt(i)) = 1
          rnum   = gfrm(i) - gaxs(i)
          call codint (MOTREG(inc),rnum,rnum,iax2)
          if (iax2 .ne. 0) kfl(ipt(i)) = 1
          inc    = inc    + 2
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  hldrap (krap,grap)
c
c   FUNCTION:  This routine saves rapid mode for hold back move
c
c   INPUT:  none
c
c   OUTPUT: krap    I*4  D8  -  Saved RAPID mode prameters (integer)
c
c           grap    I*4  D5  -  Saved RAPID mode prameters (real)
c
c***********************************************************************
c
      subroutine hldrap (krap,grap)
c
      include 'post.inc'
c
      integer*4 krap(7)
      real*8 grap(2)
c
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (IRAP  ,KPOSMP(3199)), (IRAPDO,KPOSMP(3220))
      equivalence (IRPSAV,KPOSMP(3204)), (IRPRET,KPOSMP(3219))
      equivalence (HLDRPN,KPOSMP(3985))
c
      integer*4 IRAP,IRAPDO(5),INCR,IRPSAV,IRPRET,HLDRPN(8)
c
      equivalence (RAPDIS,POSMAP(3582)), (HLDRPR,POSMAP(4550))
c
      real*8 RAPDIS,HLDRPR
c
c...Seve rapid mode
c
      call copynk (IRAPDO,krap,5)
      krap(6) = IRAP
      krap(7) = IRPSAV
      grap(1) = RAPDIS
c
c...End of routine
c
 8000 return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  hlbrap (krap,grap)
c
c   FUNCTION:  This routine restores rapid mode for hold back move
c
c   OUTPUT: krap    I*4  D8  -  Saved RAPID mode prameters (integer)
c
c           grap    I*4  D5  -  Saved RAPID mode prameters (real)
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine hlbrap (krap,grap)
c
      include 'post.inc'
c
      integer*4 krap(7)
      real*8 grap(2)
c
      equivalence (INCR  ,KPOSMP(1226))
      equivalence (IRAP  ,KPOSMP(3199)), (IRAPDO,KPOSMP(3220))
      equivalence (IRPSAV,KPOSMP(3204)), (IRPRET,KPOSMP(3219))
c
      integer*4 IRAP,IRAPDO(5),INCR,IRPSAV,IRPRET
c
      equivalence (RAPDIS,POSMAP(3582))
c
      real*8 RAPDIS
c
c...Seve rapid mode
c
      call copynk (krap,IRAPDO,5)
      IRAP   = krap(6)
      IRPSAV = krap(7)
      RAPDIS = grap(1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  hldfed (kfed,gfed)
c
c   FUNCTION:  This routine saves feed mode for held back move
c
c   INPUT:  none.
c
c   OUTPUT: kfed    I*4  D6  -  Saved feed rate prameters (integer)
c
c           gfed    I*4  D6  -  Saved feed rate prameters (real)
c
c***********************************************************************
c
      subroutine hldfed (kfed,gfed)
c
      include 'post.inc'
c
      integer*4 kfed(2)
      real*8 gfed(6)
c
      equivalence (IFITYP,KPOSMP(3150))
      equivalence (HLDFDN,KPOSMP(3994))
c
      integer*4 IFITYP,HLDFDN(5)
c
      equivalence (PFEED ,POSMAP(3540)), (HLDFDR,POSMAP(4560))
c
      real*8 HLDFDR(6),PFEED(4)
c
c...Seve feed  mode
c
      kfed(1) = IFITYP
      call copyn (PFEED,gfed,4)
c
c...End of routine
c
 8000 return
      end
c
c
c***********************************************************************
c
c   SUBROUTINE:  hlbfed (kfed,gfed)
c
c   FUNCTION:  This routine restores feed mode for held back move
c
c   INPUT:  kfed    I*4  D6  -  Saved feed rate prameters (integer)
c
c           gfed    I*4  D6  -  Saved feed rate prameters (real)
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine hlbfed (kfed,gfed)
c
      include 'post.inc'
c
      integer*4 kfed(2)
      real*8 gfed(6)
c
      equivalence (IFITYP,KPOSMP(3150))
c
      integer*4 IFITYP
c
      equivalence (PFEED ,POSMAP(3540))
c
      real*8 PFEED(4)
c
c...Seve feed  mode
c
      IFITYP = kfed(1)
      call copyn (gfed,PFEED,4)
c
c...End of routine
c
 8000 return
      end
c
