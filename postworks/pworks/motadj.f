c
c***********************************************************************
c
c   FILE NAME:  motadj
c   CONTAINS:
c               excaxs  maxaxc  smodep  maxdep  rapmot  rapsim  rposck
c               mchslw  slwang  slwfed
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        %M% , %I%
c     DATE AND TIME OF LAST  MODIFICATION
c        %G% , %U%
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  excaxs (kaxsw,kcnt,kfl,kax,kary,klft,kpt)
c
c   FUNCTION:  This routine checks that no mutually exclusive axes are
c              output on the same block.  If there are, this routine
c              will break the motion block into multiple motion blocks.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c           kax     I*4  D10 -  Internal array of switches that show
c                               which axes are queued for output.  This
c                               array should not be referenced outside
c                               of this routine.
c
c           kary    I*4  D12 -  Internal array containing the priority
c                               in which to output the axes when
c                               mutually exclusive axes are in the same
c                               block.  This array should not be refer-
c                               enced outside of this routine.
c
c           klft    I*4  D1  -  Contains the number of remaining axes
c                               awaiting output and should not be refer-
c                               enced outside of this routine.
c
c           kpt     I*4  D1  -  Contains the pointer within the priority
c                               block and should not be referenced outside
c                               of this routine.
c

c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine excaxs (kaxsw,kcnt,kfl,kax,kary,klft,kpt)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (MOTREG,KPOSMP(0381))
      equivalence (REGBNC,KPOSMP(2001)), (EXCLAX,KPOSMP(3401))
      equivalence (EXCLCO,KPOSMP(3417)), (EXCLPS,KPOSMP(3421))
      equivalence (EXCLNE,KPOSMP(3469)), (NUMEXC,KPOSMP(3517))
      equivalence (EXCLNM,KPOSMP(3518))
c
      integer*4 IDUMMY,EXCLAX(4,4),EXCLCO(4),EXCLPS(12,4),EXCLNE(12,4),
     1          MOTREG(24),REGBNC(MAXFMT),EXCLNM(4),NUMEXC
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3)
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 kaxsw(10),kcnt,kfl,kax(10),kary(12),kpt,klft
c
      integer*4 i,inc,idid,isgn,iax(10),ipt,j
c
      real*8 raxs(10)
c
      character*1 lax(10)
      character*20 lbuf
      character*80 msg
c
      data iax /1,3,5,7,9,11,13,16,19,22/
      data lax /'X','U','Y','V','Z','W','A','B','C','D'/
c
c...Second time here
c...Pop axes off of stack
c
      if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
          go to 400
      endif
c
c...Determine if there are any mutually
c...exclusive axes in this block
c
      do 200 inc=1,NUMEXC,1
          idid   = 0
          do 100 j=1,EXCLNM(inc),1
              if (kaxsw(EXCLAX(j,inc)) .eq. 1) then
                  if (idid .ne. 0) go to 300
                  idid   = j
              endif
  100     continue
  200 continue
      go to 8000
c
c......Mutually exclusive axes are in this block
c......Break it up into 2 or more blocks
c
  300 kpt    = 0
      klft   = kcnt
      do 320 i=1,10,1
          kax(i) = kaxsw(i)
  320 continue
c
c.........Output error message
c
      ipt    = iax(EXCLAX(idid,inc))
      lbuf   = REGST(MOTREG(ipt))
      if (REGBNC(MOTREG(ipt)) .eq. 0) lbuf = lax(EXCLAX(idid,inc))
      call perrst ('EXCLAXS',1,msg,0,0.d0,lbuf,3)
c
      ipt    = iax(EXCLAX(j,inc))
      lbuf   = REGST(MOTREG(ipt))
      if (REGBNC(MOTREG(ipt)) .eq. 0) lbuf = lax(EXCLAX(j,inc))
      call perrst (msg,2,msg,0,0.d0,lbuf,3)
      call psterr (2,msg,'MULTBLK',-1)
c
c.........Determine which way the
c.........controlling axis moves
c
      isgn   = 1
      if (EXCLCO(inc) .ne. 0 .and. kaxsw(EXCLCO(inc)) .eq. 1 .and.
     1    AXSOUT(EXCLCO(inc)) .lt. AXSSTO(EXCLCO(inc))) isgn = 2
c
      if (isgn .eq. 1) then
          do 360 i=1,12,1
              kary(i) = EXCLPS(i,inc)
  360     continue
      else
          do 370 i=1,12,1
              kary(i) = EXCLNE(i,inc)
  370     continue
      endif
c
c.........Determine which axes to output
c.........From priority
c
  400 kcnt   = 0
      do 450 i=1,10,1
          raxs(i) = AXSOUT(i)
          kaxsw(i) = 0
  450 continue
      if (kpt .ge. 12) go to 600
      do 500 kpt=kpt+1,12,1
          if (kary(kpt) .eq. IDUMMY) go to 600
c
c............End-of-Block
c............Set current axes position
c
          if (kary(kpt) .eq. -1) then
              if (klft .eq. 0) go to 1000
              do 480 i=1,10,1
                  if (kax(i) .eq. 1) AXSOUT(i) = AXSSTO(i)
  480         continue
              kfl    = 1
              go to 1000
          else if (kax(kary(kpt)) .eq. 1) then
              kaxsw(kary(kpt)) = 1
              kax(kary(kpt)) = 0
              kcnt   = kcnt   + 1
              klft   = klft   - 1
          endif
  500 continue
c
c.........End of user specified order
c.........Select rest of axes based on
c.........order of appearance
c
  600 if (klft .eq. 0) go to 8000
      do 650 i=1,10,1
          if (kax(i) .eq. 1) then
              kaxsw(i) = 1
              kcnt   = kcnt   + 1
          endif
  650 continue
c
c.........Adjust axes for output
c
 1000 if (kfl .ne. 0) call pshaxs (raxs,TLVEC)
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  maxaxc (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine checks that the number of axes to be output
c              in the current motion block does not exceed the maximum
c              number of axes allowed.  If it does, this routine will
c              break the motion block into multiple motion blocks.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returne*************
c
      subroutine maxaxc (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (IDUMMY,KPOSMP(0088)), (MAXAXS,KPOSMP(1212))
      equivalence (AXSPRI,KPOSMP(1213))
c
      integer*4 IDUMMY,MAXAXS,AXSPRI(10)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 icnt,i,iax(10),ist,iaxin(10)
c
c...Output all axes in this block
c
      if (kcnt .le. MAXAXS .and. kfl .eq. 0) go to 8000
      if (kfl .eq. 0) call psterr (2,'MANYAXIS','MULTBLK',-1)
c
c...Second time here
c...Pop previous position from stack
c
      if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
          if (kcnt .le. MAXAXS) go to 8000
          icnt   = kcnt
          do 100 i=1,10,1
              iax(i) = kaxsw(i)
              iaxin(i) = kaxsw(i)
              kaxsw(i) = 0
  100     continue
c
c...First time here
c...Initialize routine
c
      else
          icnt   = kcnt
          do 200 i=1,10,1
              iax(i) = kaxsw(i)
              iaxin(i) = kaxsw(i)
              kaxsw(i) = 0
  200     continue
      endif
c
c...Determine which axes to output
c...from priority
c
      ist    = 0
      kcnt   = 0
  300 ist    = ist    + 1
      if (AXSPRI(ist) .eq. IDUMMY) go to 500
      if (iaxin(AXSPRI(ist)) .eq. 1) then
          kaxsw(AXSPRI(ist)) = 1
          iax(AXSPRI(ist)) = 0
          kcnt   = kcnt   + 1
          if (kcnt .eq. MAXAXS) go to 800
      endif
      go to 300
c
c......Did not fill up motion block
c......Select rest of axes based on
c......order of appearance
c
  500 do 600 i=1,10,1
          if (iaxin(i) .eq. 1 .and. kaxsw(i) .eq. 0) then
              kaxsw(i) = 1
              kcnt   = kcnt   + 1
              if (kcnt .eq. MAXAXS) go to 800
          endif
  600 continue
c
c......Push left over axes
c......onto stack
c
  800 kfl    = 1
      icnt   = icnt   - kcnt
      call pshaxs (AXSOUT,TLVEC)
c
c......Set up current position arrays
c
      do 900 i=1,10,1
          if (kaxsw(i) .eq. 0) AXSOUT(i) = AXSSTO(i)
  900 continue
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  maxdep (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine checks that any given axis does not move
c              more than the maximum departure for that axis allows.
c              If one does, this routine will break the move into mul-
c              tiple moves.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine maxdep (kaxsw,kcnt,kfl)
c
      include 'menu.inc'
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (REGBNC,KPOSMP(2001))
      equivalence (IACHFL,KPOSMP(1646)), (IMXDRP,KPOSMP(1743))
      equivalence (IRAP  ,KPOSMP(3199))
c
      integer*4 MOTREG(24),REGBNC(MAXFMT),IACHFL,IMXDRP,IRAP
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (PPMAXD,POSMAP(1584)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),PPMAXD(10),
     1       TLVEC(3)
c
      equivalence (REGST ,CPOSMP(7011))
c
      character*24 REGST(MAXFMT)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 i,isw,isub(10),iax(10)
c
      real*8 rnum(10),rdlt,rmax,ratio,rdif
c
      character*1 lax(10)
      character*20 lbuf
      character*80 msg1,msg2
c
      data isub /1,3,5,7,9,11,13,16,19,22/
      data iax /1,3,5,7,9,11,13,16,19,22/
      data lax /'X','U','Y','V','Z','W','A','B','C','D'/
c
c...Don't do anything with RAPID moves
c...if requested
c
      if (IMXDRP .eq. 2 .and. IRAP .gt. 0) go to 8000
c
c...Second time here
c...Pop previous position from stack
c
      if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
      endif
c
c...Determine if any axis moves more
c...than the maximum departure allowed
c
      isw   = 0
      rmax  = 0.
      do 100 i=1,10,1
          call increg (MOTREG(isub(i)),AXSOUT(i),rnum(i))
          rdlt   = dabs(rnum(i))
          if (rdlt .gt. dabs(PPMAXD(i))+.000001) then
c
c......Departure is negative
c......Output warning message
c
              if (PPMAXD(i) .le. 0.) then
                  if (IACHFL .eq. 0) then
                      lbuf = REGST(MOTREG(iax(i)))
                      if (REGBNC(MOTREG(iax(i))) .eq. 0) lbuf = lax(i)
                      call perrst ('DEPMAX',1,msg1,0,rdlt,lbuf,3)
                      call perrst ('DEPLMT',1,msg2,0,rdlt,lbuf,2)
                      call perrst (msg2,2,msg2,0,dabs(PPMAXD(i)),lbuf,2)
                      call psterr (1,msg1,msg2,-1)
                      IACHFL = 0
                  endif
c
c.....Break up move logic
c
              else if (rdlt-PPMAXD(i) .gt. rmax) then
                  isw    = i
                  rdif   = rdlt
                  rmax   = rdlt   - PPMAXD(i)
              endif
          endif
  100 continue
      if (isw .eq. 0) go to 8000
c
c......Break up move into smaller moves
c
      kfl    = 1
      ratio  = 1.0d0 - (PPMAXD(isw) / rdif)
      call pshaxs (AXSOUT,TLVEC)
      do 200 i=1,10,1
          AXSOUT(i) = AXSOUT(i) - rnum(i) * ratio
  200 continue
c
c.........Set up current position arrays
c
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
      call whchax (AXSOUT,kaxsw,kcnt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  smodep (kfl)
c
c   FUNCTION:  This routine checks that any given motion does not move
c              more than the user requested while spline interpolation
c              mode (SMOOTH) for an Ultrasonic Cutter is active.
c
c   INPUT:  kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 or greater.  1+ = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine smodep (kfl)
c
      include 'post.inc'
c
      equivalence (SMONPT,KPOSMP(4095))
c
      integer*4 SMONPT
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (STONUM,POSMAP(1387)), (SMOSTP,POSMAP(4466))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),SMOSTP,
     1       TLVEC(3),STONUM(3,4),ROTSTO(20,2)
c
      integer*4 kfl
c
      integer*4 icnt,iaxsw(10)
c
      real*8 rnum,dvec(4),nvec(3),rvec(2),span
c
c...Second time here
c...Pop previous position from stack
c
      if (kfl .ne. 0) then
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iaxsw,icnt)
      endif
c
c...Calculate length of move
c
      dvec(1) = MCHNUM(1,4) - STONUM(1,4)
      dvec(2) = MCHNUM(2,4) - STONUM(2,4)
      dvec(3) = MCHNUM(3,4) - STONUM(3,4)
      dvec(4) = dsqrt (dvec(1)**2 + dvec(2)**2 + dvec(3)**2)
      nvec(1) = dvec(1) / dvec(4)
      nvec(2) = dvec(2) / dvec(4)
      nvec(3) = dvec(3) / dvec(4)
      rvec(1) = ROTANG(1,2) - ROTSTO(1,2)
      rvec(2) = ROTANG(2,2) - ROTSTO(2,2)
c
c...Determine if any axis moves more
c...than the maximum departure allowed
c
  300 if (dvec(4) .le. SMOSTP .or. kfl .ge. SMONPT*2) then
          kfl    = 0
          go to 8000
c
c...Break up into multiple moves
c......Beginning of move
c
      else if (kfl .lt. SMONPT) then
          kfl    = kfl    + 1
          call pshaxs (AXSOUT,TLVEC)
          MCHNUM(1,4) = STONUM(1,4) + nvec(1)*SMOSTP
          MCHNUM(2,4) = STONUM(2,4) + nvec(2)*SMOSTP
          MCHNUM(3,4) = STONUM(3,4) + nvec(3)*SMOSTP
          span   = SMOSTP / dvec(4)
          ROTANG(1,2) = ROTSTO(1,2) + span*rvec(1)
          ROTANG(2,2) = ROTSTO(2,2) + span*rvec(2)
c
c......End of move
c
      else
          kfl    = kfl    + 1
          rnum   = SMOSTP * (SMONPT*2-kfl+1)
          if (rnum .ge. dvec(4)) go to 300
          call pshaxs (AXSOUT,TLVEC)
          MCHNUM(1,4) = MCHNUM(1,4) - nvec(1)*rnum
          MCHNUM(2,4) = MCHNUM(2,4) - nvec(2)*rnum
          MCHNUM(3,4) = MCHNUM(3,4) - nvec(3)*rnum
          span   = rnum   / dvec(4)
          ROTANG(1,2) = ROTANG(1,2) - span*rvec(1)
          ROTANG(2,2) = ROTANG(2,2) - span*rvec(2)
      endif
c
c.........Set up current position arrays
c
      call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rapmot (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine alters motion for rapid logic.  It may break
c              up the move into multiple blocks.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1-2 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine rapmot (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (SIMACT,KPOSMP(0174)), (XFMFL ,KPOSMP(0969))
      equivalence (IRETFL,KPOSMP(1345)), (IJKROT,KPOSMP(1739))
      equivalence (IRAP  ,KPOSMP(3199)), (ILINPT,KPOSMP(3200))
      equivalence (IRAPDO,KPOSMP(3220)), (IRPDSV,KPOSMP(3230))
c
      integer*4 IRAP,IRAPDO(8),IRPDSV(9),IRETFL,ILINPT,IJKROT,SIMACT,
     1          XFMFL(20)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (AXSSTO,POSMAP(1425)), (RAPDIS,POSMAP(3582))
      equivalence (XFMMAT,POSMAP(4025))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       VECSAV(3),STONUM(3,4),ROTSTO(20,2),RAPDIS,AXSSTO(10),
     2       XFMMAT(12)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 i,inc,ierr,is3,is1,is2,ispv(3),isav
c
      logical isidmx
c
      real*8 ratio,rdis,pl(4),mdis(10),mdlt,tvec(3),rmch(3),rsto(3),
     1       rval1,rval2
c
      data ispv /3,2,1/
c
c...Re-entry after RETRCT move
c
      if (kfl .eq. 2) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
          call retrst
          do 20 i=1,8,1
              IRAPDO(i) = IRPDSV(i)
   20     continue
          IRAP   = IRPDSV(9)
c
c...Second time here
c...Pop previous position from stack
c
      else if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
          if (IRAPDO(1) .eq. 1) call raprst
          go to 8000
c
c...Retract required with rapid move
c
      else if (IRAPDO(5) .eq. 1 .and. kfl .eq. 0 .and. IRETFL .eq. 0)
     1     then
          kfl    = 2
          call pshaxs (AXSOUT,TLVEC)
          do 60 i=1,8,1
              IRPDSV(i) = IRAPDO(i)
   60     continue
          IRPDSV(9) = IRAP
          call retrct (2)
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
          call whchax (AXSOUT,kaxsw,kcnt)
          go to 8000
      endif
c
c...Rapid not active
c
      if (IRAP .eq. 0 .or. ILINPT .eq. 1) go to 8000
c
c...Don't break rapid into multiple blocks
c
      if (IRAPDO(1) .eq. 1) then
          if (RAPDIS .eq. 0 .or. (IRAPDO(4) .eq. 2 .and.
     1        RAPDIS .eq. 1.)) go to 8000
c
c......Output partial move at rapid rate
c......and rest of move at programmed feed rate
c
          do 100 i=1,10,1
              mdis(i) = AXSOUT(i) - AXSSTO(i)
  100     continue
          mdlt   = dsqrt (mdis(1)**2 + mdis(2)**2 + mdis(3)**2 +
     1                    mdis(4)**2 + mdis(5)**2 + mdis(6)**2)
          rdis   = RAPDIS
          if (IRAPDO(4) .eq. 2) rdis = mdlt * RAPDIS
c
c.........Move is too short
c.........Output entire move at feed rate
c
          if (rdis .ge. mdlt) then
              IRAP   = 3
              call raprst
c
c.........Output rapid portion of move
c
          else
              kfl    = 1
              ratio  = rdis   / mdlt
              call pshaxs (AXSOUT,TLVEC)
              do 200 i=1,10,1
                  AXSOUT(i) = AXSOUT(i) - mdis(i) * ratio
  200         continue
c
c............Set up current position arrays
c
              call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
              call whchax (AXSOUT,kaxsw,kcnt)
          endif
c
c...Rapid up tool axis vector
c
      else if (IRAPDO(1) .eq. 5 .and. IRAPDO(2) .eq. 1) then
c
c......Get major part of tool axis
c
          if (IJKROT .eq. 1) then
              call copyn (VECSAV,tvec,3)
          else
              call pivvec (ROTSTO,tvec)
          endif
          if (XFMFL(15) .eq. 1) call ptxfm (tvec,tvec,2)
          is3    = 3
          if (abs(tvec(1)) .gt. abs(tvec(is3))) is3 = 1
          if (abs(tvec(2)) .gt. abs(tvec(is3))) is3 = 2
c
          pl(1) = 0.
          pl(2) = 0.
          pl(3) = 0.
          pl(is3) = 1.
c
c......Move all axis at same time
c
          if (XFMFL(15) .eq. 1) then
              call ptxfm (MCHNUM(1,4),rmch,1)
              call ptxfm (STONUM(1,4),rsto,1)
          else
              call copyn (MCHNUM(1,4),rmch,3)
              call copyn (STONUM(1,4),rsto,3)
          endif
          call dpoint (rmch(is3),rval1,5)
          call dpoint (rsto(is3),rval2,5)
          if (rval1 .eq. rval2) go to 8000
c
c......Retract to plane first
c
          if ((tvec(is3) .ge. 0. .and. rmch(is3) .gt. rsto(is3)) .or.
     1        (tvec(is3) .lt. 0. .and. rmch(is3) .lt. rsto(is3))) then
              pl(4) = rmch(is3)
              call plnint (rsto,tvec,pl,rmch,ierr)
              if (ierr .eq. 1) go to 8000
              call pshaxs (AXSOUT,TLVEC)
              TLVEC(1) = VECSAV(1)
              TLVEC(2) = VECSAV(2)
              TLVEC(3) = VECSAV(3)
              call cpyrot (ROTSTO,ROTANG)
c
c......Position secondary axes first
c
          else
              pl(4) = rsto(is3)
              if (IJKROT .eq. 1) then
                  call copyn (TLVEC,tvec,3)
              else
                  call pivvec (ROTANG,tvec)
              endif
              if (XFMFL(15) .eq. 1) call ptxfm (tvec,tvec,2)
              call plnint (rmch,tvec,pl,rmch,ierr)
              if (ierr .eq. 1) go to 8000
              call pshaxs (AXSOUT,tvec)
          endif
c
c......Calculate actual position
c
          if (XFMFL(15) .eq. 1) then
              call ptxfr (rmch,MCHNUM(1,4),1)
          else
              call copyn (rmch,MCHNUM(1,4),3)
          endif
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
          call whchax (AXSOUT,kaxsw,kcnt)
          kfl    = 1
c
c...Position along major axis
c
      else
          isav   = XFMFL(4)
          if (SIMACT .eq. 1 .and. XFMFL(15) .eq. 1 .and.
     1        .not. isidmx(XFMMAT)) XFMFL(4) = 1
          inc    = IRAPDO(1)
          if (IJKROT .eq. 1) then
              call copyn (VECSAV,tvec,3)
          else
              call pivvec (ROTSTO,tvec)
          endif
          call pivvec (ROTSTO,tvec)
          if (XFMFL(15) .eq. 1) call ptxfm (tvec,tvec,2)
c
c......TOOL
c......Determine major axis
c
          if (inc .eq. 5) then
              inc    = 4
              if (abs(tvec(1)) .gt. abs(tvec(inc-1))) inc = 2
              if (abs(tvec(2)) .gt. abs(tvec(inc-1))) inc = 3
          endif
c
c......XAXIS
c
          if (inc .eq. 2) then
              is1    = 2
              is2    = 3
              is3    = 1
c
c......YAXIS
c
          else if (inc .eq. 3) then
              is1    = 3
              is2    = 1
              is3    = 2
c
c......ZAXIS
c
          else
              is1    = 1
              is2    = 2
              is3    = 3
          endif
c
c......Move all axis at same time
c
          if (XFMFL(15) .eq. 1) then
              call ptxfm (MCHNUM(1,4),rmch,1)
              call ptxfm (STONUM(1,4),rsto,1)
          else
              call copyn (MCHNUM(1,4),rmch,3)
              call copyn (STONUM(1,4),rsto,3)
          endif
          do 700 i=1,3,1
              call dpoint (rmch(i),rmch(i),5)
              call dpoint (rsto(i),rsto(i),5)
  700     continue
          if (rmch(is3) .eq. rsto(is3) .or.
     1       (rmch(is1) .eq. rsto(is1) .and.
     2        rmch(is2) .eq. rsto(is2))) go to 8000
c
c......Retract tool first
c
          kfl    = 1
          call pshaxs (AXSOUT,tvec)
          call dpoint (tvec(is3),tvec(is3),6)
          if ((tvec(is3) .ge. 0. .and.
     1        rmch(is3) .gt. rsto(is3)) .or.
     2        (tvec(is3) .lt. 0. .and.
     3        rmch(is3) .lt. rsto(is3))) then
              rmch(is1) = rsto(is1)
              rmch(is2) = rsto(is2)
              if (XFMFL(15) .eq. 1) then
                  call ptxfr (rmch,MCHNUM(1,4),1)
              else
                  call copyn (rmch,MCHNUM(1,4),3)
              endif
              TLVEC(1) = VECSAV(1)
              TLVEC(2) = VECSAV(2)
              TLVEC(3) = VECSAV(3)
              call cpyrot (ROTSTO,ROTANG)
c
c......Position secondary axes first
c
          else
              rmch(is3) = rsto(is3)
              if (XFMFL(15) .eq. 1) then
                  call ptxfr (rmch,MCHNUM(1,4),1)
              else
                  call copyn (rmch,MCHNUM(1,4),3)
              endif
          endif
c
c......Calculate actual position
c
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,3,1)
          call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,4,5)
          call whchax (AXSOUT,kaxsw,kcnt)
          XFMFL(4) = isav
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rapsim (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine alters motion for rapid positioning moves
c              when simulation is in effect.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = This routine was called
c                               before and move has been altered at least
c                               once.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine rapsim (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (IRAP  ,KPOSMP(3199)), (IRTDEF,KPOSMP(1485))
c
      integer*4 IRAP,IRTDEF
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (AXSDIS,POSMAP(1574)), (RAPLMT,POSMAP(3567))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),TLVEC(3),
     1       AXSSTO(10),RAPLMT(10),AXSDIS(10)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 i,ifl
c
      real*8 rdis(10),rtim(10),mintim
c
c...Second time here
c...Pop previous position from stack
c
      if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
      endif
      if (kcnt .le. 1 .or. IRAP .ne. 1) go to 8000
c
c...Calculate machining time for each axes
c
      mintim = 10000.
      do 100 i=1,6+IRTDEF,1
          rdis(i) = AXSOUT(i) - AXSSTO(i)
          rtim(i) = dabs(rdis(i)) / RAPLMT(i)
          if (rtim(i) .ne. 0. .and. rtim(i) .lt. mintim)
     1        mintim = rtim(i)
  100 continue
      if (mintim .eq. 0. .or. mintim .eq. 10000.) go to 8000
c
c...Determine if the move needs to
c...be broken up
c
      ifl    = 0
      do 200 i=1,6+IRTDEF,1
          if (rtim(i) .gt. mintim) ifl = 1
  200 continue
      if (ifl .eq. 0) go to 8000
c
c...Break up move
c
      call pshaxs (AXSOUT,TLVEC)
      do 300 i=1,6+IRTDEF,1
          if (rtim(i) .ne. 0)
     1        AXSOUT(i) = AXSSTO(i) + rdis(i)*(mintim/rtim(i))
  300 continue
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
      call whchax (AXSOUT,kaxsw,kcnt)
      kfl    = 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rposck (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine checks that a positioning table is not out-
c              put with linear motion.
c
c              *WARNING* 'rposck' may set the variable 'POSFED' to 2,
c                        disabling feed rate output, and will not reset
c                        it to 1.  It is the calling program's responsi-
c                        bility to reset 'POSFED'.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine rposck (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (IRTMOD,KPOSMP(1284)), (IRTDEF,KPOSMP(1485))
      equivalence (POSFDF,KPOSMP(3208)), (POSFED,KPOSMP(3209))
c
      integer*4 IRTDEF,IRTMOD(4),POSFDF,POSFED
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3)
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 icnt,i,iax(10),inc,iout(4)
c
c...First time here
c...Determine if we need to break out
c...positioning table
c
      if (kfl .eq. 0) then
          inc    = 0
          do 100 i=1,IRTDEF,1
              if (IRTMOD(i) .eq. 2 .and. kaxsw(i+6) .eq. 1) then
                  inc    = inc    + 1
                  iout(inc) = i      + 6
              endif
  100     continue
          if (inc .ne. 0) POSFED = POSFDF
          if (inc .eq. 0 .or. inc .eq. kcnt) go to 8000
c
c......Break out all positioning axes
c
          call psterr (2,'POSITROT','MULTBLK',-1)
          icnt   = kcnt
          do 200 i=1,10,1
              iax(i) = kaxsw(i)
              kaxsw(i) = 0
  200     continue
          do 300 i=1,inc,1
              kaxsw(iout(i)) = 1
              iax(iout(i)) = 0
  300     continue
          kcnt   = inc
c
c......Push left over axes
c......onto stack
c
          kfl    = 1
          icnt   = icnt   - kcnt
          call pshaxs (AXSOUT,TLVEC)
c
c......Set up current position arrays
c
          do 900 i=1,10,1
              if (kaxsw(i) .eq. 0) AXSOUT(i) = AXSSTO(i)
  900     continue
          call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
c
c...Second time here
c...Pop previous position from stack
c
      else
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  mchslw (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine outputs post generated slowdown spans or
c              decides whether or not to output a slowdown code.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine mchslw (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (SLWFL ,KPOSMP(1701)), (ISLWDN,KPOSMP(1721))
      equivalence (ISLWFD,KPOSMP(1723)), (ICUTDO,KPOSMP(3301))
c
      integer*4 SLWFL(5),ISLWDN,ISLWFD,ICUTDO(15)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425))
      equivalence (SLWVR ,POSMAP(2451)), (SLWFD ,POSMAP(2471))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3),SLWVR(5),SLWFD,RAD
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 ip(3),icir,dstp(5),nstp
c
      real*8 rmch(3,3),rvec(3,4),rang,rdif,span,rfed
c
      data nstp /5/, dstp /1,2,3,1054,1055/
c
c...Machine supported slowdowns
c
      if (SLWFL(1) .eq. 1) then
c
c......Determine if we should output a slowdown code
c......Based on angular change
c
          if (ISLWDN .ne. 3 .and. SLWVR(1) .ne. 0.) then
c
c.........Get from, current & to points
c
              call cutpos (ip,rmch,icir,dstp,nstp,-1,1)
c
c.........Calculate angular change
c
              call slwang (rmch,rvec,rang)
              rdif   = dacos(rang) * RAD
c
c.........Determine if we should output slowdown code
c
              if (rdif .lt. SLWVR(1)) then
                  ISLWDN = 4
              else
                  ISLWDN = 1
              endif
          endif
c
c...Post generated slowdown
c
      else
c
c......Generate slowdown span
c
          if (kfl .eq. 0) then
c
c.........Get from, current & to points
c
              call cutpos (ip,rmch,icir,dstp,nstp,-1,1)
c
c.........Calculate angular change
c
              call slwang (rmch,rvec,rang)
c
c.........Calculate slowdown feedrate
c
              call slwfed (rmch,kaxsw,rfed)
              if (rang .eq. 1.) go to 8000
              SLWFD  = SLWVR(3) / (1.0 - rang)
              if (rfed .le. SLWFD) then
                  if (ISLWDN .eq. 3) ISLWDN = 2
                  go to 8000
              endif
c
c.........Calculate slowdown span
c
              span   = ((rfed**2 - SLWFD**2) /
     1                 (2.0 * SLWVR(4) * 60.**2)) + SLWVR(5)
c
c.........Span is longer than original move
c.........Output entire span at slowdown feedrate
c
              if (span .ge. rvec(1,4)) then
                  call psterr (1,'SLWSHORT',' ',-1)
                  ISLWFD = 1
                  if (ISLWDN .eq. 3) ISLWDN = 2
c
c.........Output calculated span at original feedrate
c
              else
                  kfl    = 1
                  call pshaxs (AXSOUT,TLVEC)
                  span   = span   / dsqrt(rvec(1,1)**2 + rvec(2,1)**2 +
     1                                    rvec(3,1)**2)
                  MCHNUM(1,3) = MCHNUM(1,3) - rvec(1,1) * span
                  MCHNUM(2,3) = MCHNUM(2,3) - rvec(2,1) * span
                  MCHNUM(3,3) = MCHNUM(3,3) - rvec(3,1) * span
c
c.........Set up current position arrays
c
                  call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,2,1)
                  call alladj (MCHNUM,LINAXS,AXSOUT,ROTANG,3,5)
                  call whchax (AXSOUT,kaxsw,kcnt)
              endif
c
c......Second time here
c......Pop previous position from stack
c......Output final point at slowdown feedrate
c
          else
              kfl    = 0
              ISLWFD = 1
              call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
              if (ISLWDN .eq. 3) ISLWDN = 2
c
c.........Output cutcom codes
c
              if (ICUTDO(2) .eq. 1) call cutout (kaxsw)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  slwang (gmch,gvec,gang)
c
c   FUNCTION:  This routine calculates the delta moves and angular
c              changes as required to generate a slowdown block.
c
c   INPUT:  gmch    R*8  D3.3 -  (n,1) = Point coming from, (n,2) = Cur-
c                                rent point, (n,3) = Next point.
c
c   OUTPUT: gvec    R*8  D3.4 -  (n,1) = Delta distance of From/Current,
c                                (n,2) = Current/Next, (n,3) = Next/FROM
c                                (n,4) = Length of above moves.
c
c           gang    R*8  D1   -  Cosine of angular change of next move
c                                compared to current move.
c
c***********************************************************************
c
      subroutine slwang (gmch,gvec,gang)
c
      include 'post.inc'
c
      real*8 gmch(3,3),gvec(3,4),gang
c
      integer*4 is(3),ip(3),i
c
      real*8 dx(3),dy(3),dz(3),dang
c
      data is /2,3,1/, ip /1,2,3/
c
c...Calculate axis deltas
c...FROM to CURRENT
c...CURRENT to NEXT
c...NEXT to FROM
c
      do 100 i=1,3,1
          gvec(1,i) = gmch(1,is(i)) - gmch(1,ip(i))
          gvec(2,i) = gmch(2,is(i)) - gmch(2,ip(i))
          gvec(3,i) = gmch(3,is(i)) - gmch(3,ip(i))
  100 continue
c
c......Calculate total linear deltas &
c......Unitize individual deltas
c
      do 200 i=1,3,1
          gvec(i,4) = dsqrt (gvec(1,i)**2 + gvec(2,i)**2 + gvec(3,i)**2)
          if (gvec(i,4) .eq. 0.) then
              dx(i) = 0.
              dy(i) = 0.
              dz(i) = 0.
          else
              dx(i) = gvec(1,i) / gvec(i,4)
              dy(i) = gvec(2,i) / gvec(i,4)
              dz(i) = gvec(3,i) / gvec(i,4)
          endif
  200 continue
c
c...Calculate angular change
c
c     if (gvec(1,4) .eq. 0.) then
c         gang   = 1.
c     else if (gvec(2,4) .eq. 0. .or. gvec(3,4) .eq. 0.) then
c         gang   = -1.
c     else
c         gang   = (dx(1)*dx(3)) + (dy(1)*dy(3)) + (dz(1)*dz(3))
c         call dpoint (gang,gang,5)
c         if (dabs(gang) .ne. 1.d0) then
c             gang   = (gvec(1,4)**2 + gvec(2,4)**2 - gvec(3,4)**2) /
c    1                 (2. * gvec(1,4) * gvec(2,4))
c             gang   = gang   * (-1.)
c         endif
c     endif
      if (gvec(1,4) .eq. 0.) then
          gang   = 1.
      else if (gvec(2,4) .eq. 0. .or. gvec(3,4) .eq. 0.) then
          gang   = -1.
      else
          gang   = dx(1)*dx(2) + dy(1)*dy(2) + dz(1)*dz(2)
          call dpoint (gang,dang,5)
          if (dabs(dang) .eq. 1.d0) gang = dang
      end if
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  slwfed (gmch,kfl,gfed)
c
c   FUNCTION:  This routine calculates the programmed feed rate to be
c              used in calculating post generated slowdown blocks.
c
c   INPUT:  gmch    R*8  D3.3 -  (n,1) = Point coming from, (n,2) = Cur-
c                                rent point, (n,3) = Next point.
c
c           kaxsw   I*4  D10  -  Array of switches that show which axes
c                                are waiting to be output.  A value of 1
c                                signifies this axis needs to be output.
c
c   OUTPUT: gfed    I*4  D1   -  Programmed feedrate.
c
c***********************************************************************
c
      subroutine slwfed (gmch,kfl,gfed)
c
      include 'post.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (STONUM,POSMAP(1387))
      equivalence (FEED  ,POSMAP(3547))
c
      real*8 MCHNUM(3,4),STONUM(3,4),FEED(4)
c
      integer*4 kfl(10)
c
      real*8 gmch(3,3),gfed
c
      real*8 rdis,rtim
c
c...Get programmed feedrate
c
      call fedctl (kfl)
c
c...Calculate machining time
c
      rdis   = dsqrt((MCHNUM(1,2)-STONUM(1,2))**2 +
     1               (MCHNUM(2,2)-STONUM(2,2))**2 +
     2               (MCHNUM(3,2)-STONUM(3,2))**2)
      rtim   = rdis   / FEED(1)
c
c...Calculate actual feed rate
c...for rotary adjusted point
c
      rdis   = dsqrt((gmch(1,2)-gmch(1,1))**2 +
     1               (gmch(2,2)-gmch(2,1))**2 +
     2               (gmch(3,2)-gmch(3,1))**2)
      gfed   = rdis   / rtim
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  thrzer (kfl)
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
      subroutine thrzer (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      integer*4 kaxsw(10),kcnt,kfl
c
      equivalence (MOTREG,KPOSMP(0381))
c
      integer*4 MOTREG(24)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3),RAD
c
      real*8 rnum,vec(3),delt,unv(3),pt(3),rd(3),planes(12),ptin(9),
     -       ndist,ndot
c
      integer*4 i,n,isw(3),isub(3),is(3),ix,ier,inm1,inm2,ismt(3)
c
      data isub /1,3,5/, ismt /1,5,9/
      data planes /1.,.0,.0,.0, .0,1.,.0,.0, .0,.0,1.,.0/
c
      if (kfl .ne. 0) then
          kfl = kfl - 1
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
          go to 8000
      endif
c
c...Check if any linear axis crosses zero boundry
c
      do 110 i=1,3,1
         isw(i) = 0
         if (AXSOUT(isub(i))*AXSSTO(isub(i)) .lt. 0.d0) then
            call codint (MOTREG(ismt(i)),AXSOUT(isub(i)),rnum,inm1)
            call codint (MOTREG(ismt(i)),AXSSTO(isub(i)),rnum,inm2)
            if (inm1*inm2 .lt. 0) isw(i) = 1
         end if
         vec(i) = AXSOUT(isub(i)) - AXSSTO(isub(i))
         pt(i)  = AXSSTO(isub(i))
  110 continue
      if (isw(1)+isw(2)+isw(3) .eq. 0) go to 8000
c
c...Get intersection points with all coordinate planes
c
      call unitvc (vec,unv)
      delt  = dsqrt(ndot(vec,vec))
      do 210 i=1,3,1
         if (isw(i) .eq. 1) then
            call plnint (pt,unv,planes(i*4-3),ptin(i*3-2),ier)
            if (ier .eq. 1) then
               isw(i) = 0
            else
               rd(i) = ndist(ptin(i*3-2),pt)
               if (rd(i) .ge. delt) isw(i) = 0
            end if
         end if
  210 continue
c
c...sort points by distance from start point
c
      ix    = 1
  300 rnum  = 1.d8
      n     = 0
      do 310 i=1,3,1
         if (isw(i) .eq. 1) then
            if (rd(i) .lt. rnum) then
               is(ix) = i
               rnum = rd(i)
               n    = i
            end if
         end if
  310 continue
      if (n .gt. 0) isw(n) = 0
      if (isw(1)+isw(2)+isw(3) .ne. 0) then
         ix = ix + 1
         go to 300
      end if
c
c...output points on boundaries
c
      if (ix .gt. 0) call pshaxs (AXSOUT,TLVEC)
      do 410 i=ix,1,-1
         n = is(i)*3-2
         AXSOUT(isub(1)) = ptin(n)
         AXSOUT(isub(2)) = ptin(n+1)
         AXSOUT(isub(3)) = ptin(n+2)
         if (i .ne. ix) call pshaxs (AXSOUT,TLVEC)
  410 continue
      call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
      call whchax (AXSOUT,kaxsw,kcnt)
      kfl   = ix
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  maccel (kaxsw,kcnt,kfl)
c
c   FUNCTION:  This routine outputs post generated acceleration spans.
c
c   INPUT:  kaxsw   I*4  D10 -  Array of switches that show which axes
c                               are waiting to be output.  A value of 1
c                               signifies this axis needs to be output.
c
c           kcnt    I*4  D1  -  Number of axes waiting to be output.
c
c           kfl     I*4  D1  -  Should be set to 0 on initial call.
c                               'kfl' is also returned with a value of
c                               0 and 1.  1 = the motion block was
c                               broken up, this routine should be called
c                               again immediately after the returned
c                               motion block is output.
c
c   OUTPUT: kaxsw   I*4  D10 -  See INPUT.
c
c           kcnt    I*4  D1  -  See INPUT.
c
c           kfl     I*4  D1  -  See INPUT.
c
c***********************************************************************
c
      subroutine maccel (kaxsw,kcnt,kfl)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(0271)), (ISCIRC,KPOSMP(1238))
      equivalence (SLWFL ,KPOSMP(1701)), (ISLWDN,KPOSMP(1721))
      equivalence (ISLWFD,KPOSMP(1723)), (IACCFD,KPOSMP(1744))
      equivalence (IACCFL,KPOSMP(1745)), (IRAP  ,KPOSMP(3199))
      equivalence (ISBSPL,KPOSMP(4085))
c
      integer*4 SLWFL(5),ISLWDN,ISLWFD,ISCIRC,ICYCSW(5),
     1          ISBSPL,IRAP,IACCFL(2),IACCFD
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (MOVDIS,POSMAP(1570))
      equivalence (AXSDIS,POSMAP(1574)), (SLWFD ,POSMAP(2471))
      equivalence (FMVTIM,POSMAP(3553)), (OFEED ,POSMAP(3560))
      equivalence (ACCFED,POSMAP(4291)), (ACCFD ,POSMAP(4292))
      equivalence (MAXVEL,POSMAP(4293)), (AXSVEL,POSMAP(4294))
      equivalence (ACCSTP,POSMAP(4295)), (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),ROTANG(20,2),AXSSTO(10),
     1       TLVEC(3),SLWFD,ACCFED,AXSDIS(10),FMVTIM,MOVDIS(4),
     2       OFEED(6),MAXVEL,AXSVEL,ACCSTP(2),ACCFD
c
      integer*4 kaxsw(10),kcnt,kfl
c
      integer*4 i
c
      real*8 fdif,vtim,ratio,rvec,step
c
c...Are acceleration blocks supported
c
      if (IACCFL(1) .ne. 1) go to 8000
c
c...Second time here
c...Pop previous position from stack
c
      if (kfl .eq. 1) then
          kfl    = 0
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,kaxsw,kcnt)
      endif
c
c...Calculate machining speeds
c
      call fedctl (kaxsw)
      call mchdis (0)
      call mchtim (AXSOUT,kaxsw,kcnt,0)
c
c...Are acceleration blocks active
c
      if (IACCFL(2) .eq. 1 .and. ISCIRC .eq. 0 .and.
     1    ICYCSW(1) .eq. 0 .and. ISBSPL .ne. 1 .and. IRAP .eq. 0) then
c
c...Calculate difference in feedrates
c...If feedrate is the same, then do nothing
c
          fdif   = OFEED(2) - ACCFED
          if (fdif .le. ACCSTP(1)) go to 8000
c
c...Calculate time it will take
c...to get to acceleration limit
c
          vtim   = AXSVEL / MAXVEL / 60.
cc          if (FMVTIM .lt. vtim) go to 8000
          ratio  = vtim / FMVTIM
c
c...Calculate new feed rate
c
          ACCFD  = ACCFED + fdif * vtim*60.
          if (ACCFD .lt. ACCFED+ACCSTP(1)) ACCFD = ACCFED + ACCSTP(1)
          if (ACCFD .gt. ACCFED+ACCSTP(2)) ACCFD = ACCFED + ACCSTP(2)
          step   = vtim * ACCFD
          ratio  = step   / MOVDIS(2)
          IACCFD = 1
c
c...Output calculated span at acceleration feed rate
c
          if (ratio .lt. .95) then
              kfl    = 1
              call pshaxs (AXSOUT,TLVEC)
              do 500 i=1,10,1
                  if (kaxsw(i) .eq. 1) then
                      rvec   = AXSOUT(i) - AXSSTO(i)
                      AXSOUT(i) = AXSSTO(i) + rvec * ratio
                  endif
  500         continue
c
c......Set up current position arrays
c
              call alladr (AXSOUT,LINAXS,MCHNUM,ROTANG,TLVEC,5,1)
              call whchax (AXSOUT,kaxsw,kcnt)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
