c
c***********************************************************************
c
c   FILE NAME:  cutctl
c   CONTAINS:
c               cutctl  cutang  cutcir  cutck   cutmax  cutout  cutpos
c               cutvec
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        cutctl.f , 25.5
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 10:11:34
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  cutctl
c
c   FUNCTION:  This is the controlling routine for calculating cutcom
c              directional output.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutctl
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251)), (ICUTDO,KPOSMP(3301))
c
      integer*4 CUTCFL(20),ICUTDO(15)
c
      integer*4 is(3),icir,nstp,dstp(6)
c
      real*8 rmch(3,3),rvec(3,2),rang(2)
c
      data nstp /6/, dstp /1,2,3,1007,1054,1055/
c
c...Calculate previous, current & next positions
c
      if (ICUTDO(3) .ne. 3) then
          call cutpos (is,rmch,icir,dstp,nstp,1,CUTCFL(1))
c
c...Calculate deltas & angle
c
          call cutang (is,rmch,rvec,rang)
c
c...Check for illegal move
c
          call cutck (is,rvec(1,1),rang(1))
      endif
c
c...Calculate cutcom vectors
c
      call cutvec (is,rmch,rvec,rang,icir)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutang (ks,gmch,gvec,gang)
c
c   FUNCTION:  This routine calculates a unit directional vector and
c              angle for both the current move and next move.  The angle
c              of the next move in relationship to the angle of the
c              current move is also calculated.
c              exiting a circle or the point going to when entering a
c              circle; based on the tangency point of the current
c              position.
c
c   INPUT:  ks      I*4  D3   -  Subscript pointer to 'gmch' set up for
c                                the active cutcom plan.
c
c           gmch    R*8  D3.3 -  The previous, current and next machine
c                                positions.
c
c   OUTPUT: gvec    R*8  D3.2 -  Directional vector for the two axis in
c                                the current cutcom plan.  This vector
c                                may or may not be unitized, depending
c                                on cutcom options.  The non-cutcom plan
c                                axis vector is always the actual dis-
c                                tance moved.
c
c           gang    R*8  D2   -  (1) = Angle of current move, (2) =
c                                angle of second move.
c
c***********************************************************************
c
      subroutine cutang (ks,gmch,gvec,gang)
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251))
c
      integer*4 CUTCFL(20)
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 ks(3)
c
      real*8 gmch(3,3),gvec(3,2),gang(2)
c
      integer*4 i,j
c
      real*8 vec,dd(3)
c
c...Calculate direction of each axis
c
      do 500 i=1,2,1
          do 100 j=1,3,1
              gvec(j,i) = gmch(j,i+1) - gmch(j,i)
              call dpoint (gvec(j,i),gvec(j,i),4)
  100     continue
          dd(1) = gvec(ks(1),i)
          dd(2) = gvec(ks(2),i)
          dd(3) = gvec(ks(3),i)
          if (CUTCFL(16).ne.1) dd(3) = 0.d0
          vec    = dsqrt(dd(1)**2 + dd(2)**2 + dd(3)**2)
c
c......Unitize vector
c
          if (vec .ne. 0.) then
              if ((CUTCFL(1) .ne. 2 .and. CUTCFL(1) .ne. 3) .or.
     1            CUTCFL(14) .eq. 1) then
                  dd(1) = dd(1) / vec
                  dd(2) = dd(2) / vec
                  dd(3) = dd(3) / vec
              endif
              gvec(ks(1),i) = dd(1)
              gvec(ks(2),i) = dd(2)
              gvec(ks(3),i) = dd(3)
c
c......Calculate move angle
c
              gang(i) = dacos(dd(1)) * RAD
              if (dd(2) .lt. 0.) gang(i) = 360.d0 - gang(i)
c
c......No move
c......Set vector & angle to 0
c
          else
              gvec(ks(1),i) = 0.
              gvec(ks(2),i) = 0.
              gvec(ks(3),i) = 0.
              gang(i) = 0.
          endif
  500 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutcir (ks,gprm,kprm,gmch,kfl,kfrm)
c
c   FUNCTION:  This routine calculates the point coming from when
c              exiting a circle or the point going to when entering a
c              circle; based on the tangency point of the current
c              position.
c
c   INPUT:  ks      I*4  D3   -  Subscript pointer to 'gmch' set up for
c                                the active cutcom plan.
c
c           gprm    R*8  D25  -  Input circle parameters (same format as
c                                RCIPRM).
c
c           kprm    I*4  D8   -  Input circle parameters (same format as
c                                ICIPRM).
c
c           gmch    R*8  D3.3 -  'gmch(n,2)' = current position.
c
c           kfl     I*4  D1   -  1 = Calculate point coming from.  3 =
c                                Calculate point going to.
c
c           kfrm    I*4  D1   -  3 = Calculates point coming from (when
c                                'kfl' = 1) based on circle entry point.
c                                Any other value uses circle exit point.
c                                Based on CUTCFL(1) flag.
c
c   OUTPUT: gmch    R*8  D3.3 -  'gmch(n,1)' = point coming from when kfl
c                                = 1.  'gmch(n,3)' = point going to when
c                                kfl = 3.
c
c***********************************************************************
c
      subroutine cutcir (ks,gprm,kprm,gmch,kfl,kfrm)
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251))
c
      integer*4 CUTCFL(20)
c
      equivalence (STONUM,POSMAP(1387))
c
      real*8 STONUM(3,4)
c
      integer*4 ks(3),kprm(8),kfl,kfrm
c
      real*8 gprm(25),gmch(3,3)
c
      integer*4 is
c
      real*8 d1,d2,vec,xpt(2),ypt(2)
c
c...Calculate vector from current position
c...to the center point of the circle
c
c......Parallel to current move uses entry point
c
      if (kfrm .eq. 3 .and. kfl .eq. 1) then
          d1    = gprm(ks(1)) - STONUM(ks(1),3)
          d2    = gprm(ks(2)) - STONUM(ks(2),3)
c
c......All others use exit point
c
      else
          d1     = gprm(ks(1)) - gprm(ks(1)+7)
          d2     = gprm(ks(2)) - gprm(ks(2)+7)
      endif
c
c......Unitize vector
c
      if ((CUTCFL(1) .ne. 2 .and. CUTCFL(1) .ne. 3) .or.
     1    CUTCFL(14) .eq. 1) then
          vec    = dsqrt(d1**2 + d2**2)
          d1     = d1     / vec
          d2     = d2     / vec
      endif
c
c...Calculate tangency points
c
      xpt(1) = gmch(ks(1),2) - d2
      xpt(2) = gmch(ks(1),2) + d2
      ypt(1) = gmch(ks(2),2) + d1
      ypt(2) = gmch(ks(2),2) - d1
c
c...Use direction of move to determine
c...which tangency point to use
c
      is     = 2
c
c...ZXPLAN
c
      if (ks(3) .eq. 2) then
          if ((kfl .eq. 1 .and. kprm(5) .eq. 1) .or.
     1        (kfl .eq. 3 .and. kprm(5) .eq. 2)) is = 1
c
c...XYPLAN & YZPLAN
c
      else
          if ((kfl .eq. 1 .and. kprm(5) .eq. 2) .or.
     1        (kfl .eq. 3 .and. kprm(5) .eq. 1)) is = 1
      endif
c
c...Assign point
c
      gmch(ks(1),kfl) = xpt(is)
      gmch(ks(2),kfl) = ypt(is)
      gmch(ks(3),kfl) = gmch(ks(3),2)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutck (ks,gvec,gang)
c
c   FUNCTION:  This routine checks for the following cutcom errors:
c
c                   1) Move in non-cutcom axis only.
c                   2) Angular change too great.
c
c   INPUT:  ks      I*4  D3   -  Subscript pointer to 'gmch' set up for
c                                the active cutcom plan.
c
c           gvec    R*8  D3   -  Directional vector of current move.
c
c           gang    R*8  D1   -  Angle of current move.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutck (ks,gvec,gang)
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251)), (ICUTDO,KPOSMP(3301))
c
      integer*4 CUTCFL(20),ICUTDO(15)
c
      equivalence (CUTCVR,POSMAP(2402)), (CTLANG,POSMAP(2445))
c
      real*8 CUTCVR(8),CTLANG
c
      integer*4 ks(3)
c
      real*8 gvec(3),gang
c
      real*8 rdif
c
      character*10 lbuf
      character*80 msg
c
c...Check for move in non-cutcom plane
c
      if (CUTCFL(10) .eq. 1 .and. gvec(ks(1)) .eq. 0. .and.
     1    gvec(ks(2)) .eq. 0. .and. gvec(ks(3)) .ne. 0.)
     2        call psterr (2,'CUTMOV',' ',-1)
c
c...Check for too large of an
c...angular change
c
      if (ICUTDO(1) .eq. 1) CTLANG = gang
      if (gang .ne. 0. .or. gvec(1) .ne. 0. .or. gvec(2) .ne. 0 .or.
     1        gvec(3) .ne. 0) then
          rdif   = dabs(gang-CTLANG)
          if (rdif .gt. 180.) rdif = 360.d0 - rdif
          if (rdif .gt. CUTCVR(1)) then
              call dpoint (rdif,rdif,4)
              call perrst ('CUTANG1',1,msg,0,rdif,lbuf,2)
              call perrst (msg,2,msg,0,CUTCVR(1),lbuf,2)
              call psterr (2,'CUTANG',msg,-1)
          endif
      endif
      CTLANG = gang
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutmax (gval)
c
c   FUNCTION:  This routine checks the cutcom vector component against
c              the maximum value allowed and outputs an error message
c              if it exceeds this value.
c
c   INPUT:  gval    R*8  D1   -  Cutcom vector component.
c
c   OUTPUT: gval    R*8  D1   -  Will be set to the maximum value if it
c                                exceeds it.
c
c***********************************************************************
c
      subroutine cutmax (gval)
c
      include 'post.inc'
c
      equivalence (CUTCVR,POSMAP(2402))
c
      real*8 CUTCVR(8)
c
      real*8 gval
c
      character*10 lbuf
      character*80 msg
c
c......Check for maximum vector values
c
      if (dabs(gval) .gt. CUTCVR(2)) then
          call dpoint (gval,gval,4)
          call perrst ('CUTVEC1',1,msg,0,gval,lbuf,2)
          call perrst (msg,2,msg,0,CUTCVR(2),lbuf,2)
          call psterr (1,'CUTVEC',msg,-1)
          gval   = CUTCVR(2) * (gval/dabs(gval))
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cutout (kfl)
c
c   FUNCTION:  This routine outputs cutter compensation directional
c              vector codes.
c
c   INPUT:  kfl     I*4  D10  -  1 = This axis is waiting to be output.
c
c   OUTPUT: kfl     I*4  D10  -  'kfl' may be modified if the axis
c                                register must be output with its
c                                corresponding cutcom register.
c
c***********************************************************************
c
      subroutine cutout (kfl)
c
      include 'post.inc'
c
      equivalence (ACTLIN,KPOSMP(1205)), (ISCIRC,KPOSMP(1238))
      equivalence (CUTCFL,KPOSMP(3251))
      equivalence (CUTCCD,KPOSMP(3271)), (ICUTDO,KPOSMP(3301))
      equivalence (ICUTSW,KPOSMP(3316)), (ICTPSV,KPOSMP(3319))
c
      integer*4 ACTLIN(3),CUTCFL(20),CUTCCD(30),ICUTDO(15),ICUTSW(3),
     1          ICTPSV(3),ISCIRC
c
      equivalence (CUTCVL,POSMAP(2410)), (CUTPRM,POSMAP(2446))
c
      real*8 CUTCVL(30),CUTPRM(3)
c
      integer*4 kfl(10)
c
      integer*4 i,is(2,3),inum,isw,iax(3),ifl
c
      real*8 rnum
c
      data is /1,2, 3,4, 5,6/
c
c...3-D cutter compensation
c
      if (ICUTDO(3) .eq. 3) then
          do 80 i=1,3,1
              if (ICUTSW(i) .eq. 1)
     1            call codout (CUTCCD(i+5),CUTPRM(i))
   80     continue
c
c...Normal to compensation direction
c...Cincinnati PQR's
c
      else if (CUTCFL(1) .eq. 1) then
c
c......Output cutcom vector codes
c
          do 100 i=1,3,1
              if (ICUTSW(i) .eq. 1) then
                  call codint (CUTCCD(i+5),CUTPRM(i),rnum,inum)
c
c......Cutcom code needs to be output
c
                  if (ICUTDO(1) .eq. 1) ICTPSV(i) = inum   + 1
                  if (inum .ne. ICTPSV(i) .or. kfl(is(1,i)) .eq. 1 .or.
     1                kfl(is(2,i)) .eq. 1) then
c
c.........Output vector code
c
                      call codout (CUTCCD(i+5),CUTPRM(i))
c
c.........Make sure axis code is
c.........output with cutcom code
c
                      if (kfl(is(1,i)) .eq. 0 .and. kfl(is(2,i)) .eq. 0)
     1                        then
                              kfl(is(ACTLIN(i),i)) = 1
                              iax(1) = 0
                              iax(2) = 0
                              iax(3) = 0
                              iax(i) = 1
                              call frcmot (2,4,iax)
                      endif
                  endif
              endif
              ICTPSV(i) = inum
  100     continue
c
c...Parallel to next move
c...Fanuc IJK's
c
      else if (CUTCFL(1) .eq. 2) then
c
c......Don't output cutcom vector codes with
c......with circular interpolation if specified
c
          if (CUTCFL(7) .ne. 1 .and. ISCIRC .eq. 1) go to 8000
          if (CUTCFL(8) .ne. 1 .and. ISCIRC .eq. 1 .and.
     1        ICUTDO(11) .eq. 1) go to 8000
c
c......Output cutcom vector codes
c
          do 500 i=1,3,1
              if (ICUTSW(i) .eq. 1) call codout (CUTCCD(i+5),CUTPRM(i))
  500     continue
          ICUTDO(11) = ISCIRC
c
c...Parallel to current move
c...in block by itself
c...Meldas Tome IJK's
c
      else if (CUTCFL(1) .eq. 3) then
c
c......Don't output cutcom vector codes on
c......first block when in block by themselves
c......Or with circular interpolation if specified
c
          if (CUTCFL(6) .ne. 1 .and. ICUTDO(1) .eq. 1) go to 8000
          if (CUTCFL(7) .ne. 1 .and. ISCIRC .eq. 1) go to 8000
          if (CUTCFL(8) .ne. 1 .and. ISCIRC .eq. 1 .and.
     1        ICUTDO(11) .eq. 1 .and. ICUTDO(1) .ne. 1) go to 8000
c
c......Output cutcom vector codes
c
          isw    = 0
          do 800 i=1,3,1
c
c.........Output vector code
c
              call cmpcod (CUTCCD(i+5),CUTPRM(i),0.d0,1,ifl)
              if (ifl .eq. 1) then
                  call codout (CUTCCD(i+5),CUTPRM(i))
                  isw    = 1
              endif
  800     continue
c
c......Output cutcom vector definition code
c
          if (isw .eq. 1 .and. ICUTDO(1) .ne. 1) then
              call codout (CUTCCD(5),CUTCVL(5))
              call clrbuf
          endif
          ICUTDO(11) = ISCIRC
c
c...Angular direction of next move
c...Sharnoa polar coordinate
c
      else if (CUTCFL(1) .eq. 4) then
          if (ICUTSW(1) .eq. 1) then
              rnum   = CUTPRM(1)
              if (rnum .gt. 180.d0) rnum = rnum - 360.d0
              call codout (CUTCCD(9),rnum)
          else if (ICUTSW(2) .eq. 1) then
              inum   = 10
              if (CUTPRM(2) .eq. 2.) inum = 11
              call codout (CUTCCD(inum),CUTCVL(inum))
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
c   SUBROUTINE:  cutpos (ks,gmch,kcir,kstp,knstp,ksd,kfl)
c
c   FUNCTION:  This routine calculates the current cutcom plane indices
c              and the position coming from, currently at and the posi-
c              tion going to.
c
c   INPUT:  kstp    I*4  Dn   -  Array of major word values that con-
c                                stitute an end-of-sequence.
c
c           knstp   I*4  D1   -  Number of entries in 'kstp'.
c
c           ksd     I*4  D1   -  1 = Calculate 3rd point using the cur-
c                                rent direction when a EOS post word is
c                                encountered.  -1 = Use 180 degrees to
c                                current direction for 3rd point.
c
c           kfl     I*4  D1   -  0 or 3 = Do not need 3rd point.
c
c   OUTPUT: ks      I*4  D3   -  Subscript pointer to 'gmch' set up for
c                                the active cutcom plan.
c
c           gmch    R*8  D3.3 -  (n,1) = point coming from, (n,2) = cur-
c                                rent position, (n,3) = point going to.
c
c           kcir    I*4  D1   -  0 = Next move is not a circle, 1 = Next
c                                move is CLW circular motion, 2 = Next
c                                move is CCLW circular motion.
c
c***********************************************************************
c
      subroutine cutpos (ks,gmch,kcir,kstp,knstp,ksd,kfl)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (ICIPRM,KPOSMP(1230)), (ISCIRC,KPOSMP(1238))
      equivalence (MCHPLN,KPOSMP(1261))
      equivalence (NEXCIR,KPOSMP(1275))
      equivalence (NOPIVS,KPOSMP(1282))
      equivalence (NOTOOL,KPOSMP(1802))
      equivalence (CUTCFL,KPOSMP(3251)), (ICUTDO,KPOSMP(3301))
c
      integer*4 ITYPE,ISUBT,MXCL,NPT,ICIPRM(8),ISCIRC,NEXCIR,MCHPLN
      integer*4 CUTCFL(20),ICUTDO(15),NOPIVS,NOTOOL
c
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (MCHNUM,POSMAP(1287))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387))
      equivalence (ROTANG,POSMAP(5173))
      equivalence (ROTBAS,POSMAP(1435))
      equivalence (RCIPRM,POSMAP(2049)), (CIRBPP,POSMAP(4845))
c
      real*8 CLPT(240),CIRBUF(7),MCHNUM(3,4),VECSAV(3),STONUM(3,4),
     1       ROTBAS(4),RCIPRM(25),CIRBPP(7),TLVEC(3),ROTANG(20,2)
c
      integer*4 ks(3),kcir,kfl,kstp(10),knstp,ksd
c
      real*8 gmch(3,3)
c
      integer*4 ixs(9),inc,ierr,isw,icpr(8),i,hldnpv,hldntl
c
      real*8 rmch(3,4),tv(3),laxs(6),raxs(10),rang(20,2),rcpr(25),rnum1,
     1       rnum2
c
      data ixs /1,1,2, 2,3,3, 3,2,1/

      hldnpv = NOPIVS
      hldntl = NOTOOL
c
c...Get machining plan indices
c
      kcir  = 0
      ks(1) = ixs(ICUTDO(4))
      ks(2) = ixs(ICUTDO(4)+3)
      ks(3) = ixs(ICUTDO(4)+6)
c
c...Store current position
c
      if (CUTCFL(16) .eq. 1) then
        NOPIVS = 2
        NOTOOL = 2
        call ptrtad(MCHNUM(1,2),gmch(1,2),ROTANG,0)
        NOPIVS = hldnpv
        NOTOOL = hldntl
      else
        gmch(1,2) = MCHNUM(1,3)
        gmch(2,2) = MCHNUM(2,3)
        gmch(3,2) = MCHNUM(3,3)
      endif
c
c...Store previous position
c
      if (ISCIRC .eq. 1) then
          call cutcir (ks,RCIPRM,ICIPRM,gmch,1,kfl)
      else
          if (CUTCFL(16) .eq. 1) then
            NOPIVS = 2
            NOTOOL = 2
            call ptrtad(STONUM(1,2),gmch,ROTANG,1)
            NOPIVS = hldnpv
            NOTOOL = hldntl
          else
            gmch(1,1) = STONUM(1,3)
            gmch(2,1) = STONUM(2,3)
            gmch(3,1) = STONUM(3,3)
          endif
      endif
      if (kfl .eq. 0) go to 8000
c
c...Calculate next position
c......Buffered circular motion
c
      if (NEXCIR .eq. 2) then
          call cutcir (ks,RCIPRM,ICIPRM,gmch,3,kfl)
          kcir   = ICIPRM(5)
          go to 8000
      endif
c
c......Get next cl record
c
      isw    = 0
  100 call clsamp (isw)
  150 isw    = 1
c
c.........Post word
c.........Check for command that siginifies
c.........the end of CUTCOM
c
      if (ITYPE .eq. 2000 .or. ITYPE .eq. 14000) then
          do 200 i=1,knstp,1
              if (ISUBT .eq. kstp(i) .or. ITYPE .eq. 14000) then
                  gmch(1,3) = gmch(1,2) + (gmch(1,2)-gmch(1,1)) * ksd
                  gmch(2,3) = gmch(2,2) + (gmch(2,2)-gmch(2,1)) * ksd
                  gmch(3,3) = gmch(3,2) + (gmch(3,2)-gmch(3,1)) * ksd
                  go to 1000
              endif
  200     continue
          go to 100
c
c.........Circular record
c
      else if (ITYPE .eq. 3000) then
          call clsamp (1)
          if (ITYPE .ne. 5000) go to 150
c
c............Check for valid circle
c
          do 300 i=1,3,1
              rmch(i,1) = STONUM(i,3)
              STONUM(i,3) = MCHNUM(i,3)
  300     continue
          call cirdir (CIRBUF,CIRBPP,rcpr,icpr,0,ierr)
          STONUM(1,3) = rmch(1,1)
          STONUM(2,3) = rmch(2,1)
          STONUM(3,3) = rmch(3,1)
          if (ierr .ne. 0 .or. icpr(3) .ne. MCHPLN) go to 150
c
c............Valid circle
c............Calculate tangency point
c
          call cutcir (ks,rcpr,icpr,gmch,3,kfl)
          kcir   = icpr(5)
          go to 1000
c
c.........Motion record
c
      else if (ITYPE .eq. 5000) then
          inc    = 1
c
c............Adjust point to rotaries
c
  500     rmch(1,2) = CLPT(inc)
          rmch(2,2) = CLPT(inc+1)
          rmch(3,2) = CLPT(inc+2)
          if (NPT .eq. 3) then
              tv(1) = VECSAV(1)
              tv(2) = VECSAV(2)
              tv(3) = VECSAV(3)
          else
              tv(1) = CLPT(inc+3)
              tv(2) = CLPT(inc+4)
              tv(3) = CLPT(inc+5)
          endif
          call tlaxis (rmch,tv,laxs,raxs,rang,ROTBAS,0,0,ierr)
c
c............Save points
c
          if (CUTCFL(16) .eq. 1) then
            NOPIVS = 2
            NOTOOL = 2
            call ptrtad(rmch(1,2),gmch(1,3),ROTANG,0)
            NOPIVS = hldnpv
            NOTOOL = hldntl
          else
            gmch(1,3) = rmch(1,3)
            gmch(2,3) = rmch(2,3)
            gmch(3,3) = rmch(3,3)
          endif
c
c............See if points are the same
c
          do 600 i=1,3,1
            call dpoint (gmch(i,2),rnum1,4)
            call dpoint (gmch(i,3),rnum2,4)
            if (rnum1 .ne. rnum2)  go to 1000
  600     continue
c
c............Points are the same
c............Assign next position
c
          inc    = inc    + NPT
          if (inc .gt. MXCL) go to 100
          go to 500
c
c.........Unrecognized record
c
      else
          go to 100
      endif
c
c......Reset clfile pointers
c
 1000 call clsamp (-1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cutvec (ks,gvec,gang,kcir)
c
c   FUNCTION:  This routine calculates the output vector components for
c              cutcom.
c
c   INPUT:  ks      I*4  D3   -  Subscript pointer to 'gmch' set up for
c                                the active cutcom plan.
c
c           gmch    R*8  D3.3 -  (1) = Previous point, (2) = Current
c                                point, (3) = Next point.
c
c           gvec    R*8  D3.2 -  (1) = Directional vector of current
c                                move, (2) = Next move.
c
c           gang    R*8  D2   -  (1) = Angle of current move, (2) = Next
c                                move.
c
c           kcir    I*4  D1   -  0 = Next move is not a circle, 1 = Next
c                                move is CLW circular motion, -1 = Next
c                                move is CCLW circular motion.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cutvec (ks,gmch,gvec,gang,kcir)
c
      include 'post.inc'
c
      equivalence (CUTCFL,KPOSMP(3251)), (ICUTDO,KPOSMP(3301))
      equivalence (ICUTSW,KPOSMP(3316)), (MOTEXP,KPOSMP(4211))
c
      integer*4 CUTCFL(20),ICUTDO(15),ICUTSW(3),MOTEXP
c
      equivalence (RAD   ,POSMAP(0002)), (CUTCVR,POSMAP(2402))
      equivalence (TLVEC ,POSMAP(1369))
      equivalence (RCUTDO,POSMAP(2229)), (CUTPRM,POSMAP(2446))
      equivalence (NMLVEC,POSMAP(4492))
c
      real*8 RCUTDO(10),CUTPRM(3),CUTCVR(8),RAD,TLVEC(3),NMLVEC(3)
c
      integer*4 ks(3),kcir
c
      real*8 gmch(3,3),gvec(3,2),gang(2),rsgn
c
      integer*4 i,icd,kerr
      logical lnull
c
      real*8 ango,angy,rnum(2), r1, r2, vec(3)
      real*8 v1(3), v2(3), v3(3), p1(3), p2(3), pl1(4)
      real*8 ndot,nmag
      real*8 svpt(3)
      save svpt
c
c...Initialize routine
c
      ICUTSW(1) = 0
      ICUTSW(2) = 0
      ICUTSW(3) = 0
c
c...Manual cutcom vectors
c
      if (ICUTDO(10) .eq. 1 .or. (ICUTDO(1) .eq. 1 .and.
     1    (ICUTDO(7) .eq. 1 .or. ICUTDO(8) .eq. 1 .or.
     2     ICUTDO(9) .eq. 1))) then
          do 100 i=1,3,1
              CUTPRM(i) = RCUTDO(i+1)
              ICUTSW(i) = ICUTDO(i+6)
  100     continue
c
c...Tool end point
c...3-dimensional cutcom vectors
c
      else if (ICUTDO(3) .eq. 3) then
          do 110 i=1,3,1
              CUTPRM(i) = NMLVEC(i)
              ICUTSW(i) = 1
  110     continue
c
c...Normal to compensation direction
c...Cincinnati PQR's
c
      else if (CUTCFL(1) .eq. 1) then
          rnum(1) = 180.d0 + gang(1)
          if (rnum(1) .gt. 360.d0) rnum(1) = rnum(1) - 360.d0
          rnum(2) = gang(2)
          icd    = ICUTDO(3)
c
c......First vector is PERPTO check surface
c
          if (ICUTDO(1) .eq. 1 .and. ICUTDO(5) .eq. 2) then
              rnum(1)  = gang(1) + 90.
              if (rnum(1) .gt. 360.d0) rnum(1) = rnum(1) - 360.d0
              rnum(2)  = rnum(1)
              icd    = 1
          endif
c
c... 3-D cutter compensation
c
          if (CUTCFL(16) .eq. 1) then
            rsgn = 1.0d0
c
c...Project first vector into plane of tool.
c
            call crosvc (TLVEC,gvec,v3)
            call crosvc (v3,TLVEC,v1)
            r1 = nmag(v1)
c
c...If first vector is null and this is not first output after
c...cutcom is turned on, use vector from saved point to current point.
c
            if (ICUTDO(1).eq.0 .and. r1.lt.0.0001d0) then
              call vcmnvc(gmch(1,2),svpt,v1)
              call crosvc (TLVEC,v1,v3)
              call crosvc (v3,TLVEC,v1)
              r1 = nmag(v1)
            else
              call copyn(gmch(1,1),svpt,3)
            endif
            call unitvc (v1, v1)
c
c...Project second vector into plane of tool.
c
            call crosvc (TLVEC,gvec(1,2),v3)
            call crosvc (v3,TLVEC,v2)
            r2 = nmag(v2)
            call unitvc (v2, v2)
c
c...Subtract vectors to get bisecting vector
c
            call vcmnvc (v1, v2, v3)

            lnull = nmag(v3) .lt. 0.001d0 .or.
     x          r1.lt.0.0001d0 .or. r2.lt.0.0001d0
c
c...Extend bisecting vector to plane parallel to first vector
c
            if (.not. lnull) then
              call crosvc (v1,TLVEC,pl1)
              call vcplvc(gmch(1,2),pl1,p1,1.d0)
              pl1(4) = ndot(pl1,p1)
              call unitvc (v3, v3)
              call plnint(gmch(1,2),v3,pl1,p2,kerr)
            endif
c
c...If vectors are null or nearly parallel, or could not extend
c...bisecting vector, use cross with tlaxis.
c
            if (lnull .or. kerr.eq.1) then
              call crosvc (v1,TLVEC,v3)
              if (nmag(v3) .lt. 0.0001d0) then
                call crosvc (v2,TLVEC,v3)
                if (nmag(v3) .lt. 0.0001d0) goto 8000
              endif
              call unitvc (v3, v3)
            else
              call vcmnvc(p2,gmch(1,2),v3)
            endif
            if (ICUTDO(3) .eq. 1) rsgn = -1.0d0
c
c... Alternative method using DS vector on expanded CL records
c... Vectors produced at the end of the move appear to be bad.
c
c            if (MOTEXP.eq.1) then
c              call crosvc (TLVEC,DSVEC,v2)
c              call crosvc (v2,TLVEC,v1)
c              r1 = nmag(v1)
c              if (r1.gt.0.0d0) then
c                call unitvc(v1, v3)
c                call crosvc (CLFWD,TLVEC,v2)
c                r1 = ndot(v1,v2)
c                if (r1.lt.0.0d0) then
c                  if (ICUTDO(3) .eq. 2) rsgn = -1.0d0
c                else
c                  if (ICUTDO(3) .eq. 1) rsgn = -1.0d0
c                endif
c              endif
c            endif
            call vctmsc(v3, rsgn, vec)
            if (CUTCFL(14) .eq. 1) call unitvc (vec,vec)
            call cutmax (vec(3))
            CUTPRM(ks(3)) = vec(3)
            ICUTSW(ks(3)) = 1
          else
c
c......Calculate 2-D cutcom vector
c
            ango   = rnum(1) - rnum(2)
            if (ango .gt. -.999995 .and. ango .lt. .000005) ango = 180.
            if (ango .lt. 0.) ango = ango + 360.d0
            ango   = ango   / 2.
            angy   = gang(2) + ango
            rsgn   = 1.
            if (icd .eq. 2) rsgn = -1.
            r1     = rsgn / dabs(dsin(ango/RAD))
            vec(1) = r1 * dcos(angy/RAD)
            vec(2) = r1 * dsin(angy/RAD)
            if (CUTCFL(14) .eq. 1) call unitvc (vec,vec)
          endif
c
c......Check for maximum vector values
c
          call cutmax (vec(1))
          call cutmax (vec(2))
          CUTPRM(ks(1)) = vec(1)
          CUTPRM(ks(2)) = vec(2)
          ICUTSW(ks(1)) = 1
          ICUTSW(ks(2)) = 1
c
c...Parallel to next move
c...Fanuc IJK's
c
      else if (CUTCFL(1) .eq. 2) then
          CUTPRM(ks(1)) = gvec(ks(1),2)
          CUTPRM(ks(2)) = gvec(ks(2),2)
          ICUTSW(ks(1)) = 1
          ICUTSW(ks(2)) = 1
c
c......Check for maximum vector values
c
          call cutmax (CUTPRM(ks(1)))
          call cutmax (CUTPRM(ks(2)))
c
c...Parallel to current move
c...Meldas Tome IJK's
c
      else if (CUTCFL(1) .eq. 3) then
          CUTPRM(ks(1)) = gvec(ks(1),1)
          CUTPRM(ks(2)) = gvec(ks(2),1)
          ICUTSW(ks(1)) = 1
          ICUTSW(ks(2)) = 1
c
c......Check for maximum vector values
c
          call cutmax (CUTPRM(ks(1)))
          call cutmax (CUTPRM(ks(2)))
c
c...Angular direction of next move
c...Sharnoa polar coordinate
c
      else if (CUTCFL(1) .eq. 4) then
          if (kcir .eq. 0) then
              CUTPRM(1) = gang(2)
              ICUTSW(1) = 1
          else
              CUTPRM(2) = kcir
              ICUTSW(2) = 1
          endif
      endif
c
c...End of routine
c
 8000 return
      end
