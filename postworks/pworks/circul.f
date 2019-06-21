c
c***********************************************************************
c
c   FILE NAME:  circul
c   CONTAINS:
c               circul  ciradj  cirbnd  circk   cirdir  cirhel  cirijk
c               cirout  cirpol  cirpt   cirqad
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        circul.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/12/15 , 17:37:12
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  circul (klook,kwrn,kmac)
c
c   FUNCTION:  This is the controlling routine for circular interpola-
c              tion and helical records.
c
c   INPUT:  klook   I*4  D1  -  Look ahead flag.  0 = End of look ahead,
c                               1 = Look for next continuation or circle
c                               record, 2 = Circle record encountered
c                               during look ahead, check for following
c                               motion record.
c
c           kwrn    I*4  D1  -  1 = Output warning if circle is not
c                               in correct plane, 0 = don't output
c                               warning.
c
c           kmac    I*4  D1  -  1 = Process circular record for Macro
c                               call.
c
c   OUTPUT: klook   I*4  D1  -  Updated look ahead flag.
c
c           kmac    I*4  D1  -  Set to -1 if an error occured creating
c                               circular record if it was set to 1 on
c                               entry.
c
c***********************************************************************
c
      subroutine circul (klook,kwrn,kmac)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (ICLMSV,KPOSMP(0151)), (ICYCSW,KPOSMP(0271))
      equivalence (IZIGON,KPOSMP(0370)), (IZIGAD,KPOSMP(0371))
      equivalence (ICIPRM,KPOSMP(1230)), (ISCIRC,KPOSMP(1238))
      equivalence (MCHPLN,KPOSMP(1261)), (ICRPFL,KPOSMP(1370))
      equivalence (IHELIX,KPOSMP(1392)), (ICRPLS,KPOSMP(1398))
      equivalence (ICIDLT,KPOSMP(1741)), (IRAP  ,KPOSMP(3199))
      equivalence (ICUTDO,KPOSMP(3301)), (MODROT,KPOSMP(4031))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 ITYPE,ISUBT,MXCL,NPT,ICIPRM(8),ISCIRC,ICRPFL,ICLMSV(20),
     1          ICUTDO(15),MCHPLN,ICYCSW(5),IRAP,IZIGON,IZIGAD,
     2          LTMODE,MTPDYN,MODROT,ICRPLS(3),ICIDLT,IHELIX
c
      equivalence (CLPT  ,POSMAP(0491)), (CIRBUF,POSMAP(0731))
      equivalence (STONUM,POSMAP(1387)), (CIRTOL,POSMAP(2201))
      equivalence (RCIPRM,POSMAP(2049)), (HELANG,POSMAP(2213))
      equivalence (CIRBPP,POSMAP(4845))
c
      real*8 CLPT(240),CIRBUF(7),RCIPRM(25),STONUM(3,4),CIRTOL(5),
     1       CIRBPP(7),HELANG(2)
c
      integer*4 klook,kwrn,kmac
c
      integer*4 ipt,inc,i,iprm(8),ierr,iwrn
c
      real*8 prm(25),rary(3),ndist
c
      character*80 msg
c
c...Initialize routine
c
      iwrn   = kwrn
      if (kmac .eq. 1) then
          iwrn = 0
          ICIPRM(6) = IHELIX
          RCIPRM(21) = HELANG(1)
      endif
c
c...Reset delta movements
c
      if (klook .eq. 0 .and. ICIDLT .eq. 1) call tmpdrs
c
      if (IZIGON .eq. 1) IZIGAD = 1
c
c...Returning from look ahead
c...Check for valid record
c
      if (klook .ne. 0) then
c
c......Motion record following
c......Circle record
c
          if (klook .eq. 2) then
              if (ITYPE .eq. 5000) then
c
c......Do not process circle if polar interpolation is on
c
                  if (MODROT .ne. 0) go to 9150
c
c.........Calculate circle parameters
c
                  do 100 i=1,3,1
                      rary(i) = STONUM(i,3)
                      STONUM(i,3) = RCIPRM(i+7)
  100             continue
                  call ptxfr (STONUM(1,3),STONUM(1,3),1)
                  call cirdir (CIRBUF,CIRBPP,prm,iprm,0,ierr)
                  STONUM(1,3) = rary(1)
                  STONUM(2,3) = rary(2)
                  STONUM(3,3) = rary(3)
c
c.........Verify circles are the same
c
                  if (iprm(3) .ne. ICIPRM(3) .or.
     1                iprm(5) .ne. ICIPRM(5)) ierr = 1
                  if (ierr .eq. 0) then
                      if (iprm(3) .ne. 0) then
                          do 150 i=1,2,1
                              call dpoint (prm(iprm(i)),rary(1),4)
                              call dpoint (RCIPRM(iprm(i)),rary(2),4)
                              if (rary(1) .ne. rary(2)) ierr = 1
  150                     continue
                      else
                          rary(1) = ndist(prm,RCIPRM)
                          if (rary(1) .gt. CIRTOL(1)) ierr = 1
                      endif
                  endif
                  if (dabs(prm(4)-RCIPRM(4)) .gt. CIRTOL(1)) ierr = 1
c
c............Circles are the same
c............Write listing record &
c............Process normally
c
                  if (ierr .eq. 0) then
                      ISUBT  = 6
                      if (kmac .eq. 0) then
                          call genlst (msg,ierr)
                          if (ierr .ne. 0) go to 9200
                      endif
                      go to 300
                  endif
              endif
c
c......Continuation record
c
          else
              if (ITYPE .eq. 5000 .and. ISUBT .eq. 6) then
                  if (kmac .eq. 0) then
                      call genlst (msg,ierr)
                      if (ierr .ne. 0) go to 9200
                  endif
                  go to 300
              endif
c
c......Circle record
c
              if (ITYPE .eq. 3000) then
                  klook  = 2
                  go to 8000
              endif
          endif
c
c......We cannot use the
c......look ahead record
c......Output circular record
c
          klook  = 0
          call clumrk(1)
          if (kmac .eq. 0) then
              call cirout (RCIPRM,ICIPRM)
              if (IRAP .ne. 0) call raprst
          endif
          go to 8000
      endif
c
c...Calculate center point &
c...circular direction
c
      if (MODROT .ne. 0) go to 9150
      call cirdir (CIRBUF,CIRBPP,RCIPRM,ICIPRM,iwrn,ierr)
      if (ierr .ne. 0) go to 9100
c     if (ICIPRM(3) .ne. 3 .and. ICRPFL .eq. 2) go to 9100
      if (ICRPFL .eq. 2 .and. ICIPRM(3) .ne. 0) then
          i    = ICIPRM(3) + 1
          if (i .gt. 3) i = 1
          if (ICRPLS(i) .ne. 1) go to 9100
      end if
c
c...Check for CUTCOM plane match
c
      if (ICUTDO(2) .eq. 1) then
          if (ICIPRM(3) .ne. MCHPLN .and. ICIPRM(3) .ne. 0) then
              if (kmac .eq. 0) call psterr (2,'CUTPLN',' ',-1)
              go to 9100
          endif
          if (ICUTDO(1) .eq. 1) call psterr (1,'CUTENT',' ',-1)
      endif
c
c...Check for active cycle
c
      if (ICYCSW(1) .ne. 0) then
          if (kmac .eq. 0) call psterr (2,'CYCCIR',' ',-1)
          go to 9100
      endif
c
c...Calculate delta ending angle
c
  300 call circk (RCIPRM,ICIPRM,inc,1,ierr)
      if (ierr .ne. 0) go to 9000
c
c...Determine if next record
c...is continuation record
c
      call clmark(1)
      klook  = 1
c
c...End of routine
c
 8000 IZIGAD = 2
      return
c
c...Error processing circular record
c...Output a portion of it as linear
c
 9000 ICLMSV(1) = -1
c
c......CIRCLE Macro is active
c......Just return error
c
      if (kmac .eq. 1) then
          kmac   = -1
c
c......Output circular motion &
c......Set up linear motion
c
      else if (inc .ne. 1 .or. klook .ne. 0) then
          call cirout (RCIPRM,ICIPRM)
          ipt    = MXCL
          MXCL   = (inc-1)/NPT
          call genlst (msg,ierr)
          MXCL   = ipt
          ipt    = 0
          do 9050 i=inc,MXCL*NPT,1
              ipt    = ipt    + 1
              CLPT(ipt) = CLPT(i)
 9050     continue
          MXCL   = ipt    / NPT
      endif
c
c...Output linear motion
c
 9100 ISCIRC = 0
      IZIGAD = 2
      kmac   = 0
      if (kmac .eq. 0) then
          call mocntl
          klook  = 0
      endif
      go to 8000
c
c...Polar interpolation error
c
 9150 call psterr (1,'CIRPOL1','CIRPOL2',-1)
      go to 9100
c
c...Error writing listing file
c
 9200 call errkil (msg,ierr)
      end
c
c***********************************************************************
c
c   SUBROUTINE:  ciradj (gprm,kprm)
c
c   FUNCTION:  This routine adjusts the final point to lie on a quadrant
c              boundary if it falls within 1 pulse of it and optionally
c              adjusts it to lie exactly on the circle.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine ciradj (gprm,kprm)
c
      include 'post.inc'
c
      equivalence (ICRADJ,KPOSMP(1371)), (ICMREG,KPOSMP(1393))
c
      integer*4 ICRADJ,ICMREG(3)
c
      equivalence (RAD   ,POSMAP(0002)), (CIRTOL,POSMAP(2201))
c
      real*8 RAD,CIRTOL(5)
c
      integer*4 kprm(8)
c
      real*8 gprm(25)
c
      integer*4 iary(6),inum1,inum2
c
      real*8 ang,rspt(2),rept(2),xyz(3),rnum,rsgn(2),rtmp(2),rcen(2),
     1       ndist,rvec(3),rval
c
      data rsgn /-1.,1./
c
c...Adjust final point to lie
c...on quadrant boundary if close
c
      rnum   = dmod(gprm(6),90.d0)
      if (rnum .gt. 45.) rnum = rnum - 90.
      ang    = gprm(6) - rnum
      if (ang .lt. 0.) ang = ang + 360.
      if (ang .gt. 360.) ang = ang - 360.
      call cirpt (gprm,kprm,ang,xyz)
c
c......Compare quadrant end points
c
      if (kprm(3) .eq. 0) then
         rval   = ndist(xyz,gprm(8))
         if (rval .gt. CIRTOL(1)) go to 200
      else
          call codint (ICMREG(kprm(1)),gprm(kprm(1)+7),rept(1),iary(1))
          call codint (ICMREG(kprm(2)),gprm(kprm(2)+7),rept(2),iary(2))
          call codint (ICMREG(kprm(1)),xyz(kprm(1)),rtmp(1),iary(3))
          call codint (ICMREG(kprm(2)),xyz(kprm(2)),rtmp(2),iary(4))
          inum1  = abs(iary(1)-iary(3))
          inum2  = abs(iary(2)-iary(4))
          if (inum1 .eq. 0) then
              if (inum2 .ne. 1) go to 200
          else if (inum1 .eq. 1) then
              if (inum2 .ne. 0) go to 200
          else
              go to 200
          endif
      endif
c
c......Use quadrant boundary point
c
      gprm(6) = ang
      gprm(7) = gprm(7) - rnum   * rsgn(kprm(5))
      gprm(8) = xyz(1)
      gprm(9) = xyz(2)
      gprm(10) = xyz(3)
      go to 8000
c
c...Adjust final point to lie
c...exactly on the circle relative to the center point
c...as formatted for output - vp 8/25/94
c
  200 continue
c
c...If 3-axis circle
c
      if (kprm(3) .eq. 0) then
         if (ICRADJ .eq. 1) then
             call vcmnvc (gprm(8),gprm(1),rvec)
             call unitvc (rvec,rvec)
             call vcplvc (gprm(1),rvec,gprm(8),gprm(4))
         endif
c
c......If start and end point are close, make them the same
c
         rnum   = ndist(gprm(11),gprm(8))
         if (rnum .le. CIRTOL(1)) then
             call copyn (gprm(11),gprm(8),3)
             go to 350
         endif
         go to 8000
      endif
c
c...2-axis circle
c
      call codint (ICMREG(kprm(1)),gprm(11),rspt(1),iary(3))
      call codint (ICMREG(kprm(2)),gprm(12),rspt(2),iary(4))
      if (ICRADJ .ne. 1) go to 300
      call codint (ICMREG(kprm(1)),gprm(kprm(1)),rcen(1),iary(5))
      call codint (ICMREG(kprm(2)),gprm(kprm(2)),rcen(2),iary(6))
      rtmp(1) = rspt(1) - rcen(1)
      rtmp(2) = rspt(2) - rcen(2)
      rnum    = dsqrt(rtmp(1)**2 + rtmp(2)**2)
c      rtmp(1) = rept(1) - rcen(1)
c      rtmp(2) = rept(2) - rcen(2)
c      rrad    = dsqrt(rtmp(1)**2 + rtmp(2)**2)
c      if (rrad .eq. rnum) go to 300
c      call codint (ICMREG(kprm(1)),rnum,rtmp(1),itmp1)
c      call codint (ICMREG(kprm(1)),rrad,rtmp(2),itmp2)
c      if (rtmp(1) .eq. rtmp(2)) goto 8000
c
c...vp 3/6/98 adjust end point using rounded radius
c...i.e. radius with machine control accuracy
c...set it in gprm(4) so if R code is output it is also rounded
c
      if (rnum .eq. gprm(4)) go to 300
      gprm(4) = rnum
      gprm(kprm(1)+7) = rcen(1) + rnum * dcos(gprm(6)/RAD)
      gprm(kprm(2)+7) = rcen(2) + rnum * dsin(gprm(6)/RAD)
      call codint (ICMREG(kprm(1)),gprm(kprm(1)+7),rept(1),iary(1))
      call codint (ICMREG(kprm(2)),gprm(kprm(2)+7),rept(2),iary(2))
c
c... If start and end point are close, make them the same
c
  300 continue
      inum1  = abs(iary(1)-iary(3))
      inum2  = abs(iary(2)-iary(4))
      if (inum1 .gt. 1 .or. inum2 .gt. 1) goto 8000
      gprm(kprm(1)+7) = gprm(11)
      gprm(kprm(2)+7) = gprm(12)
  350 gprm(6)  = gprm(5)
      rnum = gprm(7)
      inum1 = 0
      do while (rnum.gt.1.d0)
        rnum = rnum-360.d0
        inum1 = inum1+1
      enddo
      if (rnum.lt.-1.d0) goto 8000
      gprm(7) = 360.d0*inum1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirbnd (gprm,kprm,kquad,ktyp)
c
c   FUNCTION:  This routine calculates the current circular quadrant.
c              When the input angle is a quadrant boundary, the the ac-
c              tual quadrant depends on the direction (for starting
c              quadrant calcs) or on the starting quadrant (for ending
c              quadrant calcs).
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           ktyp    I*4  D1  -  1 = Calculate beginning quadrant.  2 =
c                               Calculate ending quadrant.
c
c   OUTPUT: kquad   I*4  D1  -  Returns circle quadrant of input angle.
c
c***********************************************************************
c
      subroutine cirbnd (gprm,kprm,kquad,ktyp)
c
      integer*4 kprm(8),ktyp,kquad
c
      real*8 gprm(25)
c
      real*8 rang
c
c...Calculate beginning quadrant
c
      if (ktyp .eq. 1) then
          rang   = gprm(5)
          if (rang .lt. 0.) rang = rang + 360.
          if (rang .gt. 360.) rang = rang - 360.
c
c......1st quadrant
c
          if (rang .le. 90.d0) then
              kquad  = 1
              if (rang .eq. 0. .and. kprm(5) .eq. 1) kquad = 4
              if (rang .eq. 90.d0 .and. kprm(5) .eq. 2) kquad = 2
c
c......2nd quadrant
c
          else if (rang .le. 180.d0) then
              kquad  = 2
              if (rang .eq. 180.d0 .and. kprm(5) .eq. 2) kquad = 3
c
c......3rd quadrant
c
          else if (rang .le. 270.d0) then
              kquad  = 3
              if (rang .eq. 270.d0 .and. kprm(5) .eq. 2) kquad = 4
c
c......4th quadrant
c
          else
              kquad  = 4
          endif
c
c...Calculate ending quadrant
c
      else
          rang   = gprm(6)
          if (rang .lt. 0.) rang = rang + 360.
          if (rang .gt. 360.) rang = rang - 360.
c
c......1st quadrant
c
          if (rang .le. 90.d0) then
              kquad  = 1
              if (rang .eq. 0. .and. kprm(5) .eq. 2) kquad = 4
              if (rang .eq. 90.d0 .and. kprm(5) .eq. 1) kquad = 2
c
c......2nd quadrant
c
          else if (rang .le. 180.d0) then
              kquad  = 2
              if (rang .eq. 180.d0 .and. kprm(5) .eq. 1) kquad = 3
c
c......3rd quadrant
c
          else if (rang .le. 270.d0) then
              kquad  = 3
              if (rang .eq. 270.d0 .and. kprm(5) .eq. 1) kquad = 4
c
c......4th quadrant
c
          else
              kquad  = 4
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
c   SUBROUTINE:  circk (gprm,kprm,kinc,kfl,kerr)
c
c   FUNCTION:  This routine checks the tolerancing on a circular inter-
c              polation record determines the ending quadrant and delta
c              angle.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kfl     I*4  D1  -  1 = Output error messages encountered
c                               in this routine.
c
c   OUTPUT: kinc    I*4  D1  -  Returns the pointer within the CLPT
c                               array of the point that an error occur-
c                               red on.
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine circk (gprm,kprm,kinc,kfl,kerr)
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (MOTREG,KPOSMP(0381)), (ICMREG,KPOSMP(1393))
      equivalence (ICIDLT,KPOSMP(1741))
      equivalence (MACHTP,KPOSMP(1201)), (IHELIX,KPOSMP(1392))
      equivalence (LTMODE,KPOSMP(4125))
c
      integer*4 MXCL,NPT,MOTREG(24),ICMREG(3),MACHTP,LTMODE,ICIDLT,
     1          IHELIX
c
      equivalence (RAD   ,POSMAP(0002)), (CLPT  ,POSMAP(0491))
      equivalence (PPTOLR,POSMAP(1274))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (CIRTOL,POSMAP(2201)), (ROTSTO,POSMAP(5213))
c
      real*8 RAD,CLPT(240),VECSAV(3),STONUM(3,4),CIRTOL(5),PPTOLR(10),
     1       TLVEC(3),ROTSTO(20,2),AXSOUT(10)
c
      integer*4 kerr,kfl,kinc,kprm(8)
c
      real*8 gprm(25)
c
      integer*4 i,inc,imrg(10),inum,isub(10),iend,ierr
c
      real*8 rmch(3),quad(2),rnum,rval,rtmp,v1(3),v2(3),buf(6),stn(3),
     1       tmch(3,4),tlin(6),trot(20,2),taxs(10),tmst(3,4),tlst(6),
     2       trst(20,2),tast(10),vec(3),ndist,angl2p
c
      character*2 lbuf
      character*80 msg
c
      data isub /1,3,5,7,9,11,13,16,19,22/
c
c...Initialize routine
c
      call ciptad1 (STONUM(1,2),stn,ROTSTO,buf(4))
      v1(3) = 0.
      v2(3) = 0.
c      v1(1) = gprm(kprm(1)+7) - gprm(kprm(1))
c      v1(2) = gprm(kprm(2)+7) - gprm(kprm(2))
c      call codint (ICMREG(kprm(1)),v1(1),v1(1),inum)
c      call codint (ICMREG(kprm(2)),v1(2),v1(2),inum)
c      rtmp  = dsqrt (v1(1)**2 + v1(2)**2)
      v1(1) = dcos(gprm(6)/RAD)
      v1(2) = dsin(gprm(6)/RAD)
      kerr   = 0
      do 50 i=1,10,1
          imrg(i) = MOTREG(isub(i))
   50 continue
c
c...Start loop
c
      kinc    = 1
      inc    = 1
      if (MXCL .ge. 7 .and. ICIDLT .ne. 1) inc = 3
      iend   = (MXCL-1) * NPT + 1
c
c...Save previous position
c...for delta distances
c
      if (ICIDLT .eq. 1) then
          call ptxfr (gprm(8),tmst(1,3),1)
          call alladj (tmst,tlst,tast,trst,3,5)
          call alladr (tast,tlst,tmst,trst,TLVEC,2,1)
      endif
c
c......Adjust point for tool axis
c
  100     call ptrtad (CLPT(kinc),rmch,ROTSTO(1,2),0)
          call clptpp1 (kinc,rmch,ROTSTO,buf(4))
c
c......Calculate delta movements
c
          if (ICIDLT .eq. 1) then
              call ptxfr (rmch,tmch(1,3),1)
              call cpyrot (ROTSTO,trot)
              call cpyrot (ROTSTO,trst)
              do 180 i=1,4,1
                  taxs(i+6) = AXSOUT(i+6)
                  tast(i+6) = AXSOUT(i+6)
  180         continue
              call alladj (tmch,tlin,taxs,trot,3,5)
              call alladr (taxs,tlin,tmch,trot,TLVEC,2,1)
              call tmpdlt (tmch,tlin,trot,taxs,tmst,tlst,trst,tast,1)
              call copyn (tmch,tmst,12)
              call copyn (tlin,tlst,6)
              call cpyrot (trot,trst)
              call copyn (taxs,tast,10)
          endif
c
c......Check for 3-axis move
c
c          call codint (ICMREG(kprm(3)),STONUM(kprm(3),3),quad(1),inum)
c          call codint (ICMREG(kprm(3)),rmch(kprm(3)),quad(2),inum)
c          rnum   = dabs(quad(1)-quad(2))
          if (kprm(3) .eq. 0) then
              call plndis (gprm(17),rmch,rnum)
              if (rnum .gt. PPTOLR(1)*2.d0) go to 9100
          else
              rnum   = dabs(stn(kprm(3))-rmch(kprm(3)))
c
c.........Added extra check
c.........as there are times when using an APT source file
c.........that the roundoff errors are greater
c.........so check against circular plane
c.........QAR 61396
c
              if (rnum .gt. PPTOLR(1)*2.d0) then
                  if (rnum .gt. PPTOLR(1)*3.5d0) goto 9100
                  if (ICIDLT .ne. 1) then
                      call ptxfr (rmch,tmch(1,3),1)
                      call alladr (AXSOUT,tlin,tmch,ROTSTO,TLVEC,2,1)
                  endif
                  call plnvpt (TLVEC,STONUM(1,2),buf,ierr)
                  call plndis (buf,tmch(1,2),rnum)
                  if (rnum .gt. PPTOLR(1)*2.d0) then
                      go to 9100
                  endif
              endif
              if (IHELIX .eq. 0) rmch(kprm(3)) = stn(kprm(3))
          endif
c
c......Check for rotary move
c
          if (NPT .eq. 6) then
             if (MACHTP .ne. 4 .or. (LTMODE .ne. 0 .and. LTMODE .ne. 2))
     1              then
                call betvec (VECSAV,CLPT(kinc+3),rnum)
                if (rnum .gt. .001) go to 9000
             end if
          end if
c             do 300 i=1,3,1
c                 call dpoint (VECSAV(i),quad(1),5)
c                 call dpoint (TLVEC(i),quad(2),5)
c                 if (quad(1) .ne. quad(2)) go to 9000
c 300         continue
c         endif
c
c......Check radius tolerance
c
          if (kprm(3) .eq. 0) then
              rtmp   = ndist(rmch,gprm)
          else
              quad(1) = rmch(kprm(1)) - gprm(kprm(1))
              quad(2) = rmch(kprm(2)) - gprm(kprm(2))
              call codint (ICMREG(kprm(1)),quad(1),quad(1),inum)
              call codint (ICMREG(kprm(2)),quad(2),quad(2),inum)
              rtmp    = dsqrt(quad(1)**2 + quad(2)**2)
          endif
          if (dabs(rtmp-gprm(4)) .gt. CIRTOL(1)) go to 9200
c          call codint (ICMREG(kprm(1)),rtmp,rtmp,inum)
c
c......Calculate ending angle
c

          rnum   = gprm(6)
c          rsgn   = dabs(quad(1))
c          if (rsgn .gt. gprm(4)) quad(1) = gprm(4) * (quad(1)/rsgn)
          if (kprm(3) .eq. 0) then
              call vcmnvc (rmch,gprm,vec)
              gprm(6) = angl2p (gprm(14),vec,gprm(17))
          else
              rval   = quad(1) / rtmp
              if (rval .gt. 1.0d0) rval = 1.0
              if (rval .lt. -1.0d0) rval = -1.0
              gprm(6) = dacos(rval) * RAD
              if (quad(2) .lt. 0.) gprm(6) = 360. - gprm(6)
          endif
c
c......Calculate delta angle
c
          if (kprm(3) .eq. 0) then
              if (kprm(5) .eq. 1) then
                  rval = gprm(5) - (gprm(6)-360)
              else
                  rval = gprm(6) - gprm(5)
              endif
              do while (rval .lt. gprm(7))
                  rval = rval + 360.
              enddo
              gprm(7) = rval
          else
              v2(1) = quad(1) / rtmp
              v2(2) = quad(2) / rtmp
              call betvec (v1,v2,rnum)
              gprm(7) = gprm(7) + rnum
              v1(1)  = v2(1)
              v1(2)  = v2(2)
          endif
c
c...Store end point
c
          gprm(8) = rmch(1)
          gprm(9) = rmch(2)
          gprm(10) = rmch(3)
c
c...End of loop
c
      if (kinc .eq. iend) then
          if (gprm(7) .eq. 0.0) goto 9500
          go to 8000
      endif
      kinc    = kinc    + inc * NPT
      if (kinc .gt. iend) kinc = iend
      go to 100
c
c...End of routine
c
 8000 if (kinc .ne. 1) kinc = kinc - inc * NPT
      return
c
c...Angular change
c
 9000 kerr   = 1
      if (kfl .eq. 1) call psterr (1,'CIRANG',' ',-1)
      go to 8000
c
c...Total delta angle = 0.0
c
 9500 kerr   = 1
      if (kfl .eq. 1) call psterr (1,'CIRPLN',' ',-1)
      go to 8000
c
c...3-axis move
c
 9100 kerr   = 1
      if (kfl .eq. 1) call psterr (1,'CIR3AX',' ',-1)
      go to 8000
c
c...Radius out of tolerance
c
 9200 kerr   = 1
      if (kfl .eq. 1) then
          call perrst ('CIRRAD',1,msg,0,rtmp,lbuf,2)
          call perrst (msg,2,msg,0,gprm(4),lbuf,2)
          call psterr (1,'CIRTOLR',msg,-1)
      endif
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirdir (gbuf,gprm,kprm,kfl,kerr)
c
c   FUNCTION:  This routine determines the Machining Plane, starting an-
c              gle, starting quadrant & direction of a circular inter-
c              polation record.
c
c   INPUT:  gbuf    R*8  D7  -  Circle record (Type 3000), XYZIJKR.
c
c           kfl     I*4  D1  -  1 = Output error messages encountered
c                               in this routine.
c
c   OUTPUT: gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cirdir (gbuf,gbpp,gprm,kprm,kfl,kerr)
c
      include 'post.inc'
c
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (MCHOPT,KPOSMP(0308)), (MACHTP,KPOSMP(1201))
      equivalence (LASTAB,KPOSMP(1260)), (IHELIX,KPOSMP(1392))
      equivalence (ICMREG,KPOSMP(1393)), (IRTWRK,KPOSMP(1506))
      equivalence (IC3AXF,KPOSMP(1747))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
c
      integer*4 MXCL,NPT,ICMREG(3),IRTWRK(20),LASTAB,
     -          MTPDYN,LTMODE,MACHTP,IC3AXF(10),IHELIX,MCHOPT(20)
c
      equivalence (RAD   ,POSMAP(0002)), (CLPT  ,POSMAP(0491))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (CIRTOL,POSMAP(2201)), (ROTSTO,POSMAP(5213))
c
      real*8 RAD,CLPT(240),ROTSTO(20,2),STONUM(3,4),CIRTOL(5),VECSAV(3)
c
      integer*4 kerr,kfl,kprm(8),jindex
c
      real*8 gbuf(7),gprm(25),gbpp(7)
c
      integer*4 i,inc,inum,inx(4),ix(3),j,icax
c
      real*8 rmch(3),qnum,snum,quad1(2),quad2(2),rang,rdis,rnum,angl2p,
     -       rmve(6),dv(3),vec(3),rmc1(3),rmc2(3),buf(7),rot(20,2),
     2       ndist,tol,stnum(3)
c
      character*2 lbuf
      character*80 msg
c
      equivalence (rmch,rmve(1))
c
      data inx /2,3,1,2/
c
c...Initialize routine
c
      tol    = .0005
      if (MCHOPT(2) .eq. 2) tol = .005
c
c...Adjust circle buffer for rotation
c
      kerr   = 0
      call copyn (gbuf,buf,7)
      call ciaxis1 (gbuf,gbpp,rot)
      call ciptad1 (gbuf(1),buf(1),rot,buf(4))
      call copyn (buf(1),gprm(1),3)
      kprm(3) = 0
c
c...Get machining plane using circular plane
c
      vec(1) = buf(4)
      vec(2) = buf(5)
      vec(3) = buf(6)
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) go to 50
      do 45 i=1,LASTAB,1
          snum = 360.0 - ROTSTO(i,1)
          call vecadj (vec,vec,snum,IRTWRK(i))
   45 continue
   50 icax   = 0
      do 55 i=1,3,1
          if (dabs(vec(i)) .gt. .9999) icax = i
   55 continue
c
      inc    = NPT
c     call ptrtad  (CLPT(1),rmch,ROTSTO,0)
      call clptpp1 (1,rmch,ROTSTO,buf(4))
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2)
     -   call preadr (STONUM(1,2),STONUM(1,3),ROTSTO,buf(4))
      call copyn (rmch,rmc1,3)
      call ptxfm (STONUM(1,3),stnum,1)
      snum   = dsqrt((rmch(1)-stnum(1))**2 +
     -               (rmch(2)-stnum(2))**2 +
     -               (rmch(3)-stnum(3))**2)
      if (snum .ge. .001 .or. MXCL .le. 2) then
          inc = 0
      else
          call clptpp1 (inc+1,rmch,ROTSTO,buf(4))
      end if
      j       = (MXCL-1) * NPT + 1
c     call ptrtad (CLPT(j),rmve(4),ROTSTO,0)
      call clptpp1 (j,rmve(4),ROTSTO,buf(4))
c
c...To get direction use two points from circle record if
c...MXCL >= 2 or unfortunately STONUM and 1-st point
c
      if (MXCL .gt. 1) then
c        call ptrtad (CLPT(NPT+1),rmc2,ROTSTO,0)
         call clptpp1 (NPT+1,rmc2,ROTSTO,buf(4))
      else
         call copyn (rmc1,rmc2,3)
         call ptxfm (STONUM(1,3),rmc1,1)
      end if
c
c...Check machining plane using 3 points from circle
c
      do 100 j=1,3,1
         ix(j) = 1
         do 65 i=1,2,1
            qnum = dabs(rmve((i-1)*3+j) - stnum(j))
            if (qnum .gt. tol) then
                ix(j) = 0
            end if
   65    continue
  100 continue
c
c...Save ending point of circle
c...(At this time in the logic it is the starting point also)
c
      gprm(8)  = stnum(1)
      gprm(9)  = stnum(2)
      gprm(10) = stnum(3)
c
c...Select machining plane
c
      if (ix(1)+ix(2)+ix(3) .gt. 1 .or. (ix(1)+ix(2)+ix(3) .eq. 0 .and.
     1    (IC3AXF(1) .eq. 1 .or. IHELIX .ne. 0))) then
          if (icax .ne. 0 .and. ix(icax) .eq. 1) then
              kprm(3) = icax
c
c......3-axis circle
c......Determine circular plane
c
          else if (IC3AXF(1) .eq. 1 .or. IHELIX .ne. 0) then
              call copyn (gbuf(4),buf(4),3)
              call betvec (VECSAV,buf(4),rang)
              if (rang .gt. 90.) call vctmsc (buf(4),-1.d0,buf(4))
              call plnvpt (buf(4),stnum,gprm(17),kerr)
              if (kerr .eq. 1) go to 9100
c
c.........Adjust center point to be on plane
c
              call plnint (gprm,buf(4),gprm(17),gprm,kerr)
              if (kerr .eq. 1) go to 9100
c
c.........Store circle parameters
c
              kprm(1) = 0
              kprm(2) = 0
              kprm(3) = 0
c
c.........Store final point
c
              call copyn (gprm(8),gprm(11),3)
c
c.........Store starting vector
c
              call vcmnvc (stnum,gprm(1),vec)
              call unitvc (vec,gprm(14))
c
c.........Store radius and angles
c
              gprm(4) = ndist(gprm(1),gprm(8))
              gprm(5) = 0.
              gprm(6) = 0.
              gprm(7) = 0.
c
c.........Store circular direction
c
              call vcmnvc (rmc1,gprm(1),vec)
              call unitvc (vec,vec)
              call vcmnvc (rmc2,gprm(1),dv)
              call unitvc (dv,dv)
              rang = angl2p (vec,dv,buf(4))
              kprm(5) = 2
              if (rang .gt. 180.d0) kprm(5) = 1
              call cirbnd (gprm,kprm,kprm(4),1)
              if (IHELIX .ne. 0 .and. kfl .eq. 1) IHELIX = 2
              go to 8000
          else
              go to 9100
          endif
      else
          kprm(3) = jindex(ix,1,3)
      endif
      if (kprm(3) .eq. 0) go to 9100
      kprm(1) = inx(kprm(3))
      kprm(2) = inx(kprm(3)+1)
c
c...Save start point of circle
c
      gprm(11) = stnum(kprm(1))
      gprm(12) = stnum(kprm(2))
c
c...Calculate radius
c
      quad1(1) = stnum(kprm(1)) - gprm(kprm(1))
      quad1(2) = stnum(kprm(2)) - gprm(kprm(2))
      quad2(1) = rmch(kprm(1)) - gprm(kprm(1))
      quad2(2) = rmch(kprm(2)) - gprm(kprm(2))
      gprm(4) = dsqrt(quad1(1)**2 + quad1(2)**2)
      if (gprm(4) .lt. CIRTOL(3)) go to 9200
      if (gprm(4) .gt. CIRTOL(4)) go to 9300
c
c...Calculate beginning angle
c
      rnum    = quad1(1) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      gprm(5) = dacos(rnum) * RAD
      call codint (ICMREG(kprm(1)),quad1(1),quad1(1),inum)
      call codint (ICMREG(kprm(2)),quad1(2),quad1(2),inum)
      if (quad1(2) .lt. 0.) gprm(5) = 360. - gprm(5)
c      call codint (ICMREG(1),gprm(5),gprm(5),inum)
c
c...Calculate ending angle
c
      gprm(6) = gprm(5)
      rnum    = quad2(1) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      rang   = dacos(rnum) * RAD
      call codint (ICMREG(kprm(1)),quad2(1),quad2(1),inum)
      call codint (ICMREG(kprm(2)),quad2(2),quad2(2),inum)
      if (quad2(2) .lt. 0.) rang = 360. - rang
c      call codint (ICMREG(kprm(1)),rang,rang,inum)
c
c...Calculate direction and delta angle
c
      gprm(7) = 0.
c
c      rdis   = rang - gprm(5)
c      if (rdis .lt. -180.) rdis = 360. + rdis
c      if (rdis .gt. 180.) rdis = rdis - 360.
c      if (dabs(rdis) .ge. 180.) go to 9400
c      kprm(5) = 2
c      if (rdis .lt. 0.) kprm(5) = 1
      call vcplvc (rmc1,gprm,rmc1,-1.d0)
      call vcplvc (rmc2,gprm,rmc2,-1.d0)
      call unitvc (rmc1,rmc1)
      call unitvc (rmc2,rmc2)
      call vcplvc (rmc2,rmc1,dv,-1.d0)
      call unitvc (dv,dv)
      vec(kprm(1)) = -rmc1(kprm(2))
      vec(kprm(2)) = rmc1(kprm(1))
      vec(kprm(3)) = 0.0
      call betvec (vec,dv,rdis)
      kprm(5) = 2
      if (rdis .gt. 90.) kprm(5) = 1
c
c...Calculate beginning quadrant
c
      call cirbnd (gprm,kprm,inum,1)
      kprm(4) = inum
c
c...End of routine
c
 8000 return
c
c...Could not calculate machining plan
c
 9100 kerr   = 1
      if (kfl .eq. 1) call psterr (1,'CIRPLN',' ',-1)
      go to 8000
c
c...Radius under minimum
c
 9200 kerr   = 1
      if (kfl .eq. 1) then
          call perrst ('CIRRAD',1,msg,0,gprm(4),lbuf,2)
          call perrst (msg,2,msg,0,CIRTOL(3),lbuf,2)
          call psterr (1,'CIRMINR',msg,-1)
      endif
      go to 8000
c
c...Radius over maximum
c
 9300 kerr   = 1
      if (kfl .eq. 1) then
          call perrst ('CIRRAD',1,msg,0,gprm(4),lbuf,2)
          call perrst (msg,2,msg,0,CIRTOL(4),lbuf,2)
          call psterr (1,'CIRMAXR',msg,-1)
      endif
      go to 8000
c
c...Could not calculate circular direction
c
 9400 kerr   = 1
      if (kfl .eq. 1) call psterr (2,'NOCIRDIR',' ',-1)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirhel (gprm,kprm,kfl)
c
c   FUNCTION:  This routine outputs a helical interpolation record as a
c              series of GOTO points.  It uses MCHNUM(n,3) for the final
c              point and depth of the motion.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kfl     I*4  D1  -  1 = Next block will be a circular block.
c                               Used to notify look ahead routines.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cirhel (gprm,kprm,kfl)
c
      include 'post.inc'
c
      equivalence (ISCIRC,KPOSMP(1238)), (NEXCIR,KPOSMP(1275))
      equivalence (IHELIX,KPOSMP(1392)), (IJKROT,KPOSMP(1739))
c
      integer*4 ISCIRC,NEXCIR,IHELIX,IJKROT
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (CIRTOL,POSMAP(2201)), (HELANG,POSMAP(2213))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),LINAXS(6),AXSOUT(10),TLVEC(3),ROTSTO(20,2),
     1       CIRTOL(5),RAD,HELANG(2),ROTANG(20,2),VECSAV(3)
c
      integer*4 kprm(8),kfl
c
      real*8 gprm(25)
c
      integer*4 iary(10),icnt,i,isav,itim
c
      real*8 rprm(25),rsgn(2),rary(3),rinc,rtim,raxs(10),rnum,rmch(3),
     1       rval
c
      data rsgn /-1.,1./
c
c...Initialize routine
c
      isav   = ISCIRC
      ISCIRC = 0
      if (IHELIX .eq. 0) HELANG(1) = 0.
      do 100 i=1,25,1
          rprm(i) = gprm(i)
  100 continue
c
c...Calculate degree increment
c
      rnum    = (gprm(4)-CIRTOL(5)) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      rinc   = dacos(rnum) * RAD * 2.0
      if (rinc .lt. 1.) rinc = 1.
      rtim   = dint(rprm(7)/rinc) + 1
      rinc   = rprm(7) / rtim
      itim   = rtim
c
c...Create circular points
c
      call ptxfm (MCHNUM(1,3),rary,1)
      do 500 i=1,itim,1
c
c......Calculate next point
c
          call ptxfm (MCHNUM(1,3),rmch,1)
          if (i .ne. itim) then
              rprm(5) = rprm(5) + rinc * rsgn(kprm(5))
              call cirpt (rprm,kprm,rprm(5),rmch)
              rval = HELANG(1) * (rprm(7)-rinc)
              if (kprm(3) .eq. 0) then
                  call vcplvc (rmch,gprm(17),rmch,rval)
              else
                  rmch(kprm(3)) = rmch(kprm(3)) + rval
              endif
              rprm(7) = rprm(7) - rinc
          else
              rmch(1) = rary(1)
              rmch(2) = rary(2)
              rmch(3) = rary(3)
          endif
c
c......Adjust points for output
c
          call ptxfr (rmch,MCHNUM(1,3),1)
          call alladj (MCHNUM,LINAXS,raxs,ROTSTO,3,5)
c
c......Output motion block
c
          if (i .eq. 1) then
              call pshaxs (raxs,VECSAV)
          else
              call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
              if (IJKROT .eq. 1) call copyn (VECSAV,TLVEC,3)
              call pshaxs (raxs,VECSAV)
              call motion (iary,icnt)
          endif
  500 continue
c
c......Output final point
c
      call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
      if (IJKROT .eq. 1) call copyn (VECSAV,TLVEC,3)
      if (kfl .eq. 1) NEXCIR = 2
      call motion (iary,icnt)
      NEXCIR = 0
c
c...End of routine
c
 8000 ISCIRC = isav
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirijk (gprm,kprm)
c
c   FUNCTION:  This routine outputs circular interpolation specific
c              codes (circle center, radius, etc.)
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cirijk (gprm,kprm)
c
      include 'post.inc'
c
      equivalence (REGFRC,KPOSMP(0603))
      equivalence (MOTFLG,KPOSMP(1073)), (MACHTP,KPOSMP(1201))
      equivalence (LTHXY ,KPOSMP(1225)), (INCR  ,KPOSMP(1226))
      equivalence (POLCOD,KPOSMP(1262)), (IPOLIN,KPOSMP(1265))
      equivalence (ICRFMT,KPOSMP(1368)), (ICRSGN,KPOSMP(1369))
      equivalence (CIRCOD,KPOSMP(1378)), (ICRIJP,KPOSMP(1384))
      equivalence (ICRDIM,KPOSMP(1389)), (IHELIX,KPOSMP(1392))
      equivalence (IC3AXF,KPOSMP(1747)), (LCRFMT,KPOSMP(4103))
      equivalence (LFIRCD,KPOSMP(4112)), (LDIRCD,KPOSMP(4118))
      equivalence (LTMODE,KPOSMP(4125)), (MTPDYN,KPOSMP(4126))
      equivalence (CIRCON,KPOSMP(4242)), (ICRHEL,KPOSMP(4250))
c
      integer*4 MOTFLG,INCR,ICRFMT,ICRSGN,CIRCOD(6),ICRIJP,POLCOD(3),
     1          IHELIX,LTMODE,ICRHEL(10),IPOLIN,ICRDIM,MACHTP,MTPDYN,
     2          LCRFMT,LFIRCD(6),LDIRCD(6),LTHXY,IC3AXF(10),CIRCON(8),
     3          REGFRC(92)
c
      equivalence (RAD   ,POSMAP(0002))
      equivalence (STONUM,POSMAP(1387)), (HELANG,POSMAP(2213))
      equivalence (CIRCDV,POSMAP(2206)), (CIRCOV,POSMAP(2215))
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 STONUM(3,4),HELANG(2),RAD,ROTSTO(20,2),CIRCDV(12),
     1       CIRCOV(8)
c
      integer*4 kprm(8)
c
      real*8 gprm(25)
c
      integer*4 ifmt,ix(3),isav,i,ccod(6),isw(8)
c
      real*8 rnum,vec(3),svm(3),prm(25),tmp,vc1(3),stnum(3)
c
c...Set circular format
c
      call copynk (CIRCOD,ccod,6)
      call copyn (gprm,prm,25)
      call ptxfm (STONUM(1,3),stnum,1)
c
c......Mill/Turn machine
c
      if (MACHTP .eq. 4 .and. LTMODE .gt. 0) then
          ifmt   = LCRFMT
          if (LTMODE .eq. 1) then
             call copynk (LFIRCD,ccod,6)
          else if (LTMODE .eq. 2) then
             call copynk (LDIRCD,ccod,6)
             call cicent1 (gprm,prm)
          end if
      else
          ifmt   = ICRFMT
      end if
c
c......3-axis circular interpolation
c
      if (kprm(3) .eq. 0) then
          call copynk (IC3AXF(3),ccod,5)
c
c.........Calculate intermediate point
c
          call vcmnvc (gprm(8),gprm(1),vc1)
          call unitvc (vc1,vc1)
          if ((gprm(7) .gt. 175 .and. gprm(7) .lt. 185) .or.
     1        (gprm(7) .gt. 365. .or. gprm(7) .lt. 5)) then
              if (kprm(5) .eq. 1) then
                  call crosvc (gprm(17),vc1,vec)
              else
                  call crosvc (vc1,gprm(17),vec)
              endif
          else
              call vcplvc (vc1,gprm(14),vec,1.d0)
              call unitvc (vec,vec)
              if (gprm(7) .gt. 180.) call vctmsc(vec,-1.d0,vec)
          endif
          call vcplvc (gprm(1),vec,prm(1),gprm(4))
c
c.........Only support absolute or incremental IJKs
c.........Which is actually the intermediate point and
c.........not the circle center
c
          if (IC3AXF(2) .eq. 1) then
              ifmt = 8
          else if (IC3AXF(9) .eq. 2) then
              ifmt = 4
          else
              if (ifmt .eq. 3) ifmt = 2
              if (ifmt .eq. 5 .or. ifmt .eq. 6) ifmt = 4
              if (ifmt .eq. 9) ifmt = 1
          endif
      endif
c
c......Determine if absolute or incremental
c......center point output based on abs/incr mode
c
      if (ifmt .eq. 4) then
          if (INCR .eq. 1) then
              ifmt   = 1
          else
              ifmt   = 2
          endif
      else if (ifmt .eq. 5) then
          if (INCR .eq. 1) then
              ifmt   = 1
          else
              ifmt   = 3
          endif
      endif
c
c...Set center point registers
c
      if (ICRIJP .eq. 1 .or. kprm(3) .eq. 0) then
          ix(1) = 3
          ix(2) = 4
          ix(3) = 5
      else
          ix(1) = kprm(1) + 2
          ix(2) = kprm(2) + 2
          ix(3) = kprm(3) + 2
      endif
      if (LTMODE .eq. 2) then
          isav  = ix(2)
          ix(2) = ix(1)
          ix(1) = ix(3)
          ix(3) = isav
      end if
c
c...Adjust center point for TRANS/LAST
c
c      call pstadj (gprm(1),rmch)
c
c...Output helical lead
c
      if (IHELIX .eq. 1 .and. ICRHEL(3) .ne. 0) then
          if (ICRHEL(3) .eq. 1) then
              rnum   = HELANG(1) * RAD
          else
              rnum   = HELANG(1) * 360.
          endif
          call codout (ccod(ix(3)),rnum)
       endif
c
c...Absolute IJK's
c
      if (ifmt .eq. 1) then
          if (kprm(3) .eq. 0) then
              call codout (ccod(ix(1)),prm(1))
              call codout (ccod(ix(2)),prm(2))
              call codout (ccod(ix(3)),prm(3))
          else
              rnum   = prm(kprm(2))
              if (MTPDYN .eq. 2 .and. ICRDIM .eq. 2)
     1            rnum = rnum * 2.d0
              call codout (ccod(ix(1)),prm(kprm(1)))
              call codout (ccod(ix(2)),rnum)
          endif
c
c...Incremental IJK's
c
      else if (ifmt .eq. 2 .or. ifmt .eq. 3) then
          if (kprm(3) .eq. 0) then
              do 290 i=1,3,1
                  call getcod (ccod(ix(i)),tmp)
                  call setcod (ccod(ix(i)),stnum(i))
                  call increg (ccod(ix(i)),prm(i),rnum)
                  call setcod (ccod(ix(i)),tmp)
                  call codout (ccod(ix(i)),rnum)
  290         continue
          else
              if (LTMODE .eq. 2) then
                  call preadr (STONUM(1,2),svm,ROTSTO,vec)
                  svm(2) = ROTSTO(1,2)
              else
                  call copyn (stnum,svm,3)
              end if
              do 300 i=1,2,1
                  call getcod (ccod(ix(i)),tmp)
                  call setcod (ccod(ix(i)),svm(kprm(i)))
                  call increg (ccod(ix(i)),prm(kprm(i)),rnum)
                  call setcod (ccod(ix(i)),tmp)
                  if (ifmt .eq. 2) then
                      if (ICRSGN .eq. 2) rnum   = rnum   * (-1.)
                  else
                      rnum   = dabs(rnum)
                  endif
                  if (MTPDYN .eq. 2 .and. ICRDIM .eq. 2 .and. i .eq. 2)
     1                   rnum = rnum * 2.d0
                  call codout (ccod(ix(i)),rnum)
  300         continue
          endif
c
c...Radius
c
      else if (ifmt .eq. 6) then
          rnum   = prm(4)
          if (prm(7) .gt. 180.d0) rnum = rnum * (-1.)
          call codout (ccod(6),rnum)
c
c...Polar coordinates
c
      else if (ifmt .eq. 7) then
          if (IPOLIN .eq. 1) then
              rnum   = prm(kprm(2))
              if (MTPDYN .eq. 2 .and. ICRDIM .eq. 2)
     1                rnum = rnum * 2.d0
              call codout (ccod(ix(1)),prm(kprm(1)))
              call codout (ccod(ix(2)),rnum)
              call codout (ccod(6),prm(4))
              rnum   = prm(5)
              if (rnum .gt. 180.d0) rnum = rnum - 360.d0
              call codout (POLCOD(3),rnum)
          else
              rnum   = prm(6)
              if (rnum .gt. 180.d0) rnum = rnum - 360.d0
              call codout (POLCOD(3),rnum)
          endif
c
c...Cinci Dual block
c
      else if (ifmt .eq. 8) then
          isav   = MOTFLG
          MOTFLG = 0
          if (kprm(3) .eq. 0) then
              if (INCR .eq. 1) then
                  call codout (ccod(ix(1)),prm(1))
                  call codout (ccod(ix(2)),prm(2))
                  call codout (ccod(ix(3)),prm(3))
              else
                  do 500 i=1,3,1
                      call getcod (ccod(ix(i)),tmp)
                      call setcod (ccod(ix(i)),stnum(i))
                      call increg (ccod(ix(i)),prm(i),rnum)
                      call setcod (ccod(ix(i)),tmp)
                      call codout (ccod(ix(i)),rnum)
  500             continue
              endif
          else
              rnum   = prm(kprm(2))
              if (MTPDYN .eq. 2 .and. ICRDIM .eq. 2)
     1            rnum = rnum * 2.d0
              call codout (ccod(ix(1)),prm(kprm(1)))
              call codout (ccod(ix(2)),rnum)
          endif
          call clrbuf
          MOTFLG = isav
c
c...Absolute & Radius
c
      else if (ifmt .eq. 9) then
          rnum   = prm(kprm(2))
          if (MTPDYN .eq. 2 .and. ICRDIM .eq. 2)
     1            rnum = rnum * 2.d0
          call codout (ccod(ix(1)),prm(kprm(1)))
          call codout (ccod(ix(2)),rnum)
          rnum   = prm(4)
          if (prm(7) .gt. 180.d0) rnum = rnum * (-1.)
          call codout (ccod(6),rnum)
c
c...Conversation polar coordinates
c
      else if (ifmt .eq. 10) then
          isav   = MOTFLG
          MOTFLG = 0
          call svfedc (isw)
c
c......Output center point
c
          call codout (CIRCON(1),CIRCOV(1))
          REGFRC(ccod(ix(1))) = 1
          REGFRC(ccod(ix(2))) = 1
          call codout (ccod(ix(1)),prm(kprm(1)))
          call codout (ccod(ix(2)),prm(kprm(2)))
cc          call codout (ccod(ix(3)),prm(kprm(3)))
          call clrbuf
c
c.......Output circular block
c
          call codout (CIRCON(2),CIRCOV(2))
          rnum   = prm(7)
          if (kprm(5) .eq. 1) rnum = -rnum
          call codout (CIRCON(3),rnum)
          call codout (CIRCOD(kprm(5)),CIRCDV(kprm(5)))
          MOTFLG = isav
          call rsfedc (isw)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirout (gprm,kprm)
c
c   FUNCTION:  This routine outputs a circular interpolation record.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cirout (gprm,kprm)
c
      include 'post.inc'
c
      equivalence (MOTFLG,KPOSMP(1073)), (MACHTP,KPOSMP(1201))
      equivalence (ISCIRC,KPOSMP(1238)), (ICRXYZ,KPOSMP(1239))
      equivalence (MCHPLN,KPOSMP(1261)), (POLCOD,KPOSMP(1262))
      equivalence (IPOLIN,KPOSMP(1265)), (ICIPR2,KPOSMP(1266))
      equivalence (IPOLOT,KPOSMP(1274)), (NEXCIR,KPOSMP(1275))
      equivalence (IHELTP,KPOSMP(1280)), (ICRFMT,KPOSMP(1368))
      equivalence (ICIRSW,KPOSMP(1391)), (IHELIX,KPOSMP(1392))
      equivalence (ICIDLT,KPOSMP(1741)), (ITMDLT,KPOSMP(1740))
      equivalence (IC3AXF,KPOSMP(1747))
      equivalence (CUTCFL,KPOSMP(3251)), (ICUTDO,KPOSMP(3301))
      equivalence (ICUTSW,KPOSMP(3316)), (IZIGON,KPOSMP(0370))
      equivalence (IZIGAD,KPOSMP(0371)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 ISCIRC,ICRXYZ,IHELIX,MCHPLN,IPOLIN,IPOLOT,IHELTP,
     1          ICRFMT,POLCOD(3),ICIPR2(8),NEXCIR,MOTFLG,CUTCFL(20),
     2          ICUTDO(15),ICUTSW(3),MACHTP,IZIGON,IZIGAD,
     3          MTPDYN,LTMODE,ICIDLT,ITMDLT,ICIRSW,IC3AXF(10)
c
      equivalence (AXSOUT,POSMAP(1340)), (STONUM,POSMAP(1387))
      equivalence (CIRTOL,POSMAP(2201)), (HELANG,POSMAP(2213))
      equivalence (POLCDV,POSMAP(2227)), (RCIPR2,POSMAP(2074))
      equivalence (ZIGDEP,POSMAP(0190)), (ZIGSTO,POSMAP(0191))
      Equivalence (CUTPRM,POSMAP(2446))
c
      real*8 AXSOUT(10),HELANG(2),STONUM(3,4),CIRTOL(5),
     1       POLCDV(2),RCIPR2(25),CUTPRM(3),ZIGSTO,ZIGDEP
c
      integer*4 kprm(8)
c
      real*8 gprm(25)
c
      integer*4 iary(10),icnt,ifl,lfl,i,isw(10),ilin,nfl,nint,inx(4)
c
      real*8 rnum,ndist,stnum(3)
c
c...Set motion delta estimation flag
c
      ITMDLT = ICIDLT
c
c...Set up machining plan
c
      if (kprm(3) .ne. 0) MCHPLN = kprm(3)
      ilin   = 0
      nfl    = 0
c
c...Set automatic helical angle
c
      if (IHELTP .eq. 2) HELANG(1) = HELANG(1) / gprm(7)
c
c...Adjust final point
c...to lie on circle
c
      call ciradj (gprm,kprm)
c
c...Break up circular move
c...if required
c
      do 50 i=1,25,1
          RCIPR2(i) = gprm(i)
   50 continue
      do 60 i=1,8,1
          ICIPR2(i) = kprm(i)
   60 continue
  100 call cirqad (RCIPR2,ICIPR2,gprm,kprm,ifl)
c
c...Check if LMFP mode and brake arc on axis boundaries
c
  200 if (LTMODE .eq. 1) then
          call cirqad1 (RCIPR2,gprm,kprm,inx,nint,nfl)
      end if
c
c...Set up final position
c...(vp 27-feb-96 add cirfin1 to set AXSOUT)
c
      call cirfin1 (kprm,gprm)
c
c...Output circular block
c
      call whchax (AXSOUT,iary,icnt)
      if (icnt .ne. 0 .or. gprm(7) .gt. 180.) then
c
c......If move is too short
c......Output as linear motion
c
          ilin   = 0
          if (gprm(7) .lt. 180. .and. CIRTOL(2) .ne. 0.) then
              if (kprm(3) .eq. 0) then
                  rnum = ndist(gprm(8),STONUM(1,3))
              else
                  call ptxfm (STONUM(1,3),stnum,1)
                  rnum = dsqrt((gprm(kprm(1)+7)-stnum(kprm(1)))**2 +
     1                         (gprm(kprm(2)+7)-stnum(kprm(2)))**2)
              endif
              if (rnum .lt. CIRTOL(2)) then
                  call cirhel (gprm,kprm,ifl)
                  ilin   = 1
                  go to 1000
              endif
          endif
c
c......Post generated
c......Helical interpolation
c
          if (IHELIX .eq. 2 .or. ICIRSW .eq. 2) then
              call cirhel (gprm,kprm,ifl)
              ilin   = 1
              go to 1000
          endif
c
c...Output polar linear move
c...if not already output
c
          if (ICRFMT .eq. 7 .and. IPOLOT .ne. 1) then
              IPOLIN = 1
              MOTFLG = 0
              call codout (POLCOD(1),POLCDV(1))
              call cirijk (gprm,kprm)
c
c......Output circular direction
c......for active cutter compensation
c
              if (ICUTDO(2) .eq. 1 .and. CUTCFL(1) .eq. 4) then
                  ICUTSW(1) = 0
                  ICUTSW(2) = 1
                  CUTPRM(2) = kprm(5)
                  call cutout (iary)
              endif
c
              call clrbuf
              IPOLIN = 0
              IPOLOT = 1
          endif
c
c......Force position on
c......circular block
c
          if (ICRXYZ .eq. 1) then
              call get_plan (kprm,lfl,isw)
              call frcmot (2,lfl,isw)
              call whchax (AXSOUT,iary,icnt)
          endif
          if (ifl .eq. 1) NEXCIR = 2
          call motion (iary,icnt)
          if (kprm(3) .eq. 0 .or. ICRFMT .eq. 10) IC3AXF(8) = 1
          NEXCIR = 0
      endif
c
c...Setup new start point
c
      call copyn (gprm(8),RCIPR2(11),3)
      if (ICIPR2(3) .eq. 0) then
          call vcmnvc (RCIPR2(11),RCIPR2(1),RCIPR2(14))
          call unitvc (RCIPR2(14),RCIPR2(14))
      endif
c
c...Go get another portion of
c...circular record when broken up
c
 1000 if (nfl .ne. 0) go to 200
      if (ifl .eq. 1) go to 100
c
c...End of routine
c
 8000 ISCIRC = 0
      IHELIX = 0
      HELANG(1) = 0.
      ITMDLT = 2
      if (ilin .eq. 0) IPOLOT = 0
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirpol
c
c   FUNCTION:  This routine samples the next clfile record to see if it
c              is a circular record and should only be called when polar
c              coordinate circular is supported.  It sets certain flags
c              when the next record is a circle, which enable the output
c              of polar coordinates instead of linear.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cirpol
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (IPOLIN,KPOSMP(1265))
      equivalence (ICIPR2,KPOSMP(1266)), (IPOLOT,KPOSMP(1274))
      equivalence (NEXCIR,KPOSMP(1275))
c
      integer*4 ITYPE,IPOLIN,ICIPR2(8),IPOLOT,NEXCIR
c
      equivalence (CIRBUF,POSMAP(0731)), (MCHNUM,POSMAP(1287))
      equivalence (STONUM,POSMAP(1387)), (RCIPR2,POSMAP(2074))
      equivalence (CIRBPP,POSMAP(4845))
c
      real*8 RCIPR2(25),STONUM(3,4),MCHNUM(3,4),CIRBUF(7),CIRBPP(7)
c
      integer*4 ierr,i
c
      real*8 rmch(3)
c
c...Next record is buffered
c...in CIRCUL routine
c
      if (NEXCIR .eq. 2) go to 1000
c
c...Get next record
c...Check for circular record
c
      call clsamp (0)
      if (ITYPE .ne. 3000) go to 8000
c
      call clsamp (1)
      if (ITYPE .ne. 5000) go to 8000
c
c...Circular record
c......Set up previous position
c
      do 100 i=1,3,1
          rmch(i) = STONUM(i,3)
          STONUM(i,3) = MCHNUM(i,3)
  100 continue
c
c......Calculate circular parameters
c
      call cirdir (CIRBUF,CIRBPP,RCIPR2,ICIPR2,0,ierr)
      STONUM(1,3) = rmch(1)
      STONUM(2,3) = rmch(2)
      STONUM(3,3) = rmch(3)
      if (ierr .ne. 0) go to 8000
c
c......Next move is circular
c......set polar/linear move flags
c
 1000 IPOLOT = 1
      IPOLIN = 1
c
c...End of routine
c
 8000 call clsamp (-1)
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirpt (gprm,kprm,gang,gxyz)
c
c   FUNCTION:  This routine calculates a point on a circle at a given
c              angle.
c
c   INPUT:  gprm    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           gang    R*8  D1  -  Angle at which to calculate point.
c
c   OUTPUT: gxyz    R*8  D3  -  XYZ of calculated point.
c
c***********************************************************************
c
      subroutine cirpt (gprm,kprm,gang,gxyz)
c
      include 'post.inc'
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      integer*4 kprm(8)
c
      real*8 gprm(25),gang,gxyz(3)
c
      real*8 s1,s2,vec(3),xlen,ylen,yaxis(3),xcomp(3),ycomp(3)
c
c...3-axis circle
c
      if (kprm(3) .eq. 0) then
          call crosvc (gprm(17),gprm(14),yaxis)
          xlen = gprm(4) * dcos(gang/RAD)
          ylen = gprm(4) * dsin(gang/RAD)
          call vctmsc (gprm(14),xlen,xcomp)
          call vctmsc (yaxis,ylen,ycomp)
          call vcplvc (xcomp,ycomp,vec,1.d0)
          call vcplvc (gprm(1),vec,gxyz,1.d0)
c
c...Calculate point on a circle
c...at a given angle
c
      else
          s1    = dcos(gang/RAD)
          s2    = dsin(gang/RAD)
          gxyz(kprm(1)) = gprm(kprm(1)) + gprm(4) * s1
          gxyz(kprm(2)) = gprm(kprm(2)) + gprm(4) * s2
          gxyz(kprm(3)) = gprm(kprm(3)+7)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirqad (gprm,kprm,gout,kout,kfl)
c
c   FUNCTION:  This routine breaks up a circular interpolation record on
c              quadrant, hemisphere or 360 degree boundaries when re-
c              quired.
c
c   INPUT:  gprm    R*8  D25 -  Input circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Input circle parameters (same format as
c                               ICIPRM).
c
c   OUTPUT: gout    R*8  D25 -  Output circle parameters (same format as
c                               RCIPRM), broken on boundary if required.
c
c           kout    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kfl     I*4  D1  -  Returns 1 when the record was broken on
c                               a boundary.  Should be set to 0 prior to
c                               initial call and not changed thereafter.
c
c***********************************************************************
c
      subroutine cirqad (gprm,kprm,gout,kout,kfl)
c
      include 'post.inc'
c
      equivalence (ICRBRK,KPOSMP(1372))
      equivalence (IHELIX,KPOSMP(1392)), (ICMREG,KPOSMP(1393))
c
      integer*4 ICRBRK,ICMREG(3),IHELIX
c
      integer*4 kprm(8),kout(8),kfl
c
      real*8 gprm(25),gout(25)
c
      integer*4 i,inc,inum,iquad,qloc(8),hloc(8)
c
      real*8 qbnd(8),hbnd(8),rang(2),rval
c
      data rang /90.d0,180.d0/
      data qbnd /0.,90., 90.,180., 180.,270., 270.,0./
      data qloc /4,2,     1,3,       2,4,      3,1/
      data hbnd /270.,180., 0.,270., 90.,0., 180.,90./
      data hloc /3,3,         4,4,    1,1,     2,2/
c
c...Store circular parameters
c
      kfl    = 0
      do 100 i=1,25,1
          gout(i) = gprm(i)
  100 continue
      do 200 i=1,8,1
          kout(i) = kprm(i)
  200 continue
      if (kprm(3) .eq. 0) then
          call codint (ICMREG(1),gout(7),gout(7),inum)
      else
          call codint (ICMREG(kprm(1)),gout(7),gout(7),inum)
      endif
c
c...Full 360 degree circle
c
      if (ICRBRK .eq. 3) then
c
c......Calculate new ending & delta angle
c
          if (gout(7) .gt. 360.d0) then
              gout(6) = gprm(5)
              gprm(7) = gprm(7) - 360.d0
              gout(7) = 360.d0
c
c......Calculate new ending point
c
              call cirpt (gout,kout,gout(6),gout(8))
              kfl    = 1
          endif
c
c...Break on hemisphere/quadrant boundaries
c
      else
c
c......Move is greater than 180 degrees
c......Break up circular record
c
          if (gout(7) .gt. rang(ICRBRK)) then
              kfl    = 1
c
c......Calculate ending quadrant
c
          else if (kprm(3) .ne. 0) then
              call cirbnd (gprm,kprm,iquad,2)
              inum   = abs(iquad-kprm(4))
              if (ICRBRK .eq. 1 .and. inum .ne. 0) kfl = 1
              if (ICRBRK .eq. 2 .and. inum .eq. 2) kfl = 1
          endif
c
c......Move needs to be broken up
c......Calculate new end point
c
          if (kfl .eq. 1) then
              inc    = (kprm(4)-1) * 2 + kprm(5)
              if (ICRBRK .eq. 1) then
                  gout(6) = qbnd(inc)
                  kprm(4) = qloc(inc)
              else
                  gout(6) = hbnd(inc)
                  kprm(4) = hloc(inc)
              endif
              gprm(5) = gout(6)
              gout(7) = dabs(gout(6)-gout(5))
              if (gout(7) .gt. 180.) gout(7) = 360. - gout(7)
              gprm(7) = gprm(7) - gout(7)
              if (kprm(3) .eq. 0 .and. IHELIX .eq. 0) then
                  rval = gout(6) - gout(5)
                  if (rval .lt. 0.) rval = rval + 360.
              else
                  rval = gout(6)
              endif
              call cirpt (gout,kout,rval,gout(8))
          endif
      endif
c
c...End of routine
c
 8000 return
      end
