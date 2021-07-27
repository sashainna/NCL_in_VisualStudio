C
C*********************************************************************
C*    NAME         :  cirapt.f
C*       CONTAINS:
C*					cirapt (kclf,krec,cirbuf,spt,npt,kerr)
C*					cirdir (gbuf,gprm,kprm,spt,clpt,npt,mxcl,kerr)
C*					cirbnd (gprm,kprm,kquad,ktyp)
C*					circk (gprm,kprm,spt,clpt,npt,mxcl,kerr)
C*					cirout (iciprm,rciprm,kerr)
C*					dpoint (ginp,gout,kacy)
C*    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*        cirapt.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*        04/29/15 , 15:09:40
C*********************************************************************
C
c***********************************************************************
c
c   SUBROUTINE:  cirapt (kclf,krec,cirbuf,spt,npt,kerr)
c
c   FUNCTION:  This routine validates that circular motion in the clfile
c              is valid and outputs circular interpolation record(s)
c              to the APT source file if it is valid.
c              tion and helical records.
c
c   INPUT:  kclf    I*2  D1  -  0 = Internal clfile, 1 = External clfile.
c
c           krec    I*4  D1  -  Beginning circular interpolation motion
c                               record in clfile.
c
c           gcb     R*8  D7  -  Circle record from clfile.
c
c           spt     R*8  D6  -  GOTO point prior to circular motion.
c
c           npt     I*2  D1  -  3 = MULTAX/OFF, 6 = MULTAX/ON, 21= expanded cl.
c
c   OUTPUT: spt     R*8  D6  -  Last point on circular motion.
c
c           kerr    I*2  D1  -  Returns 1 if an error occurred.
c
c***********************************************************************
c
      subroutine cirapt (kclf,krec,gcb,spt,npt,kerr)
c
      include 'com8a.com'
c
      integer*2 kclf,npt,kerr
      integer*4 krec(2)
c
      real*8 gcb(7),spt(6)
c
      integer*2 mxcl,isiz,jerr,iflg,irot
      integer*4 irec(2),isav(2),iciprm(5),iclw(6),isn
c
      real*8 rclw(640),rciprm(16),sec,cirbuf(7),rivmx(12),romx(12),
     1       romxi(12),xvec(3)
c
c...Apply APTSRC matrix
c
      do 50 i=1,7,1
          cirbuf(i) = gcb(i)
   50 continue
      if (ifl(353) .eq. 1) then
          call conent (cirbuf(1),sc(183),3)
          call conent (cirbuf(4),sc(183),4)
          call conent (spt(1),sc(183),3)
          call conent (spt(4),sc(183),4)
      endif
c
c...Rotate circle into XY-plane
c...if in a 3-axis orientation
c
      irot   = 0
      if (dabs(cirbuf(4)) .gt. .9999999 .or.
     1    dabs(cirbuf(5)) .gt. .9999999 .or.
     2    dabs(cirbuf(6)) .gt. .9999999 .or. ifl(371) .ne. 1) then
          call identmx (romxi)
      else
          irot    = 1
          call point_on_plane(cirbuf(1),spt,cirbuf(4),cirbuf(1))
          call vcmnvc (spt,cirbuf(1),xvec)
          call unitvc (xvec,xvec)
          call crotmx (cirbuf(4),xvec,romx,romxi)
          call conent (cirbuf(1),romx,3)
          call conent (cirbuf(4),romx,4)
          call conent (spt(1),romx,3)
          call conent (spt(4),romx,4)
      endif
c
c...Loop until non-circular motion record is found
c
      call ncl_setptr(krec, irec)
      kerr   = 2
      iflg   = 1
  100 call ncl_setptr(irec, isav)
      call clread (kclf,irec,iclw,rclw,jerr)
      if (iclw(3) .eq. 1000 .or. iclw(3) .eq. 0) go to 100
c
c...Is this motion record part
c...of the circular interpolation record
c
      if (jerr .eq. 1 .or. (iclw(3).ne.5000 .and. iclw(3).ne.5200) .or.
     1    (iclw(4) .ne. 6 .and. iflg .ne. 1)) go to 1000
      kerr   = 0
      isn    = iclw(1)
      isiz = npt
      if (iclw(3).eq.5200) isiz = 21
      mxcl   = iclw(5) / isiz
c
c...Apply APTSRC matrix &
c...Rotation matrix
c
      if (ifl(353) .eq. 1 .or. irot .eq. 1) then
          do 200 i=1,mxcl*isiz,isiz
              if (ifl(353) .eq. 1) call conent (rclw(i),sc(183),3)
              if (irot .eq. 1) call conent (rclw(i),romx,3)
              if (isiz .gt. 3) then
                  if (ifl(353) .eq. 1) then
                      call conent (rclw(i+3),sc(183),4)
                      sec = dsqrt(rclw(i+3)**2 + rclw(i+4)**2 +
     1                            rclw(i+5)**2)
                      rclw(i+3) = rclw(i+3) / sec
                      rclw(i+4) = rclw(i+4) / sec
                      rclw(i+5) = rclw(i+5) / sec
                  endif
                  if (irot .eq. 1) call conent (rclw(i+3),romx,4)
              endif
  200     continue
      endif
c
c...Calculate center point &
c...circular direction
c
      if (iclw(4) .ne. 6) then
          call cirdir (cirbuf,rciprm,iciprm,spt,rclw,isiz,mxcl,kerr)
          if (kerr .ne. 0) go to 9000
      endif
c
c...Calculate delta ending angle
c
      call circk (rciprm,iciprm,spt,rclw,isiz,mxcl,kerr)
      if (kerr .ne. 0) go to 9000
c
c...Determine if next record
c...is continuation record
c
      iflg   = 0
      go to 100
c
c...Output circular record
c
 1000 if (kerr .ne. 2) then
          call cirout (iciprm,rciprm,spt,isn,irot,romxi,kerr)
          if (kerr .ne. 0) go to 9000
          spt(1) = rciprm(8)
          spt(2) = rciprm(9)
          spt(3) = rciprm(10)
      else
          kerr   = 0
      endif
c
c...Place spt back in programmed system
c
      if (irot .eq. 1) then
          call conent (spt,romxi,3)
          call conent (spt(4),romxi,4)
      endif
      if (ifl(353) .eq. 1) then
          call invmx (sc(183),rivmx)
          call conent (spt,rivmx,3)
          call conent (spt(4),rivmx,4)
      endif
c
c...End of routine
c
 8000 call ncl_eqlptr(isav,krec,iflg)
      if (iflg .eq. 1) kerr = 1
      call ncl_setptr(isav, krec)
      return
c
c...Error processing circular record
c
 9000 call ncl_setptr(krec, isav)
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirdir (gbuf,gprm,kprm,spt,clpt,npt,mxcl,kerr)
c
c   FUNCTION:  This routine determines the Machining Plane, starting an-
c              gle, starting quadrant & direction of a circular inter-
c              polation record.
c
c   INPUT:  gbuf    R*8  D7  -  Circle record (Type 3000), XYZIJKR.
c
c           spt     R*8  D6  -  Point coming from.
c
c           clpt    R*8  D245   Cl points in circular record.
c
c           npt     I*2  D1  -  3 = MULTAX/OFF, 6 = MULTAX/ON.
c
c           mxcl    I*2  D1  -  Number of points in 'clpt'.
c
c   OUTPUT: gprm    R*8  D12 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cirdir (gbuf,gprm,kprm,spt,clpt,npt,mxcl,kerr)
c
      integer*2 npt,mxcl,kerr
      integer*4 kprm(8)
c
      real*8 gbuf(7),gprm(16),clpt(640),spt(6)
c
      integer*4 i,inc,inum,inx(4),iacy,iax,iacy5
c
      real*8 rmch(3),qnum,snum,quad1(2),quad2(2),rang,rdis,rnum,
     1       rnum1,rnum2,pi,rad
c
      data inx /2,3,1,2/
c
c...Adjust circle buffer for rotation
c
      kerr   = 0
      iacy   = 4
      iacy5  = 5
      pi     = 3.141592653589793d0
      rad    = 180.0d0 / pi
c      call ptrtad (gbuf(1),gprm(1),ROTSTO,0)
      gprm(1) = gbuf(1)
      gprm(2) = gbuf(2)
      gprm(3) = gbuf(3)
c
c...Check that tool axis is
c...parallel to circle vector
c
      if (dabs(gbuf(6)) .gt. .9999999) then
          iax    = 6
      else if (dabs(gbuf(5)) .gt. .9999999) then
          iax    = 5
      else if (dabs(gbuf(4)) .gt. .9999999) then
          iax    = 4
      else
          go to 9000
      endif
      rnum1 = dabs(spt(iax))
      rnum2 = dabs(gbuf(iax))
      call dpoint (rnum1,rnum1,iacy5)
      call dpoint (rnum2,rnum2,iacy5)
      if (rnum1 .ne. rnum2) go to 9000
c
c...Get machining plane
c
      inc    = npt
      if (mxcl .eq. 1) inc = 0
c      call ptrtad (CLPT(inc+1),rmch,ROTSTO,0)
      rmch(1) = clpt(inc+1)
      rmch(2) = clpt(inc+2)
      rmch(3) = clpt(inc+3)
      kprm(3) = iax    - 3
      snum    = dabs(rmch(kprm(3))-spt(kprm(3)))
      do 100 i=1,3,1
          qnum   = dabs(rmch(i)-spt(i))
          if (qnum .lt. snum) then
              snum   = qnum
              kprm(3) = i
          endif
  100 continue
      if (kprm(3) .eq. 0 .or. kprm(3)+3 .ne. iax) go to 9000
      kprm(1) = inx(kprm(3))
      kprm(2) = inx(kprm(3)+1)
c
c...Calculate radius
c
      quad1(1) = spt(kprm(1)) - gprm(kprm(1))
      quad1(2) = spt(kprm(2)) - gprm(kprm(2))
      quad2(1) = rmch(kprm(1)) - gprm(kprm(1))
      quad2(2) = rmch(kprm(2)) - gprm(kprm(2))
      gprm(4) = dsqrt(quad1(1)**2 + quad1(2)**2)
      if (gprm(4) .lt. .001) go to 9000
c
c...Calculate beginning angle
c
      rnum    = quad1(1) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      gprm(5) = dacos(rnum) * RAD
      call dpoint (quad1(1),quad1(1),iacy)
      call dpoint (quad1(2),quad1(2),iacy)
      if (quad1(2) .lt. 0.) gprm(5) = 360. - gprm(5)
      call dpoint (gprm(5),gprm(5),iacy)
c
c...Calculate ending angle
c
      gprm(6) = gprm(5)
      rnum    = quad2(1) / gprm(4)
      if (rnum .gt. 1.0d0) rnum = 1.0
      if (rnum .lt. -1.0d0) rnum = -1.0
      rang   = dacos(rnum) * RAD
      call dpoint (quad2(1),quad2(1),iacy)
      call dpoint (quad2(2),quad2(2),iacy)
      if (quad2(2) .lt. 0.) rang = 360. - rang
      call dpoint (rang,rang,iacy)
c
c...Calculate direction and delta angle
c
      gprm(7) = 0.
      rdis   = rang - gprm(5)
      if (rdis .lt. -180.) rdis = 360. + rdis
      if (rdis .gt. 180.) rdis = rdis - 360.
      kprm(5) = 2
      if (rdis .lt. 0.) kprm(5) = 1
c
c...Calculate beginning quadrant
c
      call cirbnd (gprm,kprm,inum,1)
      kprm(4) = inum
c
c...Store circle axis and INDIRP point
c
      gprm(kprm(1)+10) = 0.
      gprm(kprm(2)+10) = 0.
      gprm(kprm(3)+10) = dabs(gbuf(kprm(3)+3))
      if (kprm(5) .eq. 1) gprm(kprm(3)+10) = gprm(kprm(3)+10) * (0.-1.)
      gprm(14) = clpt(1)
      gprm(15) = clpt(2)
      gprm(16) = clpt(3)
c
c...End of routine
c
 8000 return
c
c...Not a valid circular record
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirbnd (gprm,kprm,kquad,ktyp)
c
c   FUNCTION:  This routine calculates the current circular quadrant.
c              When the input angle is a quadrant boundary, then the ac-
c              tual quadrant depends on the direction (for starting
c              quadrant calcs) or on the starting quadrant (for ending
c              quadrant calcs).
c
c   INPUT:  gprm    R*8  D12 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           kfl     I*4  D1  -  1 = Calculate beginning quadrant.  2 =
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
      real*8 gprm(12)
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
c   SUBROUTINE:  circk (gprm,kprm,spt,clpt,npt,mxcl,kerr)
c
c   FUNCTION:  This routine checks the tolerancing on a circular inter-
c              polation record determines the ending quadrant and delta
c              angle.
c
c   INPUT:  gprm    R*8  D12 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           kprm    I*4  D8  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           spt     R*8  D1  -  Cl point coming from.
c
c           clpt    R*8  D245   Cl points in circular motion.
c
c           npt     I*2  D1  -  3 = MULTAX/OFF, 6 = MULTAX/ON.
c
c           mxcl    I*2  D1  -  Number of points in 'clpt'.
c
c   OUTPUT: kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine circk (gprm,kprm,spt,clpt,npt,mxcl,kerr)
c
      integer*4 kprm(8)
      integer*2 mxcl,npt,kerr
c
      real*8 gprm(12),clpt(640),spt(6)
c
      integer*4 i,inc,iend,kinc,iacy,iacy5
c
      real*8 rmch(6),quad(2),rnum,rsgn,rval,pi,rad
c
c...Initialize routine
c
      kerr   = 0
      pi     = 3.141592653589793d0
      rad    = 180.0d0 / pi
      iacy   = 4
      iacy5  = 5
c
c...Start loop
c
      kinc    = 1
      inc    = 1
      if (mxcl .ge. 7) inc = 3
      iend   = (mxcl-1) * npt + 1
c
c......Adjust point for tool axis
c
c  100     call ptrtad (clpt(kinc),rmch,ROTSTO(1,2),0)
 100      rmch(1) = clpt(kinc)
          rmch(2) = clpt(kinc+1)
          rmch(3) = clpt(kinc+2)
          if (npt .gt. 3) then
              rmch(4) = clpt(kinc+3)
              rmch(5) = clpt(kinc+4)
              rmch(6) = clpt(kinc+5)
          endif
c
c......Check for 3-axis move
c
          rnum   = dabs(spt(kprm(3))-rmch(kprm(3)))
          if (rnum .gt. .0002) go to 9000
          rmch(kprm(3)) = spt(kprm(3))
c
c......Check for rotary move
c
          if (npt .gt. 3) then
              do 300 i=1,3,1
                  call dpoint (spt(i+3),quad(1),iacy5)
                  call dpoint (rmch(i+3),quad(2),iacy5)
                  if (quad(1) .ne. quad(2)) go to 9000
  300         continue
          endif
c
c......Check radius tolerance
c
          quad(1) = rmch(kprm(1)) - gprm(kprm(1))
          quad(2) = rmch(kprm(2)) - gprm(kprm(2))
          rnum    = dsqrt(quad(1)**2 + quad(2)**2)
          if (dabs(rnum-gprm(4)) .gt. .002) go to 9000
c
c......Calculate ending angle
c
          rnum   = gprm(6)
          rsgn   = dabs(quad(1))
          if (rsgn .gt. gprm(4)) quad(1) = gprm(4) * (quad(1)/rsgn)
          rval   = quad(1) / gprm(4)
          if (rval .gt. 1.0d0) rval = 1.0
          if (rval .lt. -1.0d0) rval = -1.0
          gprm(6) = dacos(rval) * rad

          call dpoint (quad(1),quad(1),iacy)
          call dpoint (quad(2),quad(2),iacy)
          if (quad(2) .lt. 0.) gprm(6) = 360. - gprm(6)
          call dpoint (gprm(6),gprm(6),iacy)
c
c......Calculate delta angle
c
          if (kprm(5) .eq. 2) then
              if (rnum .gt. gprm(6)) rnum = rnum - 360.
          else
              if (rnum .lt. gprm(6)) rnum = rnum + 360.
          endif
          gprm(7) = gprm(7) + dabs(rnum-gprm(6))
c
c...Store end point
c
          gprm(8) = rmch(1)
          gprm(9) = rmch(2)
          gprm(10) = rmch(3)
c
c...End of loop
c
          if (kinc .eq. iend) go to 8000
          kinc    = kinc    + inc * npt
          if (kinc .gt. iend) kinc = iend
      go to 100
c
c...End of routine
c
 8000 return
c
c...Invalid circular record
c
 9000 kerr   = 1
      go to 8000
      end
c
c***********************************************************************
c
c   SUBROUTINE:  cirout (iciprm,rciprm,isn,krot,gmx,kerr)
c
c   FUNCTION:  This routine outputs a circular interpolation record to
c              the APT source file.  The type of record(s) output is
c              dependant on the *SET/APTSRC,CIRCUL command.
c
c   INPUT:  iciprm  I*4  D5  -  Output circle parameters (same format as
c                               ICIPRM).
c
c           rciprm  R*8  D16 -  Output circle parameters (same format as
c                               RCIPRM).
c
c           spt     R*8  D6  -  Starting point of circle.
c
c           isn     I*4  D1  -  ISN of circular record.
c
c           krot    I*2  D1  -  1 = XYZ-plan circular.  Points are rotated
c                               into XY-plan.
c
c           gmx     R*8  D12 -  Rotation matrix to place circle back into
c                               original plane.
c
c   OUTPUT: kerr    I*4  D1  -  Returns 1 when an error occurred.
c
c***********************************************************************
c
      subroutine cirout (iciprm,rciprm,spt,isn,krot,gmx,kerr)
c
      include 'com8a.com'
c
      integer*2 kerr,krot
      integer*4 iciprm(5),isn
c
      real*8 rciprm(16),spt(6),gmx(12)
c
      integer*2 nc
c
      real*8 a,b,d,cv(3),pt(3),secsq,co,tprm(16),tpt(3)
c
      character*80 lbuf
c
c...VX
c
      character*4 ldir(2)
      data ldir /'CLW','CCLW'/
c
c...Initialize routine
c
      kerr   = 0
c
c...Rotate circular parameters back to proper plane
c
      call conv8_8 (rciprm,tprm,16)
      if (krot .eq. 1) then
          call conent (tprm(1),gmx,3)
          call conent (tprm(8),gmx,3)
          call conent (tprm(11),gmx,4)
          call conent (tprm(14),gmx,3)
      endif
c
c...Output Varimetrix circular record
c...VX
c
      if (ifl(35) .eq. 2 .and. ifl(322) .eq. 1) then
          if (krot .eq. 0) then
              write (lbuf,5) rciprm(1),rciprm(2),rciprm(3),rciprm(11),
     1                       rciprm(12),rciprm(13),rciprm(4)
              nc     = 72
          else
              write (lbuf,7) tprm(1),tprm(2),tprm(3),tprm(11),
     1                       tprm(12),tprm(13),rciprm(4)
              nc     = 80
          endif
    5     format ('ARCDAT/',3(f12.4,','),3(f3.0,','),f11.4)
    7     format ('ARCDAT/',3(f11.4,','),3(f7.4,','),f11.4)
          call putapt (lbuf,nc)
          write (lbuf,6) ldir(iciprm(5)),rciprm(8),rciprm(9),rciprm(10),
     1                   rciprm(7)
    6     format ('ARCMOV/',a4,',',3(f12.4,','),f12.4)
          nc     = 72
          call putapt (lbuf,nc)
c
c...Output circle comment with
c...standard motion record
c
      else if (ifl(261) .ne. 1) then
          kerr   = 1
          if (krot .eq. 0) then
              write (lbuf,10) rciprm(1),rciprm(2),rciprm(3),rciprm(11),
     1                        rciprm(12),rciprm(13),rciprm(4)
              nc     = 72
          else
              write (lbuf,11) tprm(1),tprm(2),tprm(3),tprm(11),
     1                        tprm(12),tprm(13),rciprm(4)
              nc     = 80
          endif
   10     format ('$$ CIRCLE/',3(f12.4,','),3(f3.0,','),f11.4)
   11     format ('$$ CIRCLE/',3(f11.4,','),3(f7.4,','),f10.4)
          call putapt (lbuf,nc)
c
c...Put out circular interpolation as
c...PSIS/
c...INDIRP/
c...GF/(CI/...),ON,(PL/...)
c
      else if (ifl(308) .ne. 1 .and. ifl(308) .ne. 2) then
          is1    = iciprm(1)
          is2    = iciprm(2)
          is3    = iciprm(3)
          ip1    = is1    + 7
          ip2    = is2    + 7
          ip3    = is3    + 7
c
c......Write out PSIS/plane statement
c
          cv(is1) = tprm(is1+10)
          cv(is2) = tprm(is2+10)
          if (krot .eq. 0) then
              cv(is3) = dabs(rciprm(is3+10))
          else
              cv(is3) = tprm(is3+10)
          endif
          if (ifl(319) .eq. 0) then
              write (lbuf,510) cv,rciprm(ip3)
              nc     = 51
          else
              write (lbuf,511) cv,rciprm(ip3)
              nc     = 58
          endif
  510     format ('PSIS / (PLANE/',3(f7.4,','),f12.4,')')
  511     format ('PSIS / (PLANE/',3(f9.6,','),f13.6,')')
          call putapt (lbuf,nc)
c
c......Write out INDIRP/x,y,z statment
c
          write (lbuf,520) tprm(14),tprm(15),tprm(16)
          nc     = 45
  520     format ('INDIRP/',2(f12.4,','),f12.4)
          call putapt (lbuf,nc)
c
c......Write out motion command
c
          if (ifl(319) .eq. 0) then
              write (lbuf,530) tprm(1),tprm(2),tprm(3)
              nc     = 65
          else
              write (lbuf,531) tprm(1),tprm(2),tprm(3)
              nc     = 71
          endif
  530     format ('TLON,GOFWD/(CIRCLE/CANON,',3(f12.4,','),'$')
  531     format ('TLON,GOFWD/(CIRCLE/CANON,',3(f14.6,','),'$')
          call putapt (lbuf,nc)
          if (rciprm(7) .gt. 170.d0) then
              if (ifl(319) .eq. 0) then
                  write (lbuf,540) tprm(11),tprm(12),tprm(13),
     1                             rciprm(4)
                  nc     = 63
              else
                  write (lbuf,541) tprm(11),tprm(12),tprm(13),
     1                             rciprm(4)
                  nc     = 71
              endif
              a      = rciprm(ip1) - rciprm(is1)
              b      = rciprm(ip2) - rciprm(is2)
          else
              if (ifl(319) .eq. 0) then
                  write (lbuf,545) tprm(11),tprm(12),tprm(13),
     1                             rciprm(4)
                  nc     = 60
              else
                  write (lbuf,546) tprm(11),tprm(12),tprm(13),
     1                             rciprm(4)
                  nc     = 68
              endif
              a      = rciprm(ip2) - rciprm(is2)
              b      = rciprm(is1) - rciprm(ip1)
          endif
  540     format (18x,3(f7.4,','),f12.4,'),TANTO,$')
  541     format (18x,3(f9.6,','),f14.6,'),TANTO,$')
  545     format (18x,3(f7.4,','),f12.4,'),ON,$')
  546     format (18x,3(f9.6,','),f14.6,'),ON,$')
          call putapt (lbuf,nc)
c
c.........Calculate and output check plane
c
          d      = a * rciprm(ip1) + b * rciprm(ip2)
          secsq  = a**2 + b**2
          if (secsq .lt. .000001) secsq = .000001
          co     = 1.d0 / dsqrt(secsq)
          pt(is1) = a      * co
          pt(is2) = b      * co
          pt(is3) = 0.
          d      = d      * co
          if (krot .eq. 1) call conent (pt,gmx,4)
          if (ifl(319) .eq. 0) then
              write (lbuf,550) pt(1),pt(2),pt(3),d
              nc     = 70
          else
              write (lbuf,551) pt(1),pt(2),pt(3),d
              nc     = 72
          endif
  550     format ('           (PLANE/',3(f12.4,','),f12.4,')')
  551     format ('         (PLANE/',3(f13.6,','),f13.6,')')
          call putapt (lbuf,nc)
c
c......Vericut circular record
c
      else
          call conv8_8 (spt,tpt,3)
          if (krot .eq. 1) call conent (tpt,gmx,3)
  800     if (krot .eq. 0) then
              write (lbuf,810) rciprm(1),rciprm(2),rciprm(3),rciprm(11),
     1                     rciprm(12),rciprm(13),rciprm(4)
              nc     = 69
          else
              write (lbuf,811) tprm(1),tprm(2),tprm(3),tprm(11),
     1                     tprm(12),tprm(13),rciprm(4)
              nc     = 80
          endif
  810     format ('CIRCLE/',3(f12.4,','),3(f3.0,','),f11.4)
  811     format ('CIRCLE/',3(f11.4,','),3(f7.4,','),f11.4)
          call putapt (lbuf,nc)
c
          nc     = 0
          if (ifl(308) .eq. 2) call aptisn (isn,lbuf,nc)
c
c...when we deal with FSR60971, when
c...Ci1=CL/0,0,1		PL1=pl/0,0,1,0  pl2=pl/0,1,0,0
c...gf/ci1,on,2 (or even number), intof, pl2
c...will have rciprm(7) = 360.012 and out put one more circul record
c...we only out put record when it >360+170 concider
c...we use   if (rciprm(7) .gt. 170.d0) above
c
c...          if (rciprm(7) .gt. 360.) then
          if (rciprm(7) .gt. 360 + 170.d0) then
              write (lbuf(nc+1:80),820) tpt(1),tpt(2),tpt(3)
              nc     = 48
              call putapt (lbuf,nc)
              rciprm(7) = rciprm(7) - 360.
              go to 800
          else
              write (lbuf(nc+1:80),820) tprm(8),tprm(9),tprm(10)
  820         format ('GOTO  /',2(f12.4,','),f12.4)
              nc     = 48
              call putapt (lbuf,nc)
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
c   SUBROUTINE:  dpoint (ginp,gout,kacy)
c
c   FUNCTION:  This routine rounds off a real number to the specified
c              accuracy.
c
c   INPUT:  ginp    R*8  D1  -  Number to round off.
c
c   OUTPUT: gout    R*8  D1  -  Rounded off number.
c
c           kacy    I*4  D1  -  Accuracy to round off number at.
c
c***********************************************************************
c
      subroutine dpoint (ginp,gout,kacy)
c
      real*8 ginp,gout
c
      integer*4 kacy
c
c...Round off number
c
      gout   = dnint(ginp*10.0D0**kacy) / 10.0D0**kacy
      return
      end
