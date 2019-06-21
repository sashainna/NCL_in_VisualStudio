c
c***********************************************************************
c
c   FILE NAME:  fedmsc
c   CONTAINS:
c               acltim  aclpos  aclcir  clfed   clspn
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        fedmsc.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:06
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE: acltim (gsto,gaxs,gtim,kfl)
c
c   FUNCTION:  Adjusts the machining time for the acceleration and
c              deceleration of the machine axes.
c
c   INPUT:  gsto    R*8  D10  -  Previous machine axes positions.
c
c           gaxs    R*8  D10  -  Current machine axes positions.
c
c           gtim    R*8  D1   -  Calculated machining time for this move.
c
c           kfl     I*4  D1   -  0 = Assume next move requires an exact
c                                stop.  1 = Calculate actual speed of
c                                next move.
c
c   OUTPUT: gtim    R*8  D1   -  Adjusted machining time for acceleration
c                                deceleration.
c
c***********************************************************************
c
      subroutine acltim (gsto,gaxs,gtim,kfl)
c
      include 'post.inc'
c
      integer*4 kfl
c
      real*8 gaxs(10),gsto(10),gtim
c
      equivalence (ICIPRM,KPOSMP(1230)), (ISCIRC,KPOSMP(1238))
      equivalence (IACLFL,KPOSMP(1732))
c
      integer*4 ISCIRC,ICIPRM(8),IACLFL
c
      equivalence (RCIPRM,POSMAP(2049))
      equivalence (ACLDCL,POSMAP(2099)), (CURSPD,POSMAP(2109))
c
      real*8 CURSPD(10),ACLDCL(10),RCIPRM(25)
c
      integer*4 i,nstp,dstp(7)
c
      real*8 axs(10),rdis(3),rtim(3),rspd1,rspd2(10),vfed,vtim,rsto(10),
     1       rtmp
c
      data nstp /7/, dstp /1,2,3,1007,1033,1054,1055/
c
c...Initialize routine
c
      if (gtim .eq. 0. .or. IACLFL .ne. 1) go to 8000
      if (ISCIRC .eq. 1) then
          call aclcir (RCIPRM,ICIPRM,gaxs,rsto,1)
      else
          call copyn (gsto,rsto,10)
      endif
c
c...Calculate next axes positions and speeds
c
      if (kfl .eq. 1) call aclpos (rsto,gaxs,axs,rspd2)
c
c...Adjust machining time for accel/decel requirements
c
      do 1000 i=1,10,1
c
c......Calculate current axis speed
c
          rspd1  = 0.
          if (gaxs(i) .ne. rsto(i)) then
              rdis(1) = 0.
              rdis(2) = dabs(gaxs(i)-rsto(i))
              rdis(3) = 0.
              rtim(1) = 0.
              rtim(2) = gtim
              rtim(3) = 0.
              rspd1 = rdis(2) / gtim
              if (kfl .eq. 0) rspd2(i) = 0.
c
c......An acceleration rate is not specified for this axis
c......Just average out the feed rates
c
              if (ACLDCL(i) .eq. 0.) then
                  if (rspd2(i) .lt. rspd1) then
                      vfed   = (rspd1 + CURSPD(i) + rspd2(i)) / 3.
                  else
                      vfed   = (rspd1 + CURSPD(i)) / 2.
                  endif
                  rtim(2) = rdis(2) / vfed
c
c......Perform acceleration calculations
c......Calculate time to bring tool up to speed
c
              else
                  if (rspd1 .ne. CURSPD(i)) then
                      vfed   = dabs(rspd1-CURSPD(i))
                      vtim   = vfed / (ACLDCL(i)*60.)
                      if (vtim .lt. rtim(2)) then
                          vfed   = (rspd1 + CURSPD(i)) / 2.
                          rtim(1) = vtim
                          rdis(1) = vtim   / vfed
                          rdis(2) = rdis(2) - rdis(1)
                          rtim(2) = rdis(2) / rspd1
                      else
                          vfed   = (rspd1 + CURSPD(i)) / 2.
                          rtmp   = vfed * vtim
                          rspd1  = CURSPD(i) + (rdis(2) / rtmp) * vfed
                          rtim(2) = rdis(2) / rspd1
                      endif
                  endif
c
c......Perform deceleration calculations
c......Calculate time to slow down tool into corner
c
                  if (rspd2(i) .lt. rspd1) then
                      vfed   = rspd1  - rspd2(i)
                      vtim   = vfed / (ACLDCL(i)*60.)
                      if (vtim .lt. rtim(2)) then
                          vfed   = (rspd1 + CURSPD(i)) / 2.
                          rtim(3) = vtim
                          rdis(3) = vtim   / vfed
                          rdis(2) = rdis(2) - rdis(3)
                          rtim(2) = rdis(2) / rspd1
                      else
                          vfed   = (rspd1 + rspd2(i)) / 2.
                          rtmp   = vfed * vtim
                          rspd1  = rspd2(i) + (rdis(2) / rtmp) * vfed
                          rtim(2) = rdis(2) / rspd1
                      endif
                  endif
              endif
c
c......Update calculated time
c
              vtim   = rtim(1) + rtim(2) + rtim(3)
              if (vtim .gt. gtim) gtim = vtim
          endif
c
c......Store current speeds
c
          CURSPD(i) = rspd1
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  aclpos (gaxs,gnxt,gspd)
c
c   FUNCTION:  This routine calculates the next position for accel/decel
c              calculations in addition to the speeds of the next move.
c
c   INPUT:  gsto    R*8  D10  -  Previous axes position.
c
c           gaxs    R*8  D10  -  Current axes position.
c
c   OUTPUT: gnxt    R*8  D10  -  Next axes position.
c
c           gspd    R*8  D10  -  Individual axis speeds for next move.
c
c***********************************************************************
c
      subroutine aclpos (gsto,gaxs,gnxt,gspd)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (IPSTWD,KPOSMP(0006))
      equivalence (NPT   ,KPOSMP(0059)), (ICIPRM,KPOSMP(1230))
      equivalence (NEXCIR,KPOSMP(1275)), (IFITYP,KPOSMP(3150))
c
      integer*4 ITYPE,ISUBT,NPT,NEXCIR,ICIPRM(8),IFITYP,MXCL,IPSTWD(50)
c
      equivalence (PSTWD ,POSMAP(0441)), (CLPT  ,POSMAP(0491))
      equivalence (VECSAV,POSMAP(1372)), (ROTBAS,POSMAP(1435))
      equivalence (RCIPRM,POSMAP(2049)), (RPM   ,POSMAP(3307))
      equivalence (FEDLMT,POSMAP(3528)), (FEED  ,POSMAP(3547))
      equivalence (RAPLMT,POSMAP(3567))
c
      real*8 CLPT(240),VECSAV(3),ROTBAS(4),RCIPRM(25),FEDLMT(10),
     1       FEED(4),RPM,PSTWD(50),RAPLMT(10)
c
      real*8 gsto(10),gaxs(10),gnxt(10),gspd(10)
c
      integer*4 ierr,isw,i,nstp,dstp(6),fityp,idir,irp
c
      real*8 rmch(3,3),tmch(3,4),tv(3),laxs(6),rang(20,2),ndist,mdis,
     1       mtim,rfed,rnum1,rnum2,rnum1a,rnum2a,srpm
c
      data nstp /6/, dstp /1,2,3,1054,1055,1075/
c
c...Initialize routine
c
      call alladr (gsto,laxs,tmch,rang,tv,5,3)
      call copyn (tmch(1,3),rmch(1,1),3)
      call alladr (gaxs,laxs,tmch,rang,tv,5,3)
      call copyn (tmch(1,3),rmch(1,2),3)
      rfed   = FEED(1)
      fityp  = IFITYP
      srpm   = RPM
      irp    = 0
c
c...Calculate next position
c......Buffered circular motion
c
      if (NEXCIR .eq. 2) then
          call aclcir (RCIPRM,ICIPRM,gaxs,gnxt,2)
c
c......Get next cl record
c
      else
          isw    = 0
  100     call clsamp (isw)
  150     isw    = 1
c
c......Post word
c.........Check for command that siginifies
c.........a full stop
c
          if (ITYPE .eq. 2000 .or. ITYPE .eq. 14000) then
              irp    = 0
              do 200 i=1,nstp,1
                  if (ISUBT .eq. dstp(i) .or. ITYPE .eq. 14000) then
                      rmch(1,3) = rmch(1,2)
                      rmch(2,3) = rmch(2,2)
                      rmch(3,3) = rmch(3,2)
                      go to 1000
                  endif
  200         continue
c
c.........RAPID
c
              if (ISUBT .eq. 5) then
                  rfed   = 0.
                  irp    = 1
                  do 300 i=1,6,1
                      if (RAPLMT(i) .gt. rfed) rfed = RAPLMT(i)
  300             continue
c
c.........FEDRAT
c
              else if (ISUBT .eq. 1009) then
                  call clfed (IPSTWD,PSTWD,MXCL,fityp,srpm,rfed)
c
c.........SPINDL
c
              else if (ISUBT .eq. 1031) then
                  call clspn (IPSTWD,PSTWD,MXCL,srpm,idir)
              endif
              go to 100
c
c......Motion record
c
          else if (ITYPE .eq. 5000 .or. ITYPE .eq. 5200) then
c
c.........Adjust point to rotaries
c
  500         tmch(1,2) = CLPT(1)
              tmch(2,2) = CLPT(2)
              tmch(3,2) = CLPT(3)
              if (NPT .eq. 3) then
                  tv(1) = VECSAV(1)
                  tv(2) = VECSAV(2)
                  tv(3) = VECSAV(3)
              else
                  tv(1) = CLPT(4)
                  tv(2) = CLPT(5)
                  tv(3) = CLPT(6)
              endif
              call tlaxis (tmch,tv,laxs,gnxt,rang,ROTBAS,0,0,ierr)
c
c.........Save points
c
              rmch(1,3) = tmch(1,3)
              rmch(2,3) = tmch(2,3)
              rmch(3,3) = tmch(3,3)
c
c......Unrecognized record
c
          else
              go to 100
          endif
c
c......Reset clfile pointers
c
 1000     call clsamp (-1)
      endif
c
c...Calculate move time
c
      mdis   = ndist(rmch(1,3),rmch(1,2))
      if (mdis .eq. 0. .or. rfed .eq. 0.) then
          mtim   = 0.
          do 1100 i=1,10,1
              gspd(i) = 0.
 1100     continue
      else
          mtim   = mdis   / rfed
c
c...Calculate individual axis speeds
c
          do 1200 i=1,10,1
              rnum1  = gaxs(i) - gsto(i)
              rnum2  = gnxt(i) - gaxs(i)
              rnum1a = dabs(rnum1)
              rnum2a = dabs(rnum2)
              if (rnum2a .eq. 0) then
                  gspd(i) = 0.
              else if (rnum1 .ne. 0. .and.
     1            rnum1/rnum1a .ne. rnum2/rnum2a) then
                  gspd(i) = 0.
              else
                  gspd(i) = dabs(gnxt(i)-gaxs(i)) / mtim
                  if (gspd(i) .gt. FEDLMT(i) .and. irp .eq. 0)
     1                gspd(i) = FEDLMT(i)
                  if (gspd(i) .gt. RAPLMT(i) .and. irp .eq. 1)
     1                gspd(i) = RAPLMT(i)
              endif
 1200     continue
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  aclcir (gprm,kprm,gaxs,gsto,kfl)
c
c   FUNCTION:  This routine calculates the point coming from or going
c              to when exiting our entering a circle.  This point is
c              based on the tangency of the circle at the end point
c              and will be the distance of the circular move back from
c              the exit point or forward from the entry point.
c
c   INPUT:  gprm    R*8  D25  -  Input circle parameters (same format as
c                                RCIPRM).
c
c           kprm    I*4  D8   -  Input circle parameters (same format as
c                                ICIPRM).
c
c           gaxs    R*8  D10  -  Current position (circular end point).
c
c           kfl     I*4  D1   -  1 = Calculate from point based on exit
c                                point, 2 = Calculate next point based
c                                on entry point.
c
c   OUTPUT: gsto    R*8  D10 -   Pseudo from location tangent to the
c                                circle exit.
c
c***********************************************************************
c
      subroutine aclcir (gprm,kprm,gaxs,gsto,kfl)
c
      include 'post.inc'
c
      equivalence (PI    ,POSMAP(0001))
c
      real*8 PI
c
      integer*4 kprm(8),kfl
c
      real*8 gprm(25),gaxs(10),gsto(10)
c
      integer*4 is
c
      real*8 d,d1,d2,vec,xpt(2),ypt(2),laxs(6),tmch(3,4),rot(20,2),
     1       tv(3)
c
c...Calculate working coordinates
c
      call alladr (gaxs,laxs,tmch,rot,tv,5,3)
c
c...Calculate vector from current position
c...to the center point of the circle
c
      d1     = gprm(kprm(1)) - tmch(kprm(1),3)
      d2     = gprm(kprm(2)) - tmch(kprm(2),3)
c
c...Don't know length of move
c...so base it on the circle radius
c...with a minimum of 1 unit
c
      vec    = dsqrt(d1**2 + d2**2)
      d      = (2.d0 * PI * gprm(4)) * (gprm(7)/360.)
      d1     = d * (d1 / vec)
      d2     = d * (d2 / vec)
c
c...Calculate tangency points
c
      xpt(1) = tmch(kprm(1),3) - d2
      xpt(2) = tmch(kprm(1),3) + d2
      ypt(1) = tmch(kprm(2),3) + d1
      ypt(2) = tmch(kprm(2),3) - d1
c
c...Use direction of move to determine
c...which tangency point to use
c
      is     = 2
c
c...ZXPLAN
c
      if (kprm(3) .eq. 2) then
          if (kprm(5) .ne. kfl)  is = 1
c
c...XYPLAN & YZPLAN
c
      else
          if (kprm(5) .eq. kfl) is = 1
      endif
c
c...Assign point
c
      tmch(kprm(1),3) = xpt(is)
      tmch(kprm(2),3) = ypt(is)
      call alladj (tmch,laxs,gsto,rot,3,5)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clfed (kclw,gclw,kmxcl,kftyp,grpm,gfed)
c
c   FUNCTION:  This routine parses a feed rate command from a clfile and
c              returns the programmed feed rate.
c
c   INPUT:  kclw    I*4  Dn  -  Post command minor words.
c
c           gclw    R*8  Dn  -  Post command parameters.
c
c           kmxcl   I*4  D1  -  Number of parameters in command.
c
c           grpm    R*8  Dn  -  Current spindle RPM.
c
c   OUTPUT: kftyp   I*2  D1  -  1 = IPM feed rate.  2 = IPR feed rate.
c
c           gfed    R*8  D1  -  Programmed feed rate.
c
c***********************************************************************
c
      subroutine clfed (kclw,gclw,kmxcl,kftyp,grpm,gfed)
c
      integer*4 kclw(50),kmxcl,kftyp
c
      real*8 gclw(50),grpm,gfed
c
c...FEDRAT/feed
c
      if (kclw(1) .eq. 0) then
          gfed   = gclw(1)
          if (kmxcl .ge. 2 .and. kclw(2) .ne. 0) then
              if (kclw(2) .eq. 73 .or. kclw(2) .eq. 315) kftyp = 1
              if (kclw(2) .eq. 74 .or. kclw(2) .eq. 316) then
                  kftyp = 2
                  if (grpm .ne. 0) gfed = grpm * gclw(1)
              endif
          endif
c
c...FEDRAT/IPM,feed
c
      else if (kclw(1) .eq. 73 .or. kclw(1) .eq. 315 .and.
     1         kmxcl .ge. 2 .and. kclw(2) .eq. 0) then
          gfed   = gclw(2)
          kftyp  = 1
c
c...FEDRAT/IPR,feed
c
      else if (kclw(1) .eq. 74 .or. kclw(1) .eq. 316 .and.
     1         kmxcl .ge. 2 .and. kclw(2) .eq. 0) then
          if (grpm .ne. 0) gfed = grpm * gclw(2)
          kftyp  = 2
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  clspn (kclw,gclw,kmxcl,grpm,kdir)
c
c   FUNCTION:  This routine parses a spindle command from a clfile and
c              returns the programmed spindle speed.
c
c   INPUT:  kclw    I*4  Dn  -  Post command minor words.
c
c           gclw    R*8  Dn  -  Post command parameters.
c
c           kmxcl   I*4  D1  -  Number of parameters in command.
c
c   OUTPUT: grpm    R*8  D1  -  Programmed spindle speed.
c
c           kdir    I*4  D1  -  0 = CLW, 1 = CCLW.
c
c***********************************************************************
c
      subroutine clspn (kclw,gclw,kmxcl,grpm,kdir)
c
      integer*4 kclw(50),kmxcl,kdir
c
      real*8 gclw(50),grpm
c
      integer*4 ist
c
c...SPINDL/OFF
c
      if (kclw(1) .eq. 72 .or. kclw(1) .eq. 246) then
          grpm = 0.
          ist = kmxcl + 1
c
c...SPINDL/speed
c
      else if (kclw(1) .eq. 0) then
          grpm  = gclw(1)
          ist    = 2
c
c...SPINDL/RPM,speed
c
      else if (kclw(1) .eq. 78 .or. kclw(1) .eq. 115 .and.
     1         kmxcl .ge. 2 .and. kclw(2) .eq. 0) then
          grpm   = gclw(2)
          ist    = 3
      endif
c
c...SPINDL/...,dir
c
      if (ist .le. kmxcl) then
          if (kclw(ist) .eq. 60) kdir = 0
          if (kclw(ist) .eq. 59) kdir = 1
      endif
c
c...End of routine
c
 8000 return
      end
