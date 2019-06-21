c
c***********************************************************************
c
c     FILE NAME: mchlin1.f
c
c     CONTAINS:  mchlin_new  mwctrl  mwlnrz  mwadj  mwdrck  mwgenp
c                mwgrat      mwgtln  mwtvmp  mwudir
c
c     COPYRIGHT 2006 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mchlin1.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:35:31
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: mchlin_new (kflg,kerr,klin)
c
c     FUNCTION:  This routine generates intermediate points for
c                multi-axis motion when linearization is enabled.
c                The logic in these routines has been optimized for a more
c                direct solution to the linearization points in
c                relationship to the actual rotary axes.
c
c                This routine actually handles the pushing and popping
c                of the linearized points on and off the stack and calls
c                the 'mwctrl' routine to handle linearization.
c
c     INPUT:   kflg     I*4  D1  -  Set to 0 on first call, controlled by
c                                   this routine on subsequent calls.  See
c                                   OUTPUT description.
c
c              kerr     I*4  D1  -  tlaxis call error status: 3 - not the
c                                   shortest rotary motion, retract tool
c                                   when limits are reached. n - normal
c                                   linearization.
c
c              klin     I*4  D1  -  1 = Move should be linearized.
c                                   2 = Don't linearize move, just check
c                                       for longest route retraction.
c
c     OUTPUT:  kflg     I*4  D1  -  0 - last point has been generated.
c                                   1 - In the midst of linearization.
c                                   2 - End of linearization.  Pop last
c                                       position off of stack.
c                                   3 - Rapid motion is being linearized.
c                                       Another point is required from the
c                                       rapid adjustment routine.
c
c***********************************************************************
c
      subroutine mchlin_new (kflg,kerr,klin)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IFWDFL,KPOSMP(0832)), (MACHTP,KPOSMP(1201))
      equivalence (HLDBCK,KPOSMP(1223)), (INCR  ,KPOSMP(1226))
      equivalence (HLDFLG,KPOSMP(1622)), (IFLNRS,KPOSMP(1731))
      equivalence (IRAP  ,KPOSMP(3199)), (ILINPT,KPOSMP(3200))
      equivalence (IFSVLN,KPOSMP(3207))
c
      integer*4 HLDFLG,IFLNRS,IRAP,ILINPT,INCR,IFWDFL,MACHTP,
     1          HLDBCK,IFSVLN
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (ROTBAS,POSMAP(1435)), (HLDMCH,POSMAP(2141))
      equivalence (HLDVEC,POSMAP(2177))
      equivalence (LNRTOL,POSMAP(2251)), (RETFED,POSMAP(2254))
      equivalence (ROTBSV,POSMAP(2275)), (PRESTO,POSMAP(2280))
      equivalence (FDSVLN,POSMAP(2377))
      equivalence (FWDDIR,POSMAP(4580)), (FUZZ8 ,POSMAP(4913))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),VECSAV(3),ROTANG(20,2),STONUM(3,4),TLVEC(3),
     1       LINAXS(6),ROTSTO(20,2),AXSOUT(10),ROTBSV(4),HLDMCH(3,4),
     2       HLDROT(20,2),HLDVEC(3),ROTBAS(4),RETFED(4),FUZZ8,FWDDIR(3),
     3       PRESTO(6),FDSVLN,LNRTOL(3)
c
      integer*4 kerr,kflg,klin
c
      integer*4 ifl,iary(10),icnt,ify,ifsav
c
      real*8 aout(10),rsto(20,2),vsto(3),rnum,fsav
c
c...Don't linearize if tool axis vector
c...does not change
c
      ILINPT = 0
      if (kflg .eq. 0) then
          call betvec (TLVEC,VECSAV,rnum)
          if (rnum .lt. FUZZ8) go to 8000
      endif
c
c...Don't linearize rapid moves unless instructed to
c...When linearized, generate rapid motion first
c...In this routine rather than in 'motion'
c...Otherwise results are stair-stepped moves
c
      if (IRAP .gt. 0 .and. klin .ne. 2) then
          if (IRAP .ne. 3 .and. LNRTOL(2) .eq. 0.) then
              IMWRFL = 0
              go to 8000
          endif
          if (kflg .eq. 0 .or. kflg .eq. 3) then
              call rapmot (iary,icnt,IMWRFL)
              kflg   = 0
          endif
      else
          IMWRFL = 0
      endif
c
c...Initialize routine
c
  100 ifl    = kflg
      IMWFED = 0
c
c...Force incremental mode on position from stack
c
      if (ifl .ne. 0 .and. IMWINC .ne. 0) IMWINC = IMWINC + 1
c
c...Linearization is finished
c...Pop last position from the stack
c
      if (ifl .eq. 2) then
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
          call setbas (ROTANG(1,2),ROTBAS)
          if (IMWFSV .ne. 0) call linfed (IMWFSV,RETFED)
          kflg   = 0
          ILINPT = 1
          go to 8000
      endif
c
c...Save point coming from
c......Point is stored on stack
c
      if (ifl .ne. 0) then
          call copyn (STONUM(1,2),PRESTO,3)
          call copyn (VECSAV,PRESTO(4),3)
          call cpyrot (ROTSTO,rsto)
          call copyn (VECSAV,vsto,3)
          call cpyrot (ROTSTL,ROTSTO)
          call setbas (ROTSTL(1,2),ROTBSV)
          call copyn (VECSAL,VECSAV,3)
c
c......First time here
c......Initialize variables
c
      else
         IMWEFL = 2
         IMWINC = 0
         IMWLMT = 0
         IMWRTA = 0
         IMWXFL = 0
         if (kerr .ne. 0) IMWEFL = 0
c
c......Point is from held back motion
c
          if (HLDFLG .eq. 1) then
              call copyn (HLDMCH,STONUL,12)
              call copyn (HLDVEC,VECSAL,3)
              call cpyrot (HLDROT,ROTSTL)
              call setbas (ROTSTL(1,2),ROTBSV)
c
c......Point is from standard previous location
c
          else
              call copyn (STONUM,STONUL,12)
              call copyn (VECSAV,VECSAL,3)
              call cpyrot (ROTSTO,ROTSTL)
          endif
c
c......Calculate forward vector for blade machine
c
          if (MACHTP .eq. 3) then
              call gtfwvc (MCHNUM(1,2),TLVEC,STONUL(1,2),VECSAL,FWDDIR,
     1                     IFWDFL)
          endif
      endif
c
c...Create linearizion move
c...if necessary
c
      if (IMWLMT .eq. 7) then
          IMWLMT = 0
          IMWFED = 5
      endif
      call mwctrl (kflg,kerr,klin)
c
c...Pop previously store linearized point
c...from stack
c
      call copyn (AXSOUT,aout,10)
      if (kflg .eq. 1) then
          call copyn (MCHNUM,STONUL,12)
          call cpyrot (ROTANG,ROTSTL)
          call copyn (TLVEC,VECSAL,3)
          call copyn (ROTBAS,ROTBSV,4)
      endif
      if (ifl .eq. 1) then
          ify    = IMWFSV
          call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
          call setbas (ROTANG(1,2),ROTBAS)
          call cpyrot (rsto,ROTSTO)
          call copyn (vsto,VECSAV,3)
      else
          ify    = 0
      endif
c
c...Push linearized point onto stack
c
      if (kflg .eq. 1 .or. ifl .eq. 1) then
          IMWFSV = IMWFED
          call pshaxs (aout,TLVEC)
          if (kflg .eq. 0) kflg = 2
          if (ifl .eq. 0) go to 100
      endif
c
c...Set feed rate when retract procedure is active
c
      if (IFLNRS .eq. 1) call linfed (4,RETFED)
      if (ify .ne. 0) then
          if (ify .ne. 5) IFLNRS = 1
          fsav   = FDSVLN
          ifsav  = IFSVLN
          call linfed (ify,RETFED)
          FDSVLN = fsav
          IFSVLN = ifsav
      endif
c
c...End of routine
c......Last point generated was with Rapid in effect
c......Get the next Rapid adjusted point on next entry
c
 8000 if (kflg .eq. 0 .and. IMWRFL .ne. 0) kflg = 3
      if (kflg .ne. 0) ILINPT = 1
c
c......Set/Reset incremental mode
c......if required from longest route positioning
c
      if (IMWINC .eq. 2) then
          INCR   = 2
      else if (IMWINC .eq. 3) then
          INCR   = 1
          IMWINC = 0
      endif
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwctrl (kflg,kerr,klin)
c
c     FUNCTION:  This handles the insertion of "special" points when
c                linearizing and calls the 'mwlnrz' routine to calculate
c                the standard linearized points.  The special points
c                that can be inserted are as follows.
c
c                1.  Rotary axis apex point.
c                2.  Axes limit point.
c                3.  Retract/Position/Plunge procedure.
c                4.  C-axis linearization at apex point.
c                5.  Linearized points.
c
c     INPUT:   kflg     I*4  D1  -  Set to 0 on first call, controlled by
c                                   this routine on subsequent calls.  See
c                                   OUTPUT description.
c
c              kerr     I*4  D1  -  tlaxis call error status: 3 - not the
c                                   shortest rotary motion, retract tool
c                                   when limits are reached. n - normal
c                                   linearization.
c
c              klin     I*4  D1  -  1 = Move should be linearized.
c                                   2 = Don't linearize move, just check
c                                       for longest route retraction.
c
c     OUTPUT:  kflg     I*4  D1  -  0 - last point has been generated.
c                                   1 - In the midst of linearization.
c
c***********************************************************************
c
      subroutine mwctrl (kflg,kerr,klin)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (LRTRCT,KPOSMP(1278)), (NROT  ,KPOSMP(1366))
      equivalence (IRTINC,KPOSMP(1461))
c
      integer*4 LRTRCT,IRTACT(2),NROT,IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTANG,POSMAP(5173)), (ROTBAS,POSMAP(1435))
      equivalence (ROTBSV,POSMAP(2275))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),ROTBAS(4),LINAXS(6),
     1       AXSOUT(10),ROTBSV(4)
c
      integer*4 kerr,kflg,klin
c
      integer*4 ierr,itva,ierset,ir1,ir2,inc,iflg,inosat,ip1
c
      real*8 rot1(20,2),dlt
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
c...Initialize routine
c
      LINERR = 0
      ierset = 0
      inosat = 0
      itva   = 0
      iflg   = kflg
      ip1    = IRTINC(ir1)
	if (kposmp(1) .eq. 75) then
		ip1 = irtinc(ir1)
	endif
c
c...1st time here
c...Initialize variables
c
      if (kflg .eq. 0) then
c
c......Save user defined direction
c
          call mwudir (0)
c
c......Adjust tool axis when within tolerance
c......of apex point of Head carrier or Table rider
c
          if (NROT .gt. 1) call mwadj (TLVEC,VECSAL,itva,kerr)
c
c......Move takes longest route due to user defined direction
c.........If retract logic is active, then retract tool
c
          if (kerr .eq. 6) then
              inosat = 1
              if (LRTRCT .ne. 0) then
                  IMWLMT = 2
                  call mwlimt (IMWLMT)
c
c.........Otherwise output error message,
c.........Calculate new ending position
c.........and linearize normally
c
              else
                  kerr   = 0
                  call psterr (2,'ROTNOSAT','NOTDESIR',-1)
                  call copyn (ROTBSV,ROTBAS,4)
                  call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                         1,0,kerr)
              endif
          endif
c
c......Linearization is not active
c......retract tool at current location
c
          if (klin .eq. 2) IMWLMT = 2
c
c......Determine if move crosses apex point
c......If so, then the first part of motion will be changed
c......so that it stops at apex point
c......If user forced longest route, then
c......do not perform this logic
c
          if (IMWLMT .ne. 2 .and. NROT .gt. 1) call mwapex (IMWXFL,kerr)
c
c......Save point going to
c
          call copyn (MCHNUM,MCHNUL,12)
          call copyn (TLVEC,TLVECL,3)
          call cpyrot (ROTANG,ROTANL)
c
c...Otherwise
c
      else
c
c......Last move was forced to apex point
c......Recalculate final location
c
          if (IMWXFL .eq. 2) then
              call setbas (ROTSTL(1,2),ROTBAS)
              call tlaxis (MCHNUL,TLVECL,LINAXS,AXSOUT,ROTANL,ROTBAS,
     1                     1,0,kerr)
              IMWXFL = 0
          endif
c
c......Restore point going to
c
          call copyn (MCHNUL,MCHNUM,12)
          call copyn (TLVECL,TLVEC,3)
          call cpyrot (ROTANL,ROTANG)
          call setbas (ROTANG(1,2),ROTBAS)
c
c......Linearization is off and
c......Tool retraction sequence is over
c
          if (klin .eq. 2 .and. IMWLMT .eq. 0) then
              kflg   = 0
              call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                     1,IMWEFL,kerr)
              go to 8000
          endif
      endif
c
c......Move takes longest route due to limit error
c......Move to limit first
c
      if (kerr .eq. 3) then
          call mwlimt (IMWLMT)
          kerr   = 0
          if (IMWLMT .eq. 1) then
              call copyn (MCHNUM,MCHNUL,12)
              call copyn (TLVEC,TLVECL,3)
              call cpyrot (ROTANG,ROTANL)
          endif
      endif
c
c...Retract logic is active
c
      if (IMWLMT .gt. 1) then
          if (LRTRCT .ne. 0) then
              call mwretr (1,IMWLMT,kflg,itva)
              if (IMWLMT .eq. 4 .and. klin .ne. 2)
     1            call psterr (2,'LONGRTE','NOTDESIR',-1)
c
c...Retract is disabled
c
          else
              IMWLMT = 0
              call psterr (2,'LONGRTE','NOTDESIR',-1)
c
c......If tool is perpto C-axis then
c......Rotate axis at this location
c
              call tvprtb (ROTSTL(1,1),inc)
              if (inc .eq. ip1) then
                  call mwrotc (IMWRTA)
c
c......Otherwise output an error
c......and continue linearization
c
              else
                  call psterr (3,'RETRDISA','DEMPART',-1)
                  ierset = 1
              endif
          endif
      endif
c
c...If tool is currently perpto C-axis and
c...The next move requires the C-axis to rotate
c...Then position C-axis prior to linearizing move
c
      if (IMWRTA .eq. 0 .and. IMWLMT .eq. 0 .and. NROT .gt. 1) then
          call tvprtb (ROTSTL(1,1),inc)
          if (inc .eq. ip1) then
              call tvprtb (ROTANL(1,1),inc)
              if (inc .ne. ip1) then
                  dlt    = dabs(ROTANL(ip1,2)-ROTSTL(ip1,2))
                  if (dlt .gt. .5) then
                      call mwrotc (IMWRTA)
                  endif
              endif
          endif
      endif
c
c...C-axis is currently being rotated
c
      if (IMWRTA .ne. 0) then
          call mwrotc (IMWRTA)
          if (IMWRTA .ne. 0) kflg = 1
      endif
c
c...Linearize point
c
      if (IMWLMT .le. 1 .and. IMWRTA .eq. 0 .and. klin .ne. 2) then
          call mwlnrz (kflg,kerr,itva,rot1)
c
c......Rotary axis is taking longest route
c......due to retract logic being off
c......Reset error flag so it does not
c......try to handle the problem on the next entry
c
          if (ierset .eq. 1) kerr = 0
c
c......Output error message if rotaries
c......do not move in user requested direction
c......(Sometimes happens even when
c......user defined direction does not cause longest route)
c
          if (iflg .eq. 0 .and. kflg .eq. 1 .and. inosat .eq. 0 .and.
     1            IMWNXF .eq. 1) then
              call mwdrck (IMWNXT,ierr)
              if (ierr .eq. 1) call psterr (2,'ROTNOSAT','NOTDESIR',-1)
          endif
c
c......Move takes longest route due to limit error
c......during linearization (from 'mwlnrz')
c......Move to limit first
c
          if (kerr .eq. 3) then
              call mwlimt (IMWLMT)
              kerr   = 0
              if (IMWLMT .eq. 1) then
                  call copyn (MCHNUM,MCHNUL,12)
                  call copyn (TLVEC,TLVECL,3)
                  call cpyrot (ROTANG,ROTANL)
              endif
c
c...Retract logic is active
c
              if (IMWLMT .gt. 1) then
                  if (LRTRCT .ne. 0) then
                      call mwretr (1,IMWLMT,kflg,itva)
                      if (IMWLMT .eq. 4) call psterr (2,'LONGRTE',
     1                    'NOTDESIR',-1)
c
c...Retract is disabled
c
                  else
                      IMWLMT = 0
                      call psterr (2,'LONGRTE','NOTDESIR',-1)
c
c......If tool is perpto C-axis then
c......Rotate axis at this location
c
                      call tvprtb (ROTSTL(1,1),inc)
                      if (inc .eq. ir1) then
                          call mwrotc (IMWRTA)
c
c......Otherwise output an error
c......and continue linearization
c
                      else
                          call psterr (3,'RETRDISA','DEMPART',-1)
                          ierset = 1
                      endif
                  endif
              endif
          endif
      endif
c
c......Move has reached apex point
c......Restore final position
c......for next time through
c
      if (IMWXFL .eq. 1 .and. kflg .eq. 0) then
          call copyn (MCHNUX,MCHNUL,12)
          call copyn (TLVECX,TLVECL,3)
          call cpyrot (ROTANX,ROTANL)
          IMWXFL = 2
          kflg   = 1
c
c......Move has reached machine limit
c......Restore final position for next time through
c......and set the retract procedure active
c
      else if (IMWLMT .eq. 1 .and. kflg .eq. 0) then
          call copyn (MCHNUQ,MCHNUL,12)
          call copyn (TLVECQ,TLVECL,3)
          call cpyrot (ROTANQ,ROTANL)
          IMWLMT = 2
          kflg   = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwlnrz (kflg,kerr,ktva,grot)
c
c     FUNCTION:  This routine generates the actual linearized points.
c
c     INPUT:   kflg     I*4  D1  -  Set to 0 on first call, controlled by
c                                   this routine on subsequent calls.  See
c                                   OUTPUT description.  When set to -1
c                                   on initial call, linearizes first move
c                                   to calculate rotary axis positions on
c                                   first step only.
c
c              kerr     I*4  D1  -  tlaxis call error status: 3 - not the
c                                   shortest rotary motion, retract tool
c                                   when limits are reached. n - normal
c                                   linearization.
c
c              ktva     I*4  D1  -  Set to 1 when the final point was
c                                   modified to limit the rotary motion.
c                                   The final axes output will be
c                                   recalculated in this routine.
c
c     OUTPUT:  kflg     I*4  D1  -  0 - last point has been generated.
c                                   1 - In the midst of linearization.
c
c              grot     R*8  D20.2 - Rotary axes positions when 'kflg' = 1.
c
c***********************************************************************
c
      subroutine mwlnrz (kflg,kerr,ktva,grot)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (ROTBSV,POSMAP(2275))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),ROTBAS(4),LINAXS(6),
     1       AXSOUT(10),ROTBSV(4)
c
      integer*4 kerr,kflg,ktva
c
      real*8 grot(20,2)
c
      real*8 ratio,mnum(3,4),tvec(3),laxs(6),rbas(4),axs(10)
c
c...Calculate the Tool axis Vector Motion Plane (TVMP)
c
      call mwtvmp (STONUL(1,2),VECSAL,MCHNUM(1,2),TLVEC,TVMP)
c
c...Get maximum angle increment for linearized move
c
      call mwgrat (ratio)
c
c...Move does not require linearization
c
      if (ratio .ge. 1.0) go to 7000
c
c...Calculate initial rotary directions
c
      if (kflg .eq. -1) then
          call mwgenp (1,MCHNUL(1,2),TLVECL,TVMP,mnum(1,2),tvec,ratio)
          call setbas (ROTSTL(1,2),rbas)
          call tlaxis (mnum,tvec,laxs,axs,grot,rbas,0,0,kerr)
c
c...Generate linearized point
c
      else
          call mwgenp (1,MCHNUL(1,2),TLVECL,TVMP,MCHNUM(1,2),TLVEC,
     1                 ratio)
          call setbas (ROTSTL(1,2),ROTBAS)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,IMWEFL,kerr)
c
c......Mark linearization as active
c
          kflg   = 1
      endif
      go to 8000
c
c...End of linearization
c...Calculate final point
c
 7000 if (kflg .eq. -1) then
          call mwgenp (2,MCHNUL(1,2),TLVECL,TVMP,mnum,tvec,ratio)
          call setbas (ROTSTL(1,2),rbas)
          call tlaxis (mnum,tvec,laxs,axs,grot,rbas,0,0,kerr)
c
c...Output final point
c
      else
          if (kflg .ne. 0 .or. ktva .eq. 1) then
              call mwgenp (2,MCHNUL(1,2),TLVECL,TVMP,MCHNUM(1,2),TLVEC,
     1                     ratio)
              call setbas (ROTSTL(1,2),ROTBAS)
              call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                     1,IMWEFL,kerr)
              kflg   = 0
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
c     SUBROUTINE: mwadj (gvec,gsav,kfl)
c
c     FUNCTION:  This routine determines if the C-axis will fluctuate
c                greatly while the A-axis is around the apex point.  If
c                this is true, then the final tool axis will adjusted to
c                be closer to the input tool axis, keep it within tolerance
c                of the final point.  Depending on the tool axis vectors,
c                this can keep the C-axis from swinging by as much as 30
c                degrees.
c
c     INPUT:   gvec     R*8  D3  -  Tool axis of goto point.
c
c              gsav     R*8  D3  -  Tool axis of from point.
c
c     OUTPUT:  gvec     R*8  D3  -  Modified tool axis vector.
c
c              kfl      I*4  D1  -  1 = Tool axis has been adjusted to
c                                   minimize C-axis rotation.
c
c              kerr     I*4  D1  -  Adjusts this value if the tool axis
c                                   is modified to keep it in tolerance.
c
c***********************************************************************
c
      subroutine mwadj (gvec,gsav,kfl,kerr)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (LNRADJ,KPOSMP(1277))
      equivalence (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),LNRADJ,IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (ROTBAS,POSMAP(1435))
      equivalence (ROTBSV,POSMAP(2275)), (LNRATL,POSMAP(2286))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),LINAXS(6),AXSOUT(10),ROTBSV(4),
     1       LNRATL(5),ROTBAS(4)
c
      integer*4 kfl,kerr
c
      real*8 gvec(3),gsav(3)
c
      integer*4 i,ip1
c
      real*8 ptx(3),pty(3),vc(3),dlt,tlen,dang,ndist,bsv(4)
c
c...Determine if tool axis is
c...within tolerance of apex point
c
      ip1    = IRTINC(IRTACT(1))
      call tvr1ck (gvec,kfl)
      if (kfl .eq. 1 .or. LNRADJ .ne. 1) go to 8000
c
c...Tool has not been set to apex point
c...See if it is in a trouble zone
c
      call mwgtln (gvec,tlen,dang)
      dlt = dabs(ROTANG(ip1,2)-ROTSTL(ip1,2))
      if (dlt .ge. LNRATL(3) .and. dang .le. LNRATL(2)) then
          do 100 i=1,3,1
              ptx(i) = gsav(i) * tlen
              pty(i) = gvec(i) * tlen
              vc(i) = ptx(i) - pty(i)
  100     continue
          dlt = ndist(ptx,pty)
          if (dlt .le. LNRATL(1)) then
              call copyn (gsav,gvec,3)
          else
              call unitvc (vc,vc)
              call vcplvc (pty,vc,gvec,LNRATL(1))
              call unitvc (gvec,gvec)
          endif
          kfl    = 1
      endif
c
c...Tool axis was changed
c...Calculate new rotary axes
c
      if (kfl .eq. 1) then
          call mwudir (1)
cc          call copynk (knxt,IRTNXT,4)
          call copyn (ROTBSV,bsv,4)
          call copyn (ROTBSV,ROTBAS,4)
          call tlaxis (MCHNUM,gvec,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,kerr)
          call copyn (bsv,ROTBSV,4)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwdrck (kdir,kerr)
c
c     FUNCTION:  This routine determines if the rotary axes move in the
c                user requested direction.  If not, an error is returned.
c
c     INPUT:   kdir     I*4  D4  -  User requested rotary directions.
c
c     OUTPUT:  kerr     I*4  D1  -  1 = Rotary axis directions are not
c                                   satisfied.
c
c***********************************************************************
c
      subroutine mwdrck (kdir,kerr)
c
      include 'post.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (NROT  ,KPOSMP(1366))
c
      integer*4 IRTACT(2),NROT
c
      equivalence (ROTBAS,POSMAP(1435)), (ROTANG,POSMAP(5173))
c
      real*8 ROTANG(20,2),ROTBAS(4)
c
      integer*4 kdir(4),kerr
c
      integer*4 idir(4),i
c
      real*8 dlt(4)
c
c...Determine direction of rotary axes
c
      call copynk (kdir,idir,4)
      call rotlin (ROTANG,ROTBAS,0,idir,dlt)
c
c...Determine if any axis do not
c...move in the user requested direction
c
      kerr   = 0
      do 100 i=1,NROT,1
          if (kdir(IRTACT(i)) .ne. 0 .and. kdir(IRTACT(i)) .ne. 3 .and.
     1        kdir(IRTACT(i)) .ne. idir(IRTACT(i)) .and.
     2        dlt(i) .ne. 0.) kerr = 1
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwgenp (knum,glstp,glstv,gtvmp,gpos,gvec,gratio)
c
c     FUNCTION:  This routine generates a linearized cl point and
c                tool axis based on a percentage along the TVMP.
c
c     INPUT:   knum     I*4  D1  -  1 = Generate linearized point along
c                                       TVMP.
c                                   2 = Generate final point.
c
c              glstp    R*8  D3  -  Destination cl point coordinates.
c                                   This point is returned when
c                                   'knum' = 2.
c
c              glstv    R*8  D3  -  Destination tool vector.  This vector
c                                   is returned when 'knum' = 2.
c
c              gtvmp    R*8  D12 -  Tool axis vector plane (TVMP).
c
c              gratio   R*8  D3  -  Factor to apply with spans.
c
c     OUTPUT:  gpos     R*8  D3  -  Calculated tool end postion.
c
c              gvec     R*8  D3  -  Calculated tool axis vector.
c
c***********************************************************************
c
      subroutine mwgenp (knum,glstp,glstv,gtvmp,gpos,gvec,gratio)
c
      integer*4 knum
c
      real*8 glstp(3),glstv(3),gratio,gtvmp(12),gpos(3),gvec(3)
c
      real*8 mnum(3)
c
c...Return final point
c
      if (knum .eq. 2) then
          call copyn (glstp,gpos,3)
          call copyn (glstv,gvec,3)
c
c...Calculate linearized point
c...as a ratio of the entire move
c
      else
          call vcplvc (gtvmp(1),gtvmp(4),gpos,gratio)
          call vcplvc (gtvmp(7),gtvmp(10),mnum,gratio)
          call vcmnvc (mnum,gpos,gvec)
          call unitvc (gvec,gvec)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwgrat (gratio)
c
c     FUNCTION:  This routine calculates the percentage of the move
c                along the TVMP that can be made to keep the tool within
c                tolerance.
c
c                It uses an iteration method to make sure that the
c                rotary axes move exactly the distance they should to
c                keep the tool in tolerance.
c
c     INPUT:   none
c
c     OUTPUT:  gratio   R*8  D1  -  Percentage of the move to make.
c
c***********************************************************************
c
      subroutine mwgrat (gratio)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTNXT,KPOSMP(1361))
      equivalence (IRTNXF,KPOSMP(1365)), (NROT  ,KPOSMP(1366))
      equivalence (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),NROT,IRTNXF,IRTNXT(4),IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (PPMAXD,POSMAP(1584))
      equivalence (ROTBSV,POSMAP(2275)), (FUZZ8 ,POSMAP(4913))
      equivalence (ROTANG,POSMAP(5173))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),ROTBSV(4),PPMAXD(10),
     1       FUZZ8
c
      real*8 gratio
c
      integer*4 i,ierr,ir1,ir2,isw,iadj,idid,irfx,irnx(4),ip1,ip2
c
      real*8 rdlt(4),dlt(4),rnum,an2max(4),rang(20,2),an1max(4),tvec(3),
     1       rlow,rhi,rmn,d1,d2,rg1(2),rg2(2),mnum(3,4),laxs(6),axs(10),
     2       an3max(4)
c
      equivalence (ir1   ,IRTACT(1)), (ir2  ,IRTACT(2))
c
c...If both points are at apex location
c...then don't linearize this move
c
cc      firx(1) = 0.
cc      firx(2) = 0.
cc      firx(3) = 0.
cc      firx(IRTWRK(ir1)) = 1.
cc      call vecdad (firx,firx,1)
      gratio = 1.
      call betvec (VECSAL,TLVEC,d1)
      if (d1 .le. FUZZ8) go to 8000
c
c...Save user defined directions
c
      irfx   = IRTNXF
      call copynk (IRTNXT,irnx,4)
c
c...Get maximum angle increment for linearized move
c
      call cpyrot (ROTANG,rang)
      call getnpt (STONUL(1,2),ROTSTL,an1max)
      call getnpt (MCHNUM(1,2),ROTANG,an2max)
c
c...Calculate move percentage based on
c...delta rotary angles
c
      idid   = 0
   50 gratio = 1.
      iadj   = 0
      do 100 i=1,NROT,1
          isw    = 0
          ip1    = IRTINC(IRTACT(i))
          an3max(IRTACT(i)) = an1max(IRTACT(i))
          if (an2max(IRTACT(i)) .lt. an1max(IRTACT(i))) then
              an1max(IRTACT(i)) = an2max(IRTACT(i))
              isw    = 1
          endif
          rnum   = dabs(PPMAXD(IRTACT(i)+6)*.99)
          if (an1max(IRTACT(i)) .gt. rnum .and. rnum .ne. 0.)
     1        an1max(IRTACT(i)) = rnum
          rdlt(i) = rang(ip1,2) - ROTSTL(ip1,2)
          dlt(i) = dabs(rdlt(i))
          if (dlt(i) .ne. 0.) then
              rnum   = an1max(IRTACT(i)) / dlt(i)
              if (rnum .lt. gratio) then
                  gratio = rnum
                  iadj   = isw
              endif
          endif
  100 continue
c
c...Move does not require linearization
c
      if (gratio .ge. 1.0) go to 8000
c
c...The maximum step at the end of the move was taken
c...This could cause more steps than necessary at the
c...beginning of the move, so
c...Recalculate the maximum step based on
c...the FROM point and the first estimated linearized point
c
      if (iadj .eq. 1 .and. idid .eq. 0) then
          call mwgenp (1,MCHNUM(1,2),TLVEC,TVMP,mnum(1,2),tvec,gratio)
          call copyn (an3max,an1max,4)
          call getnpt (mnum(1,2),rang,an2max)
          idid   = 1
          go to 50
      endif
c
c...Iterate to correct tool axis
c...By adjusting ratio along TVMP until
c...maximum rotary deltas are met
c
      rmn    = .001
      rlow   = 0.
      rhi    = 1.
      rg1(1) = an1max(ir1) * (1.-rmn)
      rg1(2) = an1max(ir1) * (1.+rmn)
      rg2(1) = an1max(ir2) * (1.-rmn)
      rg2(2) = an1max(ir2) * (1.+rmn)
c
c......Generate point along TVMP
c
  500 call mwgenp (1,MCHNUM(1,2),TLVEC,TVMP,mnum(1,2),tvec,gratio)
      call tlaxis (mnum,tvec,laxs,axs,rang,ROTBSV,0,0,ierr)
c
c......Restore user defined directions
c
      IRTNXF = irfx
      call copynk (irnx,IRTNXT,4)
c
c......Determine if move is withing tolerance
c......of maximum delta movements allowed
c
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
      d1     = dabs(rang(ip1,2)-ROTSTL(ip1,2))
      d2     = dabs(rang(ip2,2)-ROTSTL(ip2,2))
      if ((d1 .ge. rg1(1) .and. d1 .le. rg1(2) .and. d2 .le. rg2(2))
     1        .or.
     2    (d2 .ge. rg2(1) .and. d2 .le. rg2(2) .and. d1 .le. rg1(2)))
     3        go to 8000
      if (dabs(rhi-rlow) .le. rmn/10.) go to 8000
c
c......Iteration failed
c......calculate new iteration
c
      if (d1 .gt. rg1(2) .or. d2 .gt. rg2(2)) then
          rhi    = gratio
      else
          rlow   = gratio
      endif
      gratio = rlow + (rhi-rlow)/2.
      go to 500
c
c...Calculate the tool axis on the TVMP
c...based on maximum C-axis rotary delta
c...This code provides a good approximation
c...when the C-axis is the controlling axis
c...for linearization, but does not take into
c...consideration the A-axis of an AC-style head.
c...It is left here for reference in case a direct
c...solution is found in the future.
c
cc      do 200 i=1,IRTNUM,1
cc          rang(i,2) = ROTSTL(i,2) + rdlt(i)*gratio
cc  200 continue
cc      call linrot (rang,ROTBSV,0)
cc      call getijk (rang,vec)
c
c......Calculate the A-axis direction vector
c......after the rotaries have been moved
c
cc      firx(1) = 0.
cc      firx(2) = 0.
cc      firx(3) = 0.
cc      firx(IRTWRK(IRTACT(1))) = 1.
cc      tvec(1) = 0.
cc      tvec(2) = 0.
cc      tvec(3) = 0.
cc      tvec(IRTWRK(IRTACT(2))) = 1.
cc      call crosvc (firx,tvec,vecx)
cc      call vecdad (firx,firx,1)
cc      call vecdad (vecx,vecx,1)
cc      call vecrot (vecx,tvec,firx,rang(IRTACT(1),1))
c
c......Project the A-axis vector onto the TVMP
c
cc      call copyn (TVMP(13),pln,3)
cc      pln(4) = 0.
cc      call plnint (vec,tvec,pln,vecx,ierr)
cc      call unitvc (vecx,vec)
c
c......Calculate ratio
c
cc      call betvec (TLVEC,VECSAL,ASPAN)
cc      call betvec (vec,VECSAL,rnum)
cc      gratio = rnum / ASPAN
cc      go to 8000
c
c...End of routine
c
 8000 if (gratio .gt. .999) gratio = 1.
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwgtln (gvec,gtl,gang)
c
c     FUNCTION:  This routine determines the tool length to use for
c                the linearization routines.  It also calculates the angle
c                of the input vector as compared to the C-axis rotation
c                vector.
c
c     INPUT:   gvec     R*8  D3  -  Tool axis vector.
c
c     OUTPUT:  gtl      R*8  D1  -  Tool length to use in linearization
c                                   calculations.
c
c              gang     R*8  D1  -  Angle between tool axis vector and
c                                   C-axis rotation vector.
c
c***********************************************************************
c
      subroutine mwgtln (gvec,gtl,gang)
c
      include 'post.inc'
c
      equivalence (MCHOPT,KPOSMP(0308)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTWRK,KPOSMP(1506))
      equivalence (ITP   ,KPOSMP(1801))
c
      integer*4 MCHOPT(20),IRTACT(2),IRTWRK(20),ITP,IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002)), (TL    ,POSMAP(3601))
      equivalence (LNRATL,POSMAP(2286))
c
      real*8 RAD,TL(120),LNRATL(5)
c
      real*8 gvec(3),gtl,gang
c
      integer*4 ip1
c
      real*8 firx(3)
c
c...Calculate tool length to use
c...in tool axis adjustment calculations
c
      gtl   = TL(ITP)
      if (gtl .le. 0.) gtl = LNRATL(4)
      if (gtl .le. 0.) then
          gtl = 3.
          if (MCHOPT(2) .eq. 2) gtl = gtl * 25.4
      endif
c
c...Calculate angular delta from spindle vector
c...To determine if we should be adjusting this
c...tool axis
c
      ip1    = IRTINC(IRTACT(1))
      firx(1) = 0.
      firx(2) = 0.
      firx(3) = 0.
      firx(IRTWRK(ip1)) = 1.
      call vecdad (firx,firx,1)
      call betvec (firx,gvec,gang)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwtvmp (gsnum,gvsv,gmnum,gvec,gtvmp)
c
c     FUNCTION:  This routine calculates the Tool Axis Vector Motion
c                Plane (TVMP) for the current move.  It is stored as
c                follows.
c
c                   (1:3)   = From tool end point.
c                   (4:6)   = Length vector from initial tool end point
c                             to final tool end point.
c                   (7:8)   = From tool top point.
c                   (9:12)  = Length vector from initial tool top point
c                             to final tool top point.
c                   (13:15) = TVMP plane normal.
c
c     INPUT:   gsnum    R*8  D3  -  From tool end point.
c
c              gvsv     R*8  D3  -  From tool axis vector.
c
c              gmnum    R*8  D3  -  To tool end point.
c
c              gvec     R*8  D3  -  To tool axis vector.
c
c     OUTPUT:  gtvmp    R*8  D15 -  Tool Axis Vector Motion Plane.
c
c***********************************************************************
c
      subroutine mwtvmp (gsnum,gvsv,gmnum,gvec,gtvmp)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
c
      integer*4 IRTNUM
c
      equivalence (ROTBSV,POSMAP(2275)), (FUZZ8 ,POSMAP(4913))
c
      real*8 ROTBSV(4),FUZZ8
c
      real*8 gsnum(3),gvsv(3),gmnum(3),gvec(3),gtvmp(15)
c
      integer*4 i
c
      real*8 snum(3),mnum(3),tlen,ang,tvec(3),nmag,rang(20,2),dlt
c
c...Get tool length to use
c
      call mwgtln (gvec,tlen,ang)
c
c...Calculate tool plane normal vector
c
      call crosvc (gvsv,gvec,snum)
      call copyn (gvsv,tvec,3)
c
c......Vectors are parallel
c......Create temporary offset vector
c......along direction of rotaries
c......to use in plane calculation
c
      if (nmag(snum) .lt. FUZZ8) then
          do 100 i=1,IRTNUM,1
              rang(i,2) = ROTSTL(i,2)
              dlt    = ROTANL(i,2) - ROTSTL(i,2)
              if (dabs(dlt) .gt. FUZZ8) then
                  dlt    = dabs(dlt) / dlt
                  rang(i,2) = ROTSTL(i,2) + .001*dlt
              endif
  100     continue
          call linrot (rang,ROTBSV,0)
          call getijk (rang,tvec)
          call crosvc (tvec,gvec,snum)
      endif
      call unitvc (snum,gtvmp(13))
c
c...Calculate points at top of tool
c
      call vcplvc (gsnum,tvec,snum,tlen)
      call vcplvc (gmnum,gvec,mnum,tlen)
c
c...Calculate line segments at
c...bottom and top of tool to
c...linearize along
c
      call copyn (gsnum,gtvmp(1),3)
      call vcmnvc (gmnum,gsnum,gtvmp(4))
      call copyn (snum,gtvmp(7),3)
      call vcmnvc (mnum,snum,gtvmp(10))
c
c...End of routine
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwudir (kfl)
c
c     FUNCTION:  This routine saves and restores the user requested
c                rotary direction(s).
c
c     INPUT:   kfl      I*4  D1  -  0 = Save user defined rotary directions.
c                                   1 = Restore directions.
c
c     OUTPUT:  none
c
c***********************************************************************
c
      subroutine mwudir (kfl)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRFNLS,KPOSMP(1640)), (IRTNLS,KPOSMP(1641))
c
      integer*4 IRTNXT(4),IRTNXF,IRFNLS,IRTNLS(4),IRTINC(4)
c
      integer*4 kfl
c
      integer*4 i
c
c...Save user defined rotary directions
c
      if (kfl .eq. 0) then
          IMWNXF = IRFNLS
          call copynk (IRTNLS,IMWNXT,4)
c
c...Restore user defined rotary directions
c
      else if (kfl .eq. 1 .or. (kfl .eq. 2 .and. IMWNXF .ne. 0)) then
          IRTNXF = IMWNXF
          call copynk (IMWNXT,IRTNXT,4)
c
c...Calculate directions based on current move
c...Used when rotating C-axis only so that
c...The tlaxis calculated directions are used
c
      else
          IRTNXF = 1
          do 100 i=1,4,1
              if (ROTANL(IRTINC(i),2) .eq. ROTSTL(IRTINC(i),2)) then
                  IRTNXT(i) = 0
              else if (ROTANL(IRTINC(i),2) .gt. ROTSTL(IRTINC(i),2))
     1              then
                  IRTNXT(i) = 1
              else
                  IRTNXT(i) = 2
              endif
  100     continue
      endif
c
c...End of routine
c
 8000 return
      end
