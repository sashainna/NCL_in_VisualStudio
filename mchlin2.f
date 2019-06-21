c
c***********************************************************************
c
c     FILE NAME: mchlin2
c
c     CONTAINS:  mwapex  mwlimt  mwretr  mwrotc  mwshfv  mwrdis
c
c     COPYRIGHT 2006 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        mchlin2.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/06/15 , 08:35:49
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: mwapex (kfl,kerr)
c
c     FUNCTION:  This routine determines if the tool axis crosses the
c                apex plane of the C-axis and if it does generates an
c                intermediate point on this plane to linearize to prior
c                to linearizing to the final point.
c
c                The reason for generating this intermediate point is
c                that the C-axis can swing violently while crossing this
c                plane.  This movement could be missed if only analyzing
c                the from and to locations of the move and cause
c                unpredictable results during linearization.
c
c     INPUT:   none.
c
c     OUTPUT:  kfl      I*4  D1  -  An intermediate point was generated
c                                   on the C-axis apex plane.  Points
c                                   should be linearized to this location
c                                   first, prior to restoring the final
c                                   location.
c
c              kerr     I*4  D1  -  Error status from 'tlaxis' when
c                                   intermediate point is generated.
c
c***********************************************************************
c
      subroutine mwapex (kfl,kerr)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (IRTWRK,KPOSMP(1506))
c
      integer*4 IRTACT(2),IRTWRK(20),IRTNUM,IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435))
      equivalence (ROTBSV,POSMAP(2275)), (FUZZ8 ,POSMAP(4913))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),LINAXS(6),AXSOUT(10),
     1       ROTBSV(4),FUZZ8,ROTBAS(4),ROTSTO(20,2)
c
      integer*4 kfl,kerr
c
      integer*4 itva,i,ip1
c
      real*8 firx(3),rnum,ndot,tvec(3),vecx(3),ang1,ang2,ang3,nmag,
     1       plvec(3),dlt,rang(20,2),FUZZ
c
      parameter (fuzz=1.e-6)
c
c...Initialize routine
c
      kfl    = 0
      ip1    = IRTINC(IRTACT(1))
c
c...Adjust table rider/head carrier for dead axes
c
      firx(1) = 0.
      firx(2) = 0.
      firx(3) = 0.
      firx(IRTWRK(ip1)) = 1.
      call vecdad (firx,firx,1)
c
c...Calculate the tool plane
c
      call crosvc (VECSAL,TLVEC,plvec)
c
c......Vectors are parallel
c......Create temporary offset vector
c......along direction of rotaries
c......to use in plane calculation
c
      if (nmag(plvec) .lt. FUZZ) then
          do 100 i=1,IRTNUM,1
              rang(i,2) = ROTSTO(i,2)
              dlt    = ROTANG(i,2) - ROTSTO(i,2)
              if (dabs(dlt) .gt. FUZZ) then
c
c.........Use halfway point between two angles
c.........when vectors are parallel, otherwise
c.........the vectors are too close to create
c.........a valid plane (axis wobble can occur)
c
cc                  dlt    = dabs(dlt) / dlt
                  rang(i,2) = ROTSTO(i,2) + dlt/2.
              endif
  100     continue
          call linrot (rang,ROTBSV,0)
          call getijk (rang,tvec)
          call crosvc (tvec,TLVEC,plvec)
          if (nmag(plvec) .lt. FUZZ) go to 8000
      endif
      call unitvc (plvec,plvec)
c
c...Calculate intersection vector of
c...TVMP plane and Rotary axis #1 vector
c
      rnum   = ndot(firx,plvec)
      call vctmsc (plvec,rnum,tvec)
      call vcmnvc (firx,tvec,vecx)
      if (nmag(vecx) .lt. FUZZ) go to 8000
      call unitvc (vecx,vecx)
      call tvr1ck (vecx,itva)
c
c...Determine if move crosses
c...Rotary Axis #1 apex point
c
      call betvec (VECSAL,TLVEC,ang1)
      call betvec (VECSAL,vecx,ang2)
      call betvec (TLVEC,vecx,ang3)
c
c...Move crosses apex point
c...Go to apex point first
c...prior to continuing to final point
c
      if (ang2 .lt. ang1 .and. ang3 .lt. ang1 .and.
     1    ang2 .gt. FUZZ .and. ang3 .gt. FUZZ) then
c
c......Save programmed position
c
          call copyn (MCHNUM,MCHNUX,12)
          call copyn (TLVEC,TLVECX,3)
          call cpyrot (ROTANG,ROTANX)
c
c......Calculate new position
c......at apex point
c
          rnum   = ang2 / ang1
          MCHNUM(1,2) = STONUL(1,2) + (MCHNUM(1,2)-STONUL(1,2)) * rnum
          MCHNUM(2,2) = STONUL(2,2) + (MCHNUM(2,2)-STONUL(2,2)) * rnum
          MCHNUM(3,2) = STONUL(3,2) + (MCHNUM(3,2)-STONUL(3,2)) * rnum
          call copyn (vecx,TLVEC,3)
          call copyn (ROTBSV,ROTBAS,4)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,kerr)
          kfl    = 1
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwlimt (kfl)
c
c     FUNCTION:  This routine determines if the tool axis crosses the
c                limits of any axes. If so, then an intermediate point
c                on the violated limit axis will be generated to be
c                linearized to.
c
c                After reaching the limit any tool retraction and
c                repositioning will also be handled in this routine.
c
c     INPUT:   kfl      I*4  D1  -  0 = Normal call.  2 = Force tool to
c                                   be retracted at this location.
c                                   Normally due to user defined direction
c                                   causing longest route.
c
c     OUTPUT:  kfl      I*4  D1  -  1 = Intermediate point was generated
c                                   on the axis limit.  Points should be
c                                   linearized to this location first,
c                                   prior to restoring the final location.
c
c                                   2 = From tool axis is already at limit
c                                   Retract tool at this location.
c
c***********************************************************************
c
      subroutine mwlimt (kfl)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTNXF,KPOSMP(1365))
c
      integer*4 IRTNXF
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (ROTBSV,POSMAP(2275))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2),ROTBSV(4),ROTANG(20,2),ROTBAS(4),TLVEC(3),
     1       MCHNUM(3,4),LINAXS(6),AXSOUT(10)
c
      integer*4 kfl
c
      integer*4 iret,i,iout,ion,iaxs(10),ilmt(10),ierr,inxsav,ilow,
     1          idid
c
      real*8 laxs(6),axs(10),rang(20,2),mnum(3,4),tvec(3),rlow,rhi,rat,
     1       rmn,tpl(15),mxnum(3,4),txvec(3)
c
c...Save rotary positions on other side of limit
c...for repositioning move
c
      inxsav = IRTNXF
      idid   = 0
      call cpyrot (ROTANG,ROTANQ)
      call copyn (ROTBAS,ROTBSQ,4)
      call copyn (MCHNUM,MCHNUQ,12)
      call copyn (TLVEC,TLVECQ,3)
      if (kfl .eq. 2) go to 8000
c
c...Calculate final point,
c...Forcing shortest route outside of limits
c
      call cpyrot (ROTSTL,ROTSTO)
      IRTNXF = 2
      call tlaxis (MCHNUL,TLVECL,laxs,axs,rang,ROTBSV,0,0,ierr)
c
c...Determine which axes are outside of limits
c
      call lmtchk (axs,iout,iaxs,0)
c
c...Determine which axes of current location
c...are exactly on limits
c
      call tlaxis (STONUL,VECSAL,laxs,axs,rang,ROTBSV,0,0,ierr)
      call lmtchk (axs,ion,ilmt,2)
c
c...Determine if any of the offending (out of limits) axes
c...are currently on the limit
c
      iret   = 0
      if (ion .ne. 0) then
          do 100 i=1,10,1
              if ((iaxs(i) .eq. 1 .and. ilmt(i) .eq. 3) .or.
     1            (iaxs(i) .eq. 2 .and. ilmt(i) .eq. 4)) iret = 1
  100     continue
      endif
c
c...Offending axis is on limit
c...Tool needs to be retracted
c
      if (iret .eq. 1) then
          kfl    = 2
c
c...None of the offending axes are on the limit
c...Determine closest offending axis and move to it
c
      else
          rmn    = .001
          rlow   = 0.
          rhi    = 1.
          rat    = .5
c
c......Calculate TVMP
c
          call mwtvmp (STONUL(1,2),VECSAL,MCHNUM(1,2),TLVEC,tpl)
c
c......Generate point along TVMP
c
  500     call mwgenp (1,MCHNUM(1,2),TLVEC,tpl,mnum(1,2),tvec,rat)
          call cpyrot (ROTSTL,ROTSTO)
          IRTNXF = 2
          call tlaxis (mnum,tvec,laxs,axs,rang,ROTBSV,0,0,ierr)
c
c......Determine if any axis has hit limit yet
c
          call lmtchk (axs,ion,ilmt,2)
          iret   = 0
          if (ion .ne. 0) then
              do 600 i=1,10,1
                  if ((iaxs(i) .eq. 1 .and. ilmt(i) .eq. 3) .or.
     1                (iaxs(i) .eq. 2 .and. ilmt(i) .eq. 4)) iret = 1
  600         continue
          endif
c
c......Limit has been reached
c......output this point
c
          if (iret .eq. 1 .or. dabs(rhi-rlow) .le. rmn/100.) then
              kfl    = 1
              if (iret .eq. 1) then
                  call copyn (mnum,MCHNUM,12)
                  call copyn (tvec,TLVEC,3)
              else if (idid .eq. 1) then
                  call copyn (mxnum,MCHNUM,12)
                  call copyn (txvec,TLVEC,3)
              else
                  kfl    = 2
              endif
              call copyn (ROTBSV,ROTBAS,4)
              call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                     1,0,ierr)
c
c......Iteration failed
c......Calculate new iteration
c
          else
              call lmtchk (axs,ion,ilmt,0)
              ilow   = 0
              do 700 i=1,10,1
                  if ((iaxs(i) .eq. 1 .or. iaxs(i) .eq. 2) .and.
     1                ilmt(i) .eq. 0) ilow = 1
  700         continue
              if (ilow .eq. 0) then
                  rhi    = rat
              else
                  call copyn (mnum,mxnum,12)
                  call copyn (tvec,txvec,3)
                  idid   = 1
                  rlow   = rat
              endif
              rat    = rlow + (rhi-rlow)/2.
              go to 500
          endif
      endif
c
c...End of routine
c
 8000 IRTNXF = inxsav
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwretr (kwhich,kfl,kflg,ktva)
c
c     FUNCTION:  This routine creates the axis locations when retracting
c                and repositioning the tool because the longest route
c                was taken.  It generates all locations of the retract/
c                position/plunge moves.
c
c     INPUT:   kwhich   I*4  D1  -  1 = Linearization is active,
c                                   2 = Look ahead is active.
c
c              kfl      I*4  D1  -  2 = Shift tool, 3 = Retract tool,
c                                   4 = Position axes, 5 = Plunge tool,
c                                   6 = Shift tool.
c
c                                   If the rotary axis cannot be pre-
c                                   positioned, then 'kfl' has the
c                                   following values.
c
c                                   7 = Retract tool.  8 = Position tool
c                                   and axes.  The tool is plunged as part
c                                   of the normal move.
c
c     OUTPUT:  kfl      I*4  D1  -  Incremented by 1 after each call.
c                                   Set to 0 when retract/plunge procedure
c                                   is finished.
c
c              kflg     I*4  D1  -  Set to 1 when a retract type point
c                                   needs to be output.
c
c              ktva     I*4  D1  -  Set to 1 if the retract procedure
c                                   fails and the final location needs
c                                   to be recalculated.
c
c***********************************************************************
c
      subroutine mwretr (kwhich,kfl,kflg,ktva)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (HLDBCK,KPOSMP(1223)), (INCR  ,KPOSMP(1226))
      equivalence (IRTACT,KPOSMP(1256)), (IRTSCL,KPOSMP(1288))
      equivalence (IRTSHF,KPOSMP(1374)), (IRTINC,KPOSMP(1461))
      equivalence (ITP   ,KPOSMP(1801))
      equivalence (IFITYP,KPOSMP(3150)), (IFSVLN,KPOSMP(3207))
c
      integer*4 ITP,HLDBCK,IRTACT(2),INCR,IRTSCL(4),MACHTP,
     1          IRTSHF,IRTINC(4),IFSVLN
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (ROTBAS,POSMAP(1435)), (ROTBSV,POSMAP(2275))
      equivalence (PRESTO,POSMAP(2280)), (FDSVLN,POSMAP(2377))
      equivalence (PFEED ,POSMAP(3540))
      equivalence (TL    ,POSMAP(3601)), (RTSHFD,POSMAP(4947))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),LINAXS(6),AXSOUT(10),
     1       ROTBSV(4),ROTBAS(4),TL(120),ROTSTO(20,2),VECSAV(3),
     2       RTSHFD,PRESTO(6),FDSVLN,PFEED(4)
c
      integer*4 kfl,kflg,ktva,kwhich
c
      integer*4 ierr,ir1,ir2,ivfl,ip1
c
      real*8 rdis,rsto(20,2),mnum(3,4),tvec(3),laxs(6),raxs(10),ofvc(3),
     1       rang(20,2)
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
c...Initialize routine
c
      ktva   = 0
      kflg   = 0
      ip1    = IRTINC(ir1)
c
c...First entry
c
      if (kfl .eq. 2) then
c
c...Determine if tool should be shifted
c
          IMWSHF = 0
          kfl    = 3
          IFSVLN = IFITYP
          FDSVLN = PFEED(1)
          if (IRTSHF .gt. 1 .and. RTSHFD .ne. 0. .and. kwhich .eq. 1
     1        .and. MACHTP .ne. 3) then
              call mwshfv (IRTSHF,PRESTO,PRESTO(4),STONUL(1,2),VECSAL,
     1                     MCHNUM(1,2),TLVEC,ofvc,ivfl)
              if (ivfl .eq. 1) then
                  IMWSHF = 1
                  kfl    = 2
              endif
          endif
c
c......Make sure that initial rotary axes position
c......can be reached
c......Only performed during linearizatio
c
          if (kwhich .eq. 1) then
              call copyn (STONUL(1,2),mnum(1,2),3)
              call cpyrot (ROTSTO,rsto)
              call cpyrot (ROTANQ,ROTSTO)
              call copyn (VECSAV,tvec,3)
              call copyn (TLVECQ,VECSAV,3)
              call tlaxis (mnum,tvec,laxs,raxs,rang,ROTBSQ,0,0,ierr)
              call cpyrot (rsto,ROTSTO)
              call copyn (tvec,VECSAV,3)
c
c.........Suggested rotation cannot be reached
c.........Store final position and output
c.........error message
c
              if (dabs(rang(ip1,2)-ROTSTO(ip1,2)) .lt. .001) then
                  kfl    = 7
c
cc                  call copyn (mnum,MCHNUL,12)
cc                  call tlaxis (MCHNUL,TLVECL,laxs,raxs,ROTANL,ROTBSV,
cc     1                         0,0,ierr)
                  call psterr (2,'ROTNOSAT','NOTDESIR',-1)
cc                  kfl    = 0
cc                  ktva   = 1
cc                  go to 8000
              endif
          endif
c
c......Disable held back motion
c
          IMWHLD = HLDBCK
          HLDBCK = 2
      endif
c
c...Shift tool away from part
c
      if (kfl .eq. 2) then
c
c......Save initial point
c
          call copyn (STONUL(1,2),SHFSTO,3)
c
c......Calculate point at shifted distance
c
          call vcplvc (STONUL(1,2),ofvc,MCHNUM(1,2),RTSHFD)
c
c......Calculate output axes
c
          call copyn (VECSAL,TLVEC,3)
          call copyn (ROTBSV,ROTBAS,4)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          IMWFED = 7
          kflg   = 1
c
c...Retract tool
c
      else if (kfl .eq. 3 .or. kfl .eq. 7) then
c
c......Get retract distance
c
          call mwrdis (STONUL(1,3),ROTSTL,rdis)
c
c......Save plunge point
c
          call copyn (STONUL(1,2),RETSTO,3)
c
c......Calculate point at retract distance
c
          call vcplvc (STONUL(1,2),VECSAL,MCHNUM(1,2),rdis)
c
c......Calculate output axes
c......If linearization is active
c
          if (kwhich .eq. 1) then
              call copyn (VECSAL,TLVEC,3)
              call copyn (ROTBSV,ROTBAS,4)
              call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                     1,0,ierr)
          endif
          IMWFED = 1
          kflg   = 1
c
c...Reposition rotary axes within limits
c
      else if (kfl .eq. 4) then
          IMWFED = 3
          call copyn (STONUL(1,2),MCHNUM(1,2),3)
          call cpyrot (ROTSTO,rsto)
          call cpyrot (ROTANQ,ROTSTO)
          call copyn (ROTBSQ,ROTBAS,4)
          call copyn (VECSAV,TLVEC,3)
          call copyn (TLVECQ,VECSAV,3)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          call cpyrot (rsto,ROTSTO)
          kflg   = 1
c
c...If Rotary axis #1 moves on a Rotary scale
c...and the delta movement is 360 degrees
c...Then force incremental mode, otherwise
c...axis will not be output
c
          if (IRTSCL(ir1) .eq. 2 .and. INCR .eq. 1 .and.
     1        dabs(ROTANG(ip1,2)-ROTSTO(ip1,2)) .gt. 359.99)
     2            IMWINC = 1
c
c...Plunge the tool
c
      else if (kfl .eq. 5) then
          IMWFED = 2
          call copyn (RETSTO,MCHNUM(1,2),3)
          call setbas (ROTSTL(1,2),ROTBAS)
          call copyn (VECSAV,TLVEC,3)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          kflg   = 1
          if (IMWSHF .eq. 0) then
              kfl    = kfl    + 1
              HLDBCK = IMWHLD
          endif
c
c...Shift tool back to original location
c
      else if (kfl .eq. 6) then
          IMWFED = 7
          call copyn (SHFSTO,MCHNUM(1,2),3)
          call setbas (ROTSTL(1,2),ROTBAS)
          call copyn (VECSAV,TLVEC,3)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          HLDBCK = IMWHLD
          kflg   = 1
c
c...Position tool above hole
c
      else if (kfl .eq. 8) then
c
c......Get retract distance
c
          call mwrdis (MCHNUQ(1,3),ROTANQ,rdis)
c
c......Calculate point at retract distance
c
          call vcplvc (MCHNUQ(1,2),TLVECQ,MCHNUM(1,2),rdis)
c
c......Position above hole
c
          IMWFED = 3
          call cpyrot (ROTANQ,ROTSTO)
          call cpyrot (ROTSTO,rsto)
          call copyn (ROTBSQ,ROTBAS,4)
          call copyn (TLVECQ,TLVEC,3)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          call cpyrot (rsto,ROTSTO)
          kflg   = 1
          kfl    = 6
      endif
      kfl    = kfl    + 1
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwrotc (kfl)
c
c     FUNCTION:  This routine creates the axis locations when rotating
c                the C-axis around the apex point when the rotary axes
c                are taking the longest route and the retract procedure
c                is disabled.
c
c                Either a single rotation will occur when the tool is
c                at the center of the C-axis or multiple rotations/
c                positions will be output otherwise.
c
c     INPUT:   kfl      I*4  D1  -  Set to 0 on intial call, handled by
c                                   this routine on subsequent calls.
c                                   Refer to OUTPUT description.
c
c     OUTPUT:  kfl      I*4  D1  -  1 = Creating multiple rotations of
c                                       the C-axis.  This routine should
c                                       be called until 'kfl' is set to 0.
c
c***********************************************************************
c
      subroutine mwrotc (kfl)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
c
      integer*4 IRTACT(2),IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (ROTBAS,POSMAP(1435))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),ROTANG(20,2),TLVEC(3),LINAXS(6),AXSOUT(10),
     1       ROTBAS(4),ROTSTO(20,2),VECSAV(3),HLDROT(20,2)
c
      integer*4 kfl
c
      integer*4 ierr,ir1,ir2,ntim,itva,ifl,ip1
c
      real*8 dlt,rtim,an1max(4)
c
      equivalence (ir1   ,IRTACT(1)), (ir2   ,IRTACT(2))
c
c...First time here
c
      ip1    = IRTINC(ir1)
      if (kfl .eq. 0) then
c
c......Determine C-axis direction
c......based on first step in case
c......this is a nutating head
c......(the C-axis does not maintain the same position
c......from the first step to the final position)
c
          ifl    = -1
          call mwudir (2)
          call mwlnrz (ifl,ierr,itva,ROTANC)
c
c......Could not move in the direction of the final rotary positions
c......Probably because this is a nutating head
c......Recalculate first step at shortest direction instead
c
          if (ierr .ne. 0) then
              call mwudir (1)
              call mwlnrz (ifl,ierr,itva,ROTANC)
          endif
c
c......Determine maximum rotation angle
c
          call getnpt (STONUL(1,2),ROTSTL,an1max)
          dlt    = ROTANC(ip1,2) - ROTSTL(ip1,2)
          rtim   = dabs(dlt) / an1max(ir1) + .5
          ntim   = rtim
          if (ntim .le. 1) ntim = 1
          MWCANG = dlt / ntim
          IMWCMX = ntim
          kfl    = 1
c
c...Linearize around C-axis
c
      else if (kfl .le. IMWCMX) then
          call copyn (STONUL(1,2),MCHNUM(1,2),3)
          call cpyrot (ROTSTL,ROTSTO)
          if (kfl .eq. IMWCMX) then
              ROTSTO(ip1,2) = ROTANC(ip1,2)
          else
              ROTSTO(ip1,2) = ROTSTL(ip1,2) + MWCANG
          endif
          HLDROT(ip1,2) = ROTSTO(ip1,2)
          call setbas (ROTSTO(1,2),ROTBAS)
          call linrot (ROTSTO,ROTBAS,0)
          call copyn (VECSAV,TLVEC,3)
          call copyn (TLVECL,VECSAV,3)
          call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,
     1                 1,0,ierr)
          kfl    = kfl    + 1
c
c...Final C-axis position has been reached
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
c   SUBROUTINE: mwshfv (kdir,gpt1,gta1,gpt2,gta2,gpt3,gta3,gvec,kdid)
c
c   FUNCTION:  This routine calculates a "cutter compensation" style
c              vector to use when shifting the tool during an automatic
c              retract procedure.
c
c   INPUT:  kdir    I*4  D1   -  2 = Create Left offset vector, 3 = Right.
c
c           gpt1    R*8  D3   -  From point.
c
c           gta1    R*8  D3   -  Tool axis for from point.
c
c           gpt2    R*8  D3   -  To point.
c
c           gta2    R*8  D3   -  Tool axis for to point.
c
c           gpt3    R*8  D3   -  Next point.
c
c           gta3    R*8  D3   -  Tool axis for next point.
c
c   OUTPUT: gvec    R*8  D3   -  Vector to use to shift the tool.
c
c           kfl     I*4  D1   -  1 = Was able to calculate vector, 0 = not.
c
c***********************************************************************
c
      subroutine mwshfv (kdir,gpt1,gta1,gpt2,gta2,gpt3,gta3,gvec,kfl)
c
      integer*4 kdir,kfl
c
      real*8 gpt1(3),gpt2(3),gpt3(3),gta1(3),gta2(3),gta3(3),gvec(3)
c
      integer*4 ierr,inc,i
c
      real*8 r1,r2,fvc1(3),fvc2(3),v1(3),v2(3),v3(3),p1(3),p2(3),pl1(4),
     1       rsgn,tol
      real*8 ndot,nmag
c
c...Initialize routine
c
      rsgn   = 1.
      if (kdir .eq. 2) rsgn = -1.
      tol    = .0001d0
c
c...Calculate forward vectors
c
      call gtfwvc (gpt1,gta1,gpt2,gta2,fvc1,kfl)
      if (kfl .eq. 0) go to 8000
      call gtfwvc (gpt2,gta2,gpt3,gta3,fvc2,kfl)
      if (kfl .eq. 0) go to 8000
c
c...Project first vector onto plane of tool
c
      call crosvc (gta2,fvc1,v3)
      call crosvc (v3,gta2,v1)
      r1 = nmag(v1)
c
c...Project second vector into plane of tool.
c
      call crosvc (gta2,fvc2,v3)
      call crosvc (v3,gta2,v2)
      r2 = nmag(v2)
c
c...If either vector is null,
c...then use other vector for both
c
      kfl    = 0
      if (r1 .lt. tol .and. r2 .lt. tol) then
          go to 8000
      else if (r1 .lt. tol) then
          call copyn (v2,v1,3)
      else if (r2 .lt. tol) then
          call copyn (v1,v2,3)
      endif
      call unitvc (v1,v1)
      call unitvc (v2,v2)
c
c...Subtract vectors to get bisecting vector
c
      call vcmnvc (v1,v2,v3)
      r1     = nmag(v3)
c
c...Extend bisecting vector to plane parallel to first vector
c
      ierr   = 0
      if (r1 .ge. tol) then
          call crosvc (v1,gta2,pl1)
          call vcplvc (gpt2,pl1,p1,1.d0)
          pl1(4) = ndot(pl1,p1)
          call unitvc (v3,v3)
          call plnint(gpt2,v3,pl1,p2,ierr)
      endif
c
c...If vectors are parallel
c...Use cross with tool axis
c
      if (r1 .lt. tol .or. ierr .eq. 1) then
          call crosvc (v1,gta2,v3)
          if (nmag(v3) .lt. 0.001d0) then
              call crosvc (v2,gta2,v3)
              if (nmag(v3) .lt. tol) go to 8000
          endif
      else
          call vcmnvc (p2,gpt2,v3)
      endif
c
c...Calculate final shift vector
c
      call unitvc (v3,v3)
      call vctmsc (v3,rsgn,gvec)
      kfl    = 1
c
c...Determine if cutting a closed angle
c...if so, then don't return offset vector
c
      r1     = -1.
      inc    = 0
      do 100 i=1,3,1
          if (dabs(gta2(i)) .gt. r1) then
              r1     = dabs(gta2(i))
              inc    = i
          endif
          v1(i) = 0.
  100 continue
      v1(inc) = 1.
      call betvec (gvec,v1,r2)
      if (r2 .gt. 90.0001) kfl = 0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: mwrdis (gpt,grot,gdis)
c
c     FUNCTION:  This routine calculates the distance to retract the tool
c                due to the longest route being taken.
c
c     INPUT:   gpt      R*8  D3    -  Current tool location in machine
c                                     coordinates (MCHNUM(1,3)).
c
c              grot     R*8  D20.2 -  Current rotary axes positions.
c
c     OUTPUT:  gdis     R*8  D1    -  Incremental distance to retract the
c                                     tool.
c
c
c***********************************************************************
c
      subroutine mwrdis (gpt,grot,gdis)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (LRTRCT,KPOSMP(1278)), (IFHEAD,KPOSMP(1281))
      equivalence (LRTTAD,KPOSMP(1335)), (ITP   ,KPOSMP(1801))
c
      integer*4 LRTTAD,ITP,LRTRCT,IFHEAD
c
      equivalence (RETPL ,POSMAP(2258)), (RETDIS,POSMAP(2279))
      equivalence (SPIVEC,POSMAP(3583)), (TL    ,POSMAP(3601))
c
      real*8 TL(120),RETDIS,RETPL(4),SPIVEC(3)
c
      real*8 gpt(3),grot(20,2),gdis
c
      integer*4 ierr
c
      real*8 pt(3),tvec(3),ndist,rnum(3)
c
c...Automatic retract is not active
c
      if (LRTRCT .eq. 0) then
          gdis   = 0.
c
c...Retract distance
c
      else if (LRTRCT .eq. 1) then
          gdis   = RETDIS
          if (LRTTAD .eq. 1) gdis = TL(ITP) + gdis
c
c...Retract to plane
c
      else
          call pivvec (grot,tvec)
c
c......Calculate tool end point
c......after rotated by table
c
          if (IFHEAD .eq. 1) then
              call pivadj (gpt,rnum,grot,1)
          else
              rnum(1) = gpt(1) - TL(ITP) * SPIVEC(1)
              rnum(2) = gpt(2) - TL(ITP) * SPIVEC(2)
              rnum(3) = gpt(3) - TL(ITP) * SPIVEC(3)
          endif
c
c......Intersect tool end point with plane
c......and calculate retract distance
c
          call plnint (rnum,tvec,RETPL,pt,ierr)
          if (ierr .ne. 0) then
              call psterr (2,'VECPLN',' ',-1)
              gdis   = 0.
          else
              gdis   = ndist(rnum,pt)
          endif
      endif
c
c...End of routine
c
 8000 return
      end
