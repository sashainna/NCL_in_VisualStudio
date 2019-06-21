c
c***********************************************************************
c
c   FILE NAME:  tlaxis
c   CONTAINS:
c               tlaxis  tlaxck  linrot  setbas  rotbas  rotlin  rotbeg
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tlaxis.f , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 12:00:52
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  tlaxis (gmch,gvec,glin,gaxs,grot,gbase,kbase,kfl,kerr)
c
c   FUNCTION:  This routine accepts as input, an XYZ position and tool
c              axis vector, and returns the following:
c
c                 1)  The rotary axes positions in both rotary and
c                     linear scales.
c                 2)  The modified base angles for each rotary.
c                 3)  The XYZ position after being adjusted for the
c                     rotaries.
c                 4)  The XYZ position after being modified for TRANS/
c                     LAST.
c                 5)  The Primary and Secondary linear axes.
c                 6)  The Primary and Secondary linear axes after being
c                     modified for TRANS/-AXIS.
c
c   INPUT:  gmch    R*8  D3.4 -  (n,2) = Input XYZ position.
c
c           gvec    R*8  D3   -  Tool axis vector.
c
c           gbase   R*8  D4   -  Current base angles for rotaries on a
c                                linear scale.
c
c           kbase   I*4  D1   -  1 = Adjust base angles for newly
c                                calculated rotations.
c
c           kfl     I*4  D1   -  1 = Output any error messages encoun
c                                tered in this routine.
c                                2 = output all errors except longest route
c
c   OUTPUT: gmch    R*8  D3.4 -  (n,3) = XYZ position adjusted for rota-
c                                ries.  (n,4) = XYZ position adjusted
c                                for TRANS/LAST.
c
c           glin    R*8  D6   -  Primary and secondary linear axis.
c
c           gaxs    R*8  D10  -  Primary and secondary linear axis ad-
c                                  justed for TRANS/-AXIS.
c
c           grot    R*8  D20.2 - (n,1) = Rotary angles on a rotary
c                                scale.  (n,2) = Rotary angles on a
c                                linear scale.
c
c           gbase   R*8  D4   -  Modified base angles for each rotary
c                                axes.  'gbase' is only changed when
c                                'kbase' = 1.
c
c           kerr    I*4  D1   -  1 = Both active rotary axes move about
c                                the same vector.  2 = Calculated ro-
c                                tary axes do not fulfill tool axis vec-
c                                tor.  3 = Longest route taken to stay
c                                within limits.  4 = Could not satisfy
c                                user requested direction.  5 = Z-axis
c                                is parallel to XYPLAN.  6 = Longest
c                                route forced with ROTABL/NEXT.  7 = Li-
c                                near axis limit error when TV is perpto
c                                table and it is possible to get the
c                                point after table rotation.
c
c***********************************************************************
c
      subroutine tlaxis (gmch,gvec,glin,gaxs,grot,gbase,kbase,kfl,kerr)
c
      include 'post.inc'
c
      equivalence (ICYCSW,KPOSMP(271))
      equivalence (MACHTP,KPOSMP(1201))
      equivalence (IRTNUM,KPOSMP(1243)), (IRTACT,KPOSMP(1256))
      equivalence (IRTRTE,KPOSMP(1343)), (IJKROT,KPOSMP(1739))
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (IRTINC,KPOSMP(1461))
      equivalence (IRTNLS,KPOSMP(1641)), (IRFNLS,KPOSMP(1640))
      equivalence (NROT  ,KPOSMP(1366)), (HLDFLG,KPOSMP(1622))
      equivalence (IRTSAM,KPOSMP(1724)), (KERRSV,KPOSMP(0109))
      equivalence (MROTTV,KPOSMP(1334))
      equivalence (MODROT,KPOSMP(4031)), (MDTLIN,KPOSMP(4032))
      equivalence (MODRAT,KPOSMP(4033)), (LTMODE,KPOSMP(4125))
c
      integer*4 IRTNUM,IRTACT(2),IRTNXT(4),IRTNXF,NROT,HLDFLG,IRFNLS,
     1          IRTNLS(4),IRTSAM,KERRSV,MROTTV,MODROT,MDTLIN,
     2          MACHTP,LTMODE,IRTRTE(2),MODRAT,IJKROT,IRTINC(4),ICYCSW
c
      equivalence (IJKVC ,POSMAP(0329)), (VECSAV,POSMAP(1372))
      equivalence (HLDVEC,POSMAP(2177)), (ROTBSV,POSMAP(2275))
      equivalence (ROTSTO,POSMAP(5213)), (HLDROT,POSMAP(5253))
c
      real*8 ROTSTO(20,2),HLDROT(20,2),ROTBSV(4),HLDVEC(3),VECSAV(3),
     -       IJKVC(3)
c
      integer*4 kbase,kfl,kerr
c
      real*8 gmch(3,4),gvec(3),glin(6),gaxs(10),grot(20,2),gbase(4),
     1       svec(3)
c
      integer*4 i,iout1,iout2,isub,ifl1(2),ifl2(2),ierr1,ierr2,irt,
     -          intr,irtf,ip1,ip2
c
      real*8 abc1(20,2),abc2(20,2),vecdif(3),rmch1(3,4),rmch2(3,4),
     1       rlin1(6),rlin2(6),raxs1(10),raxs2(10),rdlt1(3),rdlt2(3),
     2       base1(4),base2(4),rot1(2),rot2(2),srot(20,2),
     3       tmch(3),sav
c
      character*80 msg
c
c...Initialize routine
c
      intr   = 0
      kerr   = 0
      ip1    = IRTINC(IRTACT(1))
      ip2    = IRTINC(IRTACT(2))
      IRFNLS = IRTNXF
      do 30 i=1,4
          IRTNLS(i) = IRTNXT(i)
   30 continue
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2)
     -           call copyn (gmch(1,1),tmch,3)
c
c...This machine does not have
c...any rotary axes.
c...Adjust points only
c...vp 5/5/98 if tool axis vector is supported make sure it
c...is in common so adjustment will be correct
c
      if (IRTNUM .eq. 0 .or. MACHTP .eq. 4 .and. LTMODE .eq. 0) then
c     if (IRTNUM .eq. 0) then
          if (IJKROT .eq. 1) call copyn (gvec,IJKVC,3)
          call alladj (gmch,glin,gaxs,grot,1,5)
          go to 8000
      endif
c
c...Set up previous rotary position
c
      if (HLDFLG .eq. 1) then
          call cpyrot (ROTSTO,srot)
          call cpyrot (HLDROT,ROTSTO)
          call copyn (VECSAV,svec,3)
          call copyn (HLDVEC,VECSAV,3)
          call setbas (ROTSTO(1,2),gbase)
      endif
c
c...Save old base angle
c
      if (kbase .eq. 1) call copyn (gbase,ROTBSV,4)
c
c...Get 2 sets of rotary angles
c
      call cpyrot (ROTSTO,abc1)
      call cpyrot (ROTSTO,abc2)
      rot1(1) = abc1(ip1,1)
      rot2(1) = abc1(ip1,1)
      if (NROT .eq. 2) then
         rot1(2) = abc1(ip2,1)
         rot2(2) = abc1(ip2,1)
      end if
c
c...Check if current tool vector is same as old and
c...is PERPTO any table
c
      call perpto (irt)
      irtf  = 0
      if (irt .eq. 0) then
          if (MODROT .ne. 0 .and. MODRAT .eq. 0) then
              MODROT = 0
              MDTLIN = 0
              if (kfl .gt. 0) call psterr (1,'NOTPERP1','NOTPERP2',-1)
          end if
          call getang (gvec,rot1,rot2,vecdif,kerr)
          abc1(ip1,1) = rot1(1)
          abc2(ip1,1) = rot2(1)
          if (NROT .eq. 2) then
              abc1(ip2,1) = rot1(2)
              abc2(ip2,1) = rot2(2)
          endif
c
c...check again if current TV is PERPTO any table
c...and this table didn't move
c
          call perptp (abc1,ROTSTO,irt)
          if (irt .ne. 0) then
             irtf = 1
             call reprot2 (abc1,ROTSTO,sav,irt)
          end if
      end if
c
c...TV is PERPTO table, check linear limits
c
      if (irt .ne. 0) then
        if (MODROT .eq. 0) then
          if (KERRSV .eq. 0) then
              call limang (gmch,irt,kerr)
              if (irtf .eq. 1) call fixrot2 (sav,ROTSTO,irt)
              if (kerr .ne. 0) go to 8000
c
c...Use post generated points if limit error
c
          else if (KERRSV .eq. 7) then
              call limset (irt,abc1,abc2)
          end if
        else
          intr = 1
          if (MDTLIN .eq. 0) then
              call tbmang (gmch,raxs1,abc1,ierr1,0,irtf)
              MDTLIN = ierr1
              if (ierr1 .ne. 0) go to 8000
          end if
          call rtmset (irt,abc1,abc2)
        end if
        if (irtf .eq. 1) call fixrot2 (sav,ROTSTO,irt)
      end if
c
c......Error calculating axes
c
      if (kerr .eq. 1 .and. kfl .gt. 0) then
          call psterr (3,'SMROTAX1','SMROTAX2',-1)
      else if (kerr .eq. 2 .and. kfl .gt. 0) then
          call perrst ('LEFTVEC',1,msg,0,vecdif(1),msg,2)
          call perrst (msg,2,msg,0,vecdif(2),msg,2)
          call perrst (msg,2,msg,0,vecdif(3),msg,2)
          call psterr (3,'NOROTSOL',msg,-1)
      else if (kerr .eq. 5 .and. kfl .gt. 0) then
          call psterr (3,'ZPARLEL',' ',-1)
      endif
c
c...Determine best solution
c...for 1st set of axes
c
      do 200 i=1,4,1
          base1(i) = gbase(i)
          rmch1(1,i) = gmch(1,i)
          rmch1(2,i) = gmch(2,i)
          rmch1(3,i) = gmch(3,i)
  200 continue
      call tlaxck (rmch1,rlin1,raxs1,abc1,base1,kbase,rdlt1,iout1,ifl1,
     1            ierr1)
c
c...Determine best solution
c...for 2nd set of axes
c
      if (NROT .lt. 2) then
          isub   = 1
          rdlt2(1) = rdlt1(1) + 1.
          rdlt2(2) = rdlt1(1) + 1.
          rdlt2(3) = rdlt1(3) + 1.
      else
          do 300 i=1,4,1
              base2(i) = gbase(i)
              rmch2(1,i) = gmch(1,i)
              rmch2(2,i) = gmch(2,i)
              rmch2(3,i) = gmch(3,i)
  300     continue
          call tlaxck (rmch2,rlin2,raxs2,abc2,base2,kbase,rdlt2,iout2,
     1                 ifl2,ierr2)
c
c......Determine best solution
c......using both sets of axes
c
          IRTSAM = 0
          if (IRTNXF .eq. 2) then
              isub   = 1
              if (rdlt2(1) .lt. rdlt1(1)) isub = 2
          else if (iout1 .eq. 0 .and. iout2 .ne. 0) then
              isub   = 1
          else if (iout1 .ne. 0 .and. iout2 .eq. 0) then
              isub   = 2
          else if (ifl1(1) .eq. 1 .and. ifl2(1) .eq. 0) then
              isub   = 1
          else if (ifl1(1) .eq. 0 .and. ifl2(1) .eq. 1) then
              isub   = 2
          else
              isub   = 1
              if (rdlt2(1) .lt. rdlt1(1)) isub = 2
              if (rdlt2(1) .eq. rdlt1(1)) IRTSAM = 1
          endif
c
c......For single axis criteria check if rdlt(3) is better
c......for selected set when both sets have same rdlt(1)
c
          if (IRTRTE(1) .eq. 4) then
             if (rdlt1(1) .eq. rdlt2(1) .and. ifl1(1) .eq. ifl2(1) .and.
     -           iout1 .eq. iout2) then
                isub = 1
                if (rdlt2(3) .lt. rdlt1(3)) isub = 2
             end if
          end if
      endif
c
c...First set of axes was chosen
c
      if (isub .eq. 1) then
          call copyn (rmch1,gmch,12)
          call cpyrot (abc1,grot)
          call copyn (base1,gbase,4)
          call copyn (rlin1,glin,6)
          call copyn (raxs1,gaxs,10)
c
          if (ierr1 .eq. 1 .or.
     1        (rdlt1(1) .gt. rdlt2(1) .and. (ifl2(1) .eq. 1 .or.
     2                                       ifl1(1) .eq. ifl2(1))) .or.
     3        (rdlt1(1) .gt. rdlt2(2) .and. ierr2 .eq. 1)) then
              if (kerr .eq. 0) kerr = 3
              if (kfl .eq. 1) call psterr (2,'LONGRTE','NOTDESIR',-1)
          endif
c
          if ((rdlt1(1) .gt. rdlt1(2) .or. rdlt1(1) .gt. rdlt2(2)) .and.
     1        kerr .eq. 0) kerr = 6
c
          if (ifl1(1) .eq. 0) then
              if (kerr .eq. 0) kerr = 4
              if (kfl .gt. 0) call psterr (2,'ROTNOSAT','NOTDESIR',-1)
          endif
c
c...Second set of axes was chosen
c
      else
          call copyn (rmch2,gmch,12)
          call cpyrot (abc2,grot)
          call copyn (base2,gbase,4)
          call copyn (rlin2,glin,6)
          call copyn (raxs2,gaxs,10)
c
          if (ierr2 .eq. 1 .or.
     1        (rdlt2(1) .gt. rdlt1(1) .and. (ifl1(1) .eq. 1 .or.
     2                                       ifl2(1) .eq. ifl1(1))) .or.
     3        (rdlt2(1) .gt. rdlt1(2) .and. ierr1 .eq. 1)) then
              if (kerr .eq. 0) kerr = 3
              if (kfl .eq. 1) call psterr (2,'LONGRTE','NOTDESIR',-1)
          endif
c
          if ((rdlt2(1) .gt. rdlt2(2) .or. rdlt2(1) .gt. rdlt1(2)) .and.
     1        kerr .eq. 0) kerr = 6
c
          if (ifl2(1) .eq. 0) then
              if (kerr .eq. 0) kerr = 4
              if (kfl .gt. 0) call psterr (2,'ROTNOSAT','NOTDESIR',-1)
          endif
      endif
      IRTNXF = 0
      IRTNXT(1) = 0
      IRTNXT(2) = 0
      IRTNXT(3) = 0
      IRTNXT(4) = 0
c
c...Fix round off problems with linear axes
c...If MODE/ROTATE is in effect
c
      if (ICYCSW .eq. 0) then
	      if (MODROT .ne. 0) call fixaxs (gmch,glin,gaxs,grot)
	  endif
c
c...Fix angle for Lathe/Mill in LMDP mode
c
      if (MACHTP .eq. 4 .and. LTMODE .eq. 2) then
          call fixang1 (tmch,grot,gbase,gaxs,kbase)
      end if
c
c...Restore previous rotary position
c
      if (intr .eq. 1 .and. kerr .eq. 3) MDTLIN = 3
      if (HLDFLG .eq. 1) then
          call cpyrot (srot,ROTSTO)
          call copyn (svec,VECSAV,3)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  tlaxck (gmch,glin,gaxs,grot,gbase,kbase,gdlt,kout,kfl,
c                        kerr)
c
c   FUNCTION:  This routines' main function is to determine which set of
c              rotary axes to use depending on the following rules.
c
c                 1)  Machine limits.
c                 2)  Users' choice of direction.
c                 3)  Least rotary movement.
c
c              It returns the following information.
c
c                 1)  The best choice rotary axes positions.
c                 2)  The linear axes positions.
c                 3)  The modified base angles for each rotary.
c
c   INPUT:  gmch    R*8  D3.4 -  (n,2) = Input XYZ position.
c
c           grot    R*8  D20.2 - (n,1) = Rotary angles on a rotary
c                                scale.
c
c           gbase   R*8  D4   -  Current base angles for rotaries on a
c                                linear scale.
c
c           kbase   I*4  D1   -  1 = Adjust base angles for newly
c                                calculated rotations.
c
c   OUTPUT: gmch    R*8  D3.4 -  (n,3) = XYZ position adjusted for rota-
c                                ries.  (n,4) = XYZ position adjusted
c                                for TRANS/LAST.
c
c           glin    R*8  D6   -  Primary and secondary linear axis.
c
c           gaxs    R*8  D10  -  Primary and secondary linear axis ad-
c                                justed for TRANS/-AXIS.
c
c           grot    R*8  D20.2-  (n,1) = Rotary angles on a rotary
c                                scale.  (n,2) = Rotary angles on a
c                                linear scale.
c
c           gbase   R*8  D4   -  Modified base angles for each rotary
c                                axes.  'gbase' is only changed when
c                                'kbase' = 1.
c
c           gdlt    R*8  D2   -  1 = Total delta movement of all rota-
c                                ries for chosen angle.  2 = Least
c                                rotary movement for both set of angles.
c
c           kout    I*4  D1   -  1 = Axes are out of limits.
c
c           kfl     I*4  D2   -  1 = Users' requirements were met. (1) =
c                                for chosen angles.  (2) = for shortest
c                                route angles.
c
c           kerr    I*4  D1   -  1 = Longest route was taken to meet
c                                limit requirements.
c
c***********************************************************************
c
      subroutine tlaxck (gmch,glin,gaxs,grot,gbase,kbase,gdlt,kout,kfl,
     1                   kerr)
c
      include 'post.inc'
c
      equivalence (IRTACT,KPOSMP(1256))
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (NROT  ,KPOSMP(1366))
      equivalence (IRTRTE,KPOSMP(1343)), (IRTFRC,KPOSMP(1777))
c
      integer*4 IRTACT(2),IRTNXT(4),IRTNXF,NROT,IRTRTE(2),
     1          IRTFRC
c
      integer*4 kbase,kout,kfl(2),kerr
c
      real*8 gmch(3,4),glin(6),gaxs(10),grot(20,2),gbase(4),gdlt(3)
c
      integer*4 i,j,idir(4,4),ie,ifl,isub,iout,iaxs(10),isw
c
      real*8 abc(20,2),rmch(3,4),base(4,4),rlin(6),dlt(4),rdlt,raxs(10),
     -       smr
c
c...Initialize routine
c
      call cpyrot (grot,abc)
      do 100 i=1,4,1
          rmch(1,i) = gmch(1,i)
          rmch(2,i) = gmch(2,i)
          rmch(3,i) = gmch(3,i)
          do 80 j=1,4,1
             base(i,j) = gbase(i)
             idir(i,j) = 3
   80     continue
  100 continue
c
      ie     = 1
      if (NROT .eq. 1) ie = 2
      if (NROT .ge. 2) ie = 4
c
c...Calculate next set of angles
c
      do 1000 i=1,ie,1
c
c......Get linear scale rotaries
c
          call rotlin (abc,base(1,i),kbase,idir(1,i),dlt)
c
c......Determine if this meets rotation criteria
c
          ifl    = 1
          if (IRTNXF .eq. 1) then
              do 400 j=1,NROT,1
                  isub   = IRTACT(j)
                  if (IRTNXT(isub) .ne. 0 .and.
     1                idir(isub,i) .ne. IRTNXT(isub)) ifl = 0
                  if (IRTFRC .eq. 2 .and.
     1               ((IRTNXT(isub) .eq. 0 .and. dlt(isub) .ne. 0.) .or.
     2                (IRTNXT(isub) .ne. 0 .and. dlt(isub) .eq. 0.)))
     3                    ifl = 0
  400         continue
          endif
c
c......Adjust points for rotations
c
  450     call alladj (rmch,rlin,raxs,abc,1,5)
c
c......Check limits
c
          call lmtchk (raxs,iout,iaxs,0)
c
c......Calculte rotary distance based on
c......user's preference for shortest route
c
c.........Largest movement
c
          smr   = dabs(dlt(1)) + dabs(dlt(2)) + dabs(dlt(3)) +
     1            dabs(dlt(4))
          if (IRTRTE(1) .eq. 2) then
              rdlt = dmax1(dabs(dlt(1)),dabs(dlt(2)),dabs(dlt(3)),
     1                     dabs(dlt(4)))
c
c.........Summation of all movements
c
          else if (IRTRTE(1) .eq. 3) then
              rdlt   = smr
c
c.........User specified axis
c
          else if (IRTRTE(1) .eq. 4) then
              rdlt   = dabs(dlt(IRTRTE(2)))
c
c.........Combination of all movements
c
          else
              rdlt   = dsqrt(dlt(1)**2 + dlt(2)**2 + dlt(3)**2 +
     1                       dlt(4)**2)
          endif
c
c......Determine if we want to use this set
c
          isw    = 0
          call dpoint (rdlt,rdlt,6)
          call dpoint (smr,smr,6)
          if (i .eq. 1) then
              isw    = 1
          else if (IRTNXF .eq. 2) then
               if (rdlt .lt. gdlt(1)) isw = 1
          else if (iout .eq. 0 .and. ifl .eq. 1 .and. (kout .ne. 0 .or.
     1             kfl(1) .eq. 0)) then
              isw    = 1
          else if ((iout .eq. 0 .or. iout .eq. kout) .and.
     1             (ifl .eq. 1 .or. ifl .eq. kfl(1))) then
               if (rdlt .lt. gdlt(1)) isw = 1
          else if (iout .eq. 0 .and. kout .ne. 0) then
              isw    = 1
          endif
c
c......This set is the best one so far
c
          if (isw .eq. 1) then
              call cpyrot (abc,grot)
              do 600 j=1,4,1
                  gmch(1,j) = rmch(1,j)
                  gmch(2,j) = rmch(2,j)
                  gmch(3,j) = rmch(3,j)
                  gbase(j) = base(j,i)
  600         continue
c
              do 700 j=1,6,1
                  glin(j) = rlin(j)
  700         continue
c
              do 750 j=1,10,1
                  gaxs(j) = raxs(j)
  750         continue
c
              kout   = iout
              kfl(1) = ifl
              gdlt(1) = rdlt
              gdlt(3) = smr
c
c.........Check for longest route
c
              kerr   = 0
              if (i .eq. 1 .or. gdlt(1) .lt. gdlt(2)) then
                  gdlt(2) = gdlt(1)
                  kfl(2) = kfl(1)
              else
                  if (kfl(1) .eq. 0 .or. kfl(2) .eq. 1) kerr = 1
              endif
c
              if (i .eq. 1 .and. kout .eq. 0 .and. kfl(1) .eq. 1)
     1                go to 8000
          endif
c
c......Set up rotation directions
c......for next set of axes
c
          if (i .eq. 1) then
              if (NROT .ge. 1) then
                  isub   = IRTACT(1)
                  idir(isub,2) = 3 - idir(isub,1)
                  idir(isub,3) = idir(isub,1)
                  idir(isub,4) = idir(isub,2)
              endif
c
              if (NROT .ge. 2) then
                  isub   = IRTACT(2)
                  idir(isub,2) = idir(isub,1)
                  idir(isub,3) = 3 - idir(isub,1)
                  idir(isub,4) = idir(isub,3)
              endif
          endif
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  linrot (grot,gbase,kbase)
c
c   FUNCTION:  This routine calculates the rotary axes on a rotary scale
c              based on the linear scale angles.
c
c   INPUT:  grot    R*8  D20.2-  (n,2) = Rotary axes on a linear scale.
c
c           kbase   I*4  D1   -  1 = Adjust base angles for input ro-
c                                tations.
c
c   OUTPUT: grot    R*8  D20.2-  (n,1) = Rotary axes on a rotary scale.
c
c           gbase   R*8  D4   -  Current base angles for rotaries on a
c                                linear scale.
c
c***********************************************************************
c
      subroutine linrot (grot,gbase,kbase)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381))
      equivalence (IRTOUT,KPOSMP(0813)), (IRTINC,KPOSMP(1461))
c
      integer*4 MOTREG(24),IRTOUT(4),IRTINC(4)
c
      integer*4 kbase
c
      real*8 grot(20,2),gbase(4)
c
      integer*4 iax(4),inum,i
c
      real*8 ang
c
      data iax /13,16,19,22/
c
c...Calculate angles on a rotary scale
c
      do 1000 i=1,4,1
          if (IRTOUT(i) .eq. 0) go to 1000
          call codint (MOTREG(iax(i)),grot(IRTINC(i),2),ang,inum)
          grot(IRTINC(i),1) = dmod(ang,360.d0)
          if (grot(IRTINC(i),1) .lt. 0.)
     1        grot(IRTINC(i),1) = grot(IRTINC(i),1) + 360.
c
c...Calculate base angle
c
          if (kbase .eq. 1) then
              call rotbas (ang,gbase(i))
cc              rnum   = dint (ang/360.d0)
cc              gbase(i) = rnum   * 360.d0
cc              if (ang .lt. 0. .and. grot(i,1) .ne. 0)
cc     1            gbase(i) = gbase(i) - 360.d0
          endif
 1000 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  setbas (grot,gbase)
c
c   FUNCTION:  This routine calculates the base angles for all rotary
c              axes on a linear scale.
c
c   INPUT:  grot    R*8  D20  -  Rotary axis on a linear scale.
c
c   OUTPUT: gbase   R*8  D1   -  Calculated base angle.
c
c***********************************************************************
c
      subroutine setbas (grot,gbase)
c
      include 'post.inc'
c
      equivalence (IRTINC,KPOSMP(1461))
c
      integer*4 IRTINC(4)
c
      real*8 grot(20),gbase(4)
c
      integer*4 i
c
c...Set base angles for all rotaries
c
      do 100 i=1,4,1
          call rotbas (grot(IRTINC(i)),gbase(i))
  100 continue
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotbas (grot,gbase)
c
c   FUNCTION:  This routine calculates the base angle for an input angle
c              on a linear scale.
c
c   INPUT:  grot    R*8  D1   -  Rotary axis on a linear scale.
c
c   OUTPUT: gbase   R*8  D1   -  Calculated base angle.
c
c***********************************************************************
c
      subroutine rotbas (grot,gbase)
c
      real*8 grot,gbase
c
      real*8 rnum
c
c...Calculate base angle
c
      rnum   = dint (grot/360.d0)
      gbase  = rnum   * 360.d0
      if (grot .lt. 0. .and. dmod(grot,360.d0) .ne. 0)
     1    gbase = gbase - 360.d0
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotlin (grot,gbase,kbase,kdir,gdlt)
c
c   FUNCTION:  This routine calculates the rotary axes on a linear scale
c              based on the rotary scale angles.
c
c   INPUT:  grot    R*8  D20.2-  (n,1) = Rotary axes on a rotary scale.
c
c           gbase   R*8  D4   -  Current base angles for rotaries on a
c                                linear scale.
c
c           kbase   I*4  D1   -  1 = Adjust base angles for newly
c                                calculated rotations.
c
c           kdir    I*4  D4   -  Direction to rotate angles.  1 = CLW,
c                                2 = CCLW, 3 = Based on shortest route.
c
c   OUTPUT: grot    R*8  D20.2-  (n,2) = Rotary axes on a linear scale.
c
c           gbase   R*8  D4   -  See INPUT.
c
c           kdir    I*4  D4   -  See INPUT.  A value of 3 will not be
c                                returned.
c
c           gdlt    R*8  D4   -  Delta movement for each rotary axes.
c
c***********************************************************************
c
      subroutine rotlin (grot,gbase,kbase,kdir,gdlt)
c
      include 'post.inc'
c
      equivalence (MOTREG,KPOSMP(0381)), (MACHTP,KPOSMP(1201))
      equivalence (IRTINC,KPOSMP(1461)), (IRTDEF,KPOSMP(1485))
c
      integer*4 MOTREG(24),MACHTP,IRTDEF,IRTINC(4)
c
      equivalence (ROTSTO,POSMAP(5213))
c
      real*8 ROTSTO(20,2)
c
      integer*4 kbase,kdir(4)
c
      real*8 grot(20,2),gbase(4),gdlt(4)
c
      integer*4 iax(4),inum,i
c
      real*8 ang,base,rd,sta
c
      data iax /13,16,19,22/
c
c...Calculate angles on a rotary scale
c
      do 1000 i=1,IRTDEF,1
          call codint (MOTREG(iax(i)),grot(IRTINC(i),1),ang,inum)
          if (ang .eq. 360.d0) ang = 0.d0
          call codint (MOTREG(iax(i)),ROTSTO(IRTINC(i),1),sta,inum)
          if (sta .eq. 360.d0) sta = 0.d0
          base   = gbase(i)
c
c......Determine shortest direction
c
          if (kdir(i) .eq. 3) then
              kdir(i) = 1
              rd     = ang    - sta
c
c......if rd=180 and direction not requested use position
c......closest to 0.
c
              if (dabs(rd) .eq. 180.d0) then
                 if (rd .gt. 0. .and. ROTSTO(IRTINC(i),2) .gt. 0.)
     1               kdir(i) = 2
              else
                 if (rd .gt. 180. .or. (rd .gt. -180. .and. rd .lt. 0.))
     1                kdir(i) = 2
              end if
          endif
c
c......CLW direction
c
          if (kdir(i) .eq. 1 .and. ang .lt. sta)
     1            base = base + 360.
c
c......CCLW direction
c
          if (kdir(i) .eq. 2 .and. ang .gt. sta)
     1            base = base - 360.
c
c......Calculate angle and delta
c
          grot(IRTINC(i),2) = base   + ang
          gdlt(i) = grot(IRTINC(i),2) - ROTSTO(IRTINC(i),2)
c
c......Fixes a problem where the base angle has been
c......recalculated during a move which is not output
c......due to PPTOL.  Therefore the current base angle
c......does not match the stored axis.
c
      if (ang .eq. sta .and. dabs(gdlt(i)) .ge. 360.) then
          call rotbas (sta,base)
          grot(IRTINC(i),2) = base   + ang
          gdlt(i) = grot(IRTINC(i),2) - ROTSTO(IRTINC(i),2)
      endif
c
c......Adjust base angle
c
          if (kbase .eq. 1) gbase(i) = base
 1000 continue
c
c...Zero out non-supported axes
c
      if (IRTDEF .lt. 4) then
          do 1500 i=IRTDEF+1,4,1
              grot(IRTINC(i),2) = 0.
              gdlt(i) = 0.
 1500     continue
      endif
c
c...Restore Blade angle
c
      if (MACHTP .eq. 3 .and. IRTDEF .lt. 4) grot(4,2) = ROTSTO(4,2)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  rotbeg
c
c   FUNCTION:  This routine calls the rotary initialization routine and
c              outputs any appropriate errors.  This routine should be
c              called at the beginning of the program, when the active
c              rotary axes change and whenever a dead rotary axis moves.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rotbeg
c
      include 'post.inc'
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (NCONTB,KPOSMP(1347)), (NROT  ,KPOSMP(1366))
      equivalence (IRDEAD,KPOSMP(1465)), (IRTDEF,KPOSMP(1485))
      equivalence (IRTYPE,KPOSMP(1486))
c
      integer*4 NROT,IRTNUM,IRDEAD(20),NCONTB,IRTYPE(20),IRTDEF
c
      integer*4 ierr,i,n
c
c...Initialize rotary axes
c
      if (NCONTB .eq. 2) then
          n = 0
          do 110 i=1,IRTNUM,1
             if (IRTYPE(i) .eq. 1 .and. IRDEAD(i) .eq. 0)
     -           n = n + 1
  110     continue
          if (n .gt. 1) then
             do 115 i=2,IRTNUM,1
                if (IRTYPE(i) .eq. 1 .and. IRDEAD(i) .eq. 0)
     -              IRDEAD(i) = 1
  115        continue
c            call psterr (2,'LOCK2R',' ',-1)
          end if
      end if
c
c...7/18/97 make sure that at least IRTNUM number of axes is
c...active
c
      NROT = IRTDEF
      if (NROT .gt. 2) NROT = 2
c
      call rotini (ierr)
c
c......Both rotary axes rotate
c......around the same vector
c......Automatically lock 2nd axis
c
      if (ierr .eq. 1) then
c         call psterr (2,'LOCK2R',' ',-1)
          NROT  = 1
c
c......Rotary axes rotate around spindle
c......spindle axis
c......Locked both axes
c
      else if (ierr .ne. 0) then
ccc       call psterr (2,'LOCKALLR',' ',-1)
          NROT  = 0
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE:
c
c   FUNCTION:  This routine calls the rotary initialization routine and
c              outputs any appropriate errors.  This routine should be
c              called at the beginning of the program, when the active
c              rotary axes change and whenever a dead rotary axis moves.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine fixang1 (gmch,grot,gbase,gaxs,kbase)
c
      include 'post.inc'
      real*8 gmch(3),grot(20,2),gbase(4),gaxs(10)
      integer*4 kbase
c
      equivalence (LTHXY ,KPOSMP(1225)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (NCPLAN,KPOSMP(4226))
c
      integer*4 LTHXY,IRTACT(2),NCPLAN(3),IRTINC(4)
c
      equivalence (RAD   ,POSMAP(0002)), (PRTRAD,POSMAP(4604))
c
      real*8 PRTRAD,RAD
c
      real*8 a,bas,ang,rnum
c
      integer*4 ir1,n,iax(5),ip1
c
      data iax /1,2,3,1,2/
c
      ir1   = IRTACT(1)
      ip1   = IRTINC(ir1)
      ang   = gmch(2) * RAD / PRTRAD
c
c...Adjust angle for spindle_vector/primary_axis misplacement
c
      if (NCPLAN(3) .ne. iax(NCPLAN(2)+1)) ang = ang - 90.0
c
c...Fix rotary axis and base angle to wind up
c...acording to part circumference
c
      rnum  = dabs (ang - grot(ip1,2))
      if (rnum .gt. .002) then
         a  = ang
         if (a .lt. 0.) a = a - 360.d0
         n  = a / 360.d0
         bas  = n * 360.d0
         grot(ip1,2) = grot(ip1,1) + bas
         gaxs(6+ir1) = grot(ip1,2)
         if (kbase .eq. 1) gbase(ir1) = bas
      end if
c
      return
      end
