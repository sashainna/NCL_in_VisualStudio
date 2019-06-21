c
c***********************************************************************
c
c     FILE NAME: tablin
c
c     CONTAINS:  tablin  limset  aclook  acposc  aclmtc
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        tablin.f , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:13
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: tablin (kflg,kerr)
c
c     FUNCTION:  This routine generates motion when single move will
c                cross the limit of linear axis but there is possible
c                tool path to avoid limit violation rotating table
c                which axis is parallel to tool vector.  General there
c                is move (1) to point on the limit plane, optional tool
c                retract (2), table rotation with tool reposition at the
c                limit point (3), tool plunge (4) if (2), move to the
c                destination point (5).
c
c     INPUT:   kflg     I*4  D1  -  Should be set to 0 on initial call.
c
c              kerr     I*4  D1  -  tlaxis call error status: must be 7
c                                   to call this routine.
c
c     OUTPUT:  kflg     I*4  D1  -  Set to non-zero if this routine needs
c                                   to be called again.
c
c***********************************************************************
c
      subroutine tablin (kflg,kerr)
c
      include 'post.inc'
c
      equivalence (LRTRCT,KPOSMP(1278)), (NROT  ,KPOSMP(1366))
      equivalence (IRAP  ,KPOSMP(3199))
      equivalence (KERRSV,KPOSMP(0109)), (LIMERR,KPOSMP(0110))
      equivalence (NRLTAB,KPOSMP(0111)), (IRTACT,KPOSMP(1256))
      equivalence (IFLNRS,KPOSMP(1731)), (ITP   ,KPOSMP(1801))
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 LRTRCT,NROT,IRAP,KERRSV,ITP,HLDFLG,
     -          LIMERR,NRLTAB,IRTACT(2),IFLNRS
c
      equivalence (MCHNUM,POSMAP(1287)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372)), (STONUM,POSMAP(1387))
      equivalence (AXSOUT,POSMAP(1340))
      equivalence (RETFED,POSMAP(2254)), (PTPOST,POSMAP(2472))
      equivalence (TBPOST,POSMAP(4585)), (PTDEST,POSMAP(2477))
      equivalence (TBLIML,POSMAP(2480))
      equivalence (LINAXS,POSMAP(1299)), (ROTBAS,POSMAP(1435))
      equivalence (TL    ,POSMAP(3601)), (PTLPLU,POSMAP(2301))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 MCHNUM(3,4),VECSAV(3),STONUM(3,4),ROTSTO(20,2),
     -       TLVEC(3),AXSOUT(10),PTPOST(3),LINAXS(6),
     -       TBPOST(2),PTDEST(3),TBLIML(2),ROTANG(20,2),ROTBAS(4),
     -       TL(120),PTLPLU(3),RETFED(4)
c
      integer*4 kerr,kflg
c
      integer*4 ierr,irro,ir1,ir2
c
      equivalence (IRTACT(1),ir1), (IRTACT(2),ir2)
c
      real*8 rdis
c
c...set flags
c
      irro   = 1
      if (kflg .eq. 0) then
          IFLNRS = 0
          KERRSV = kerr
      end if
      if (KERRSV .eq. 7) irro = 0
c
c...switch entry for subsequent calls
c
      go to (200,500,600,700,800) kflg + 1
      go to 8000
c
c...Initail call, use point on limit
c
  200 MCHNUM(1,2) = PTPOST(1)
      MCHNUM(2,2) = PTPOST(2)
      MCHNUM(3,2) = PTPOST(3)
      TBPOST(1) = ROTSTO(NRLTAB,1)
      TBPOST(2) = ROTSTO(NRLTAB,1)
      kflg   = 1
      go to 2000
c
c...Retract if flag is set
c
  500 if (LRTRCT .eq. 0) then
          go to 600
      else
          IFLNRS = 1
          if (HLDFLG .eq. 1) call clrmot (1)
          call mwrdis (STONUM,ROTSTO,rdis)
          MCHNUM(1,2) = PTPOST(1) + rdis * VECSAV(1)
          MCHNUM(2,2) = PTPOST(2) + rdis * VECSAV(2)
          MCHNUM(3,2) = PTPOST(3) + rdis * VECSAV(3)
          call copyn (MCHNUM(1,2),PTLPLU,3)
          call linfed (1,RETFED)
          kflg   = 2
      end if
      go to 2000
c
c...output limit point with complimentary table position
c
  600 TBPOST(1) = TBLIML(1)
      TBPOST(2) = TBLIML(2)
      if (LRTRCT .ne. 0) then
          if (HLDFLG .eq. 1) call clrmot (1)
          call copyn (PTLPLU,MCHNUM(1,2),3)
          call linfed (3,RETFED)
      else
          call copyn (PTPOST,MCHNUM(1,2),3)
      end if
      kflg   = 3
      go to 2000
c
c...Plunge the tool back
c
  700 if (LRTRCT .eq. 0) go to 800
      if (HLDFLG .eq. 1) call clrmot (1)
      call copyn (PTPOST,MCHNUM(1,2),3)
      call raprst
      call linfed (2,RETFED)
      kflg   = 4
      go to 2000
c
c...Turn off offlimit logic and
c...move to the final point
c
  800 if (HLDFLG .eq. 1) call clrmot (1)
      call copyn (PTDEST,MCHNUM(1,2),3)
      if (IFLNRS .eq. 1) call raprst
      if (IFLNRS .eq. 2) call linfed (5,RETFED)
      kflg   = -1
      LIMERR = 0
      KERRSV = 0
      go to 8000
c
c...Get new point axes
c
 2000 call tlaxis (MCHNUM,TLVEC,LINAXS,AXSOUT,ROTANG,ROTBAS,1,1,ierr)
      if (ierr .eq. 7) then
         LIMERR = 0
         call psterr (1,'NOROTB1','NOROTB2',-1)
      end if
      KERRSV = LIMERR
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: limset (krt,gabc1,gabc2)
c
c     FUNCTION:  This routine returns table angles used in tlaxis routine
c                instead of call getang (used when tool vector change
c                generates rotary angles).  'limset' is used when tool
c                vector is perpto to table plane and linear axis limit
c                can be avoided at different table position.
c                destination point (5).
c
c     INPUT:   krt      I*4  D1  -  Table axis number.
c                                   n < 0 - retract procedure (1,2,3,4),
c                                   n - following calls.
c
c              gabc1    R*8  D20 -  The first set of rotary axes
c                                   positions (rotary scale).
c
c              gabc2    R*8  D20 -  The second set of rotary axes
c                                   positions (rotary scale).
c
c***********************************************************************
c
      subroutine limset (krt,gabc1,gabc2)
c
      integer*4 krt
      real*8 gabc1(*),gabc2(*)
c
      include 'post.inc'
c
      equivalence (TBPOST,POSMAP(4585))
c
      real*8 TBPOST(2)
c
      gabc1(krt) = TBPOST(1)
      gabc2(krt) = TBPOST(2)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: aclook (kflg)
c
c     FUNCTION:  This routine looks ahead to the next record to see if
c                the C-axis moves when the tool axis is currently 0,0,1.
c                If so, then set the C-axis to it's next angle.
c
c     INPUT:   kflg     I*4  D1  -  Should be set to 0 on initial call.
c                                   1 = Move to final position.  4 =
c                                   Position C-axis, 5 = Plunge tool.
c
c     OUTPUT:  kflg     I*4  D1  -  1 = C-axis was positioned.  4 = Tool was
c                                   retracted, 5 = C-axis was positioned
c                                   after retract.
c
c
c***********************************************************************
c
      subroutine aclook (kflg)
c
      include 'post.inc'
      include 'lintol.inc'
c
      equivalence (ISUBT ,KPOSMP(0004))
      equivalence (FRMFLG,KPOSMP(1211)), (IRTACT,KPOSMP(1256))
      equivalence (IRTINC,KPOSMP(1461)), (HLDFLG,KPOSMP(1622))
      equivalence (IACHFL,KPOSMP(1646)), (IACNXT,KPOSMP(1647))
      equivalence (IACFLG,KPOSMP(1648)), (IFLNRS,KPOSMP(1731))
      equivalence (IRAP  ,KPOSMP(3199)), (LTMODE,KPOSMP(4125))
      equivalence (MTPDYN,KPOSMP(4126))
c
      integer*4 IACHFL,IACNXT,ISUBT,FRMFLG,IRAP,IRTACT(2),IACFLG(5),
     1          IFLNRS,HLDFLG,MTPDYN,LTMODE,IRTINC(4)
c
      equivalence (MCHNUM,POSMAP(1287))
      equivalence (TLVEC ,POSMAP(1369)), (VECSAV,POSMAP(1372))
      equivalence (STONUM,POSMAP(1387)), (ROTBAS,POSMAP(1435))
      equivalence (RETFED,POSMAP(2254))
      equivalence (HLDVEC,POSMAP(2177)), (ROTBSV,POSMAP(2275))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 TLVEC(3),ROTSTO(20,2),MCHNUM(3,4),STONUM(3,4),VECSAV(3),
     1       ROTBSV(4),ROTANG(20,2),ROTBAS(4),RETFED(4),HLDROT(20,2),
     2       HLDVEC(3)
c
      integer*4 kflg
c
      integer*4 ierr,ifrc,inc,ir1,ir2,ihld,ifl,itva,ip1,ip2
c
      real*8 abc(2),abc1(2),dvec(4),rsto(20,2),hrot(20,2),hvec(3),d1,
     1       rtmp(20,2),mtmp(3),ttmp(3),rang(20)
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
cc	character*80 cbuf
c
c...Use held back motion positions
c
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
      ihld   = HLDFLG
      if (MTPDYN .eq. 2 .and. LTMODE .eq. 0) go to 8000
      if (HLDFLG .eq. 1) then
          call cpyrot (ROTSTO,hrot)
          call cpyrot (HLDROT,ROTSTO)
          call copyn (VECSAV,hvec,3)
          call copyn (HLDVEC,VECSAV,3)
          HLDFLG = 0
      endif
c
c...C-axis was forced on last block
c...Output normal move
c
      if (kflg .eq. 1) then
          call copyn (MCHNLK,MCHNUM(1,2),3)
          call copyn (VECNLK,TLVEC,3)
          call linfed (IFLNRS+3,RETFED)
          kflg   = 0
          IACHFL = 0
          go to 8000
c
c...Tool was retracted last move
c...force C-axis rotation
c
      else if (kflg .eq. 4) then
          call cpyrot (ROTSLK,ROTSTO)
          call cpyrot (ROTSLK,ROTANG)
          call copyn (STONUM(1,2),MCHNUM(1,2),3)
          call copyn (VECSAV,TLVEC,3)
          call copyn (ROTBSK,ROTBAS,4)
          IFLNRS = 1
          call linfed (3,RETFED)
          kflg   = 5
          go to 8000
c
c...Tool was retracted and C-axis positioned
c...Plunge tool back down
c
      else if (kflg .eq. 5) then
          call copyn (STONUL,MCHNUM,12)
          call linfed (4,RETFED)
          IFLNRS = 1
          call linfed (2,RETFED)
          kflg   = 1
          go to 8000
      endif
c
c...Align logic is not active
c
      if (IACFLG(1) .eq. 1) go to 8000
c
c...If previous command was ROTABL/,NEXT
c...Then don't do anything
c
      if (IACNXT .eq. 1)  then
          IACHFL = 1
          go to 8000
      endif
c
c...Ignore FROM
c
      if (ISUBT .eq. 3 .and. FRMFLG .ne. 3) go to 8000
c
c...Look ahead only during Rapid moves
c
      if (IACFLG(1) .eq. 3 .and. IRAP .eq. 0) go to 8000
c
c...Make sure previous A-axis was at 0
c
      call tvprtb (ROTSTO,inc)
      if (inc .ne. ip1) go to 8000
c
c...Get current tool axis &
c...Make sure current A-axis is at 0
c
      abc(1) = ROTSTO(ip1,1)
      abc(2) = ROTSTO(ip2,1)
      abc1(1) = ROTSTO(ip1,1)
      abc1(2) = ROTSTO(ip2,1)
      call getang (TLVEC,abc,abc1,dvec,ierr)
      call copyn (ROTSTO(1,1),rang,20)
      rang(ip1) = abc(1)
      rang(ip2) = abc(2)
      call tvprtb (rang,inc)
c
c...A-axis is not at 0 degrees
c
      ifrc   = 0
      if (inc .ne. ip1) then
          if (IACFLG(3) .eq. 1) go to 8000
          ifrc   = 1
      endif
c
c...Find proper C-axis position based
c...on scanning clfile for limit violations
c
      if (IACFLG(2) .eq. 2) call aclmtc (abc)
c
c...Get next C-axis position
c
      call cpyrot (ROTSTO,rsto)
      call acposc (ifrc,abc,IACHFL)
c
c...If C-axis position changes and
c...Motion is held back, then
c...Force this motion now
c
      d1     = dabs(ROTSTO(ip1,2)-rsto(ip1,2))
      if (ihld .eq. 1 .and. d1 .gt. .0005) then
          call cpyrot (ROTSTO,rtmp)
          call copyn (MCHNUM(1,2),mtmp,3)
          call copyn (TLVEC,ttmp,3)
          HLDFLG = ihld
          call clrmot (0)
          call cpyrot (rtmp,ROTSTO)
          call copyn (mtmp,MCHNUM(1,2),3)
          call copyn (ttmp,TLVEC,3)
          ihld   = 0
      endif
c
c...Force C-axis in single block
c
      if (ifrc .eq. 1 .and. d1 .gt. .0005) then
          call copyn (MCHNUM(1,2),MCHNLK,3)
          call copyn (TLVEC,VECNLK,3)
          call copyn (VECSAV,TLVEC,3)
c
c......Rotate C-axis while tool is down
c
          if (IACFLG(4) .eq. 1) then
              call copyn (STONUM(1,2),MCHNUM(1,2),3)
              IFLNRS = 1
              if (IRAP .eq. 1) IFLNRS = 0
              call linfed (3,RETFED)
              kflg   = 1
c
c......Retract tool to rotate C-axis
c
          else if (IACFLG(4) .eq. 2) then
              call cpyrot (ROTSTO,ROTSLK)
              call cpyrot (rsto,ROTSTO)
              call copyn (STONUM,STONUL,12)
              call copyn (VECSAV,VECSAL,3)
              call copyn (ROTBAS,ROTBSK,4)
              call copyn (ROTBSV,ROTBAS,4)
              kflg   = 2
              call mwretr (2,kflg,ifl,itva)
              if (ifl .eq. 0) then
                  kflg = 0
              else
                  IFLNRS = 1
                  call linfed (1,RETFED)
              endif
          endif
      endif
c
c...End of routine
c
 8000 IACNXT = 0
c
c......Restore held back motion positions
c
      HLDFLG = ihld
      if (HLDFLG .eq. 1) then
          call cpyrot (hrot,ROTSTO)
          call copyn (hvec,VECSAV,3)
      endif
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: acposc (kfrc,gabc,kflg)
c
c     FUNCTION:  This calculates the next C-axis position and sets the
c                stored rotary axes to force this position during AC-
c                style rotary look ahead.
c
c     INPUT:   kfrc     I*4  D1  -  1 = The C-axis moves in this block
c                                       and the previous position was
c                                       at A=0.  Output C-axis in a
c                                       block by itself.
c
c                                   2 = Find the correct C-axis position
c                                       even though the current position
c                                       matches the required position of
c                                       the next move.  Used when looking
c                                       ahead for longest route errors.
c
c              gabc     R*8  D2  -  Current calculated rotary axis
c                                   positions.
c
c     OUTPUT:  kflg     I*4  D1  -  1 = C-axis requires positioning.
c
c***********************************************************************
c
      subroutine acposc (kfrc,gabc,kflg)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (NPT   ,KPOSMP(0059))
      equivalence (FRMFLG,KPOSMP(1211)), (IRTACT,KPOSMP(1256))
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (IRTINC,KPOSMP(1461)), (IACFLG,KPOSMP(1648))
c
      integer*4 ITYPE,NPT,IRTNXT(4),IRTNXF,ISUBT,FRMFLG,IRTACT(2),
     1          IACFLG(5),IRTINC(4)
c
      equivalence (CLPT  ,POSMAP(0491)), (TLVEC ,POSMAP(1369))
      equivalence (ROTBAS,POSMAP(1435)), (LIMITS,POSMAP(1254))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 TLVEC(3),CLPT(240),ROTANG(20,2),ROTBAS(4),ROTSTO(20,2),
     1       LIMITS(2,10)
c
      integer*4 kflg,kfrc
      logical ltlcmd
c
      real*8 gabc(2)
c
      integer*4 ierr,isw,iary(4),iout(10),iout1(10),nout,iary1(4),
     1          iaryx(4),ioutx(10),i,inc,ir1,ir2,ichg,ip1,ip2
c
      real*8 abc1(2),abc2(2),dlt(4),dvec(4),tvec(3),dlt1(4),
     1       axs(10),a1,a2,a3,a4,axsx(10),rout(10),rbas(4),rang(20)
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
cc	character*80 cbuf
c
c...Look for next record
c
      isw    = 0
      kflg   = 0
      ip1    = IRTINC(ir1)
      ip2    = IRTINC(ir2)
  100 if (kfrc .eq. 0) then
          call clsamp (isw)
          isw   = 1
c
c...Check for motion record
c
          if (ITYPE .eq. 14000) go to 7000
          if (ltlcmd()) go to 7000
          if (IACFLG(1) .eq. 3 .and. ITYPE .eq. 2000 .and. ISUBT .eq. 5)
     1        go to 7000
          if (ITYPE .ne. 5000) go to 100
          if (ISUBT .eq. 3 .and. FRMFLG .ne. 3) go to 100
      endif
c
c...Motion record found
c...Check for change in C-axis
c
      if (NPT .gt. 3 .and. kfrc .eq. 0) then
          tvec(1) = CLPT(4)
          tvec(2) = CLPT(5)
          tvec(3) = CLPT(6)
      else
          tvec(1) = TLVEC(1)
          tvec(2) = TLVEC(2)
          tvec(3) = TLVEC(3)
      endif
      abc1(1) = ROTSTO(ip1,1)
      abc1(2) = ROTSTO(ip2,1)
      abc2(1) = ROTSTO(ip1,1)
      abc2(2) = ROTSTO(ip2,1)
      call getang (tvec,abc2,abc1,dvec,ierr)
c
c...C-axis is going to change on the next move
c
      ichg   = 0
      if (kfrc .eq. 2) then
          ichg   = 1
      else if (gabc(1) .ne. abc2(1)) then
          ichg   = 1
      else
          call copyn (ROTSTO(1,1),rang,20)
          rang(ip1) = abc1(1)
          rang(ip2) = abc1(2)
          call tvprtb (rang,inc)
          if (inc .ne. ip1) ichg = 1
      endif
      if (ichg .eq. 1) then
          do 150 i=1,10,1
              axs(i) = 0.
              axsx(i) = 0.
  150     continue
c
c......If closes to a limit was specified then
c......Base angles should reflect limit angle
c
          if (IRTNXF .eq. 1 .and. IRTNXT(ir1) .ne. 0) then
              call copyn (ROTANG(1,2),rang,20)
              if (IRTNXT(ir1) .eq. 1) then
                  rang(ip1) = LIMITS(2,ir1+6)
              else
                  rang(ip1) = LIMITS(1,ir1+6)
              endif
              ROTSTO(ip1,2) = rang(ip1)
              call setbas (rang,rbas)
          else
              call copyn (ROTBAS,rbas,4)
          endif
c
c......Calculate first possible angle
c
          ROTANG(ip1,1) = abc2(1)
          iary(1) = 3
          iary(2) = 3
          iary(3) = 3
          iary(4) = 3
          call rotlin (ROTANG,rbas,0,iary,dlt)
          axs(ir1+6) = ROTANG(ip1,2)
          call axsxfm (axs,rout)
          call lmtchk (rout,nout,iout,0)
cc	call trmmsg (' ')
cc	write (cbuf,5) kposmp(1)
cc5	format ('ISN = ',i6)
cc	call trmmsg (cbuf)
cc	write (cbuf,10) axs(7),iout(7),iary(1),dlt(1)
cc10	format ('ang = ',f10.3,'  out = ',i1,'  dir = ',i1,
cc	1	'  dlt = ',f10.3)
cc	call trmmsg (cbuf)
c
c.........If out of limits calculate
c.........alternate angle
c
          if (iout(ir1+6) .ne. 0) then
              iary(ir1) = 3 - iary(ir1)
              call rotlin (ROTANG,rbas,0,iary,dlt)
              axs(ir1+6) = ROTANG(ip1,2)
              call axsxfm (axs,rout)
              call lmtchk (rout,nout,iout,0)
cc	write (cbuf,10) axs(7),iout(7),iary(1),dlt(1)
cc	call trmmsg (cbuf)
c
c.........User specified to go next to a limit
c
          else if (IRTNXF .eq. 1) then
              iaryx(ir1) = 3 - iary(ir1)
              call rotlin (ROTANG,rbas,0,iaryx,dlt)
              axsx(ir1+6) = ROTANG(ip1,2)
              call axsxfm (axsx,rout)
              call lmtchk (rout,nout,ioutx,0)
              if (ioutx(ir1+6) .eq. 0) then
                  if ((IRTNXT(ir1) .eq. 1 .and.
     1                dabs(axsx(ir1+6)-LIMITS(2,ir1+6)) .lt.
     2                dabs(axs(ir1+6)-LIMITS(2,ir1+6))) .or.
     3                (IRTNXT(ir1) .eq. 2 .and.
     4                dabs(axsx(ir1+6)-LIMITS(1,ir1+6)) .lt.
     5                dabs(axs(ir1+6)-LIMITS(1,ir1+6)))) then
                      iary(ir1) = iaryx(ir1)
                      axs(ir1+6) = axsx(ir1+6)
                  endif
              endif
          endif
c
c........Determine deltas based on
c........closest to user defined limit
c........or by default, the center of the limits
c
          if (IRTNXT(ir1) .eq. 1 .and. IRTNXF .eq. 1) then
              a1 = dabs(axs(ir1+6) - LIMITS(2,ir1+6))
              a2 = 0.
          else if (IRTNXT(ir1) .eq. 2 .and. IRTNXF .eq. 1) then
              a1 = dabs(axs(ir1+6) - LIMITS(1,ir1+6))
              a2 = 0.
          else
              a1 = dabs(axs(ir1+6) - LIMITS(1,ir1+6))
              a2 = dabs(axs(ir1+6) - LIMITS(2,ir1+6))
          endif
c
c......Calculate second possible angle
c
          ROTANG(ip1,1) = abc1(1)
          iary1(1) = 3
          iary1(2) = 3
          iary1(3) = 3
          iary1(4) = 3
          call rotlin (ROTANG,rbas,0,iary1,dlt1)
          axs(ir1+6) = ROTANG(ip1,2)
          call axsxfm (axs,rout)
          call lmtchk (rout,nout,iout1,0)
cc	write (cbuf,20) axs(7),iout1(7),iary1(1),dlt1(1)
cc20	format ('ang2 = ',f10.3,'  out = ',i1,'  dir = ',i1,
cc	1	'  dlt = ',f10.3)
cc	call trmmsg (cbuf)
c
c.........If out of limits calculate
c.........alternate angle
c
          if (iout1(ir1+6) .ne. 0) then
              iary1(ir1) = 3 - iary1(1)
              call rotlin (ROTANG,rbas,0,iary1,dlt1)
              axs(ir1+6) = ROTANG(ip1,2)
              call axsxfm (axs,rout)
              call lmtchk (rout,nout,iout1,0)
cc	write (cbuf,20) axs(7),iout1(7),iary1(1),dlt1(1)
cc	call trmmsg (cbuf)
c
c.........User specified to go next to a limit
c
          else if (IRTNXF .eq. 1) then
              iaryx(ir1) = 3 - iary1(ir1)
              call rotlin (ROTANG,rbas,0,iaryx,dlt1)
              axsx(ir1+6) = ROTANG(ip1,2)
              call axsxfm (axsx,rout)
              call lmtchk (rout,nout,ioutx,0)
              if (ioutx(ir1+6) .eq. 0) then
                  if ((IRTNXT(ir1) .eq. 1 .and.
     1                dabs(axsx(ir1+6)-LIMITS(2,ir1+6)) .lt.
     2                dabs(axs(ir1+6)-LIMITS(2,ir1+6))) .or.
     3                (IRTNXT(ir1) .eq. 2 .and.
     4                dabs(axsx(ir1+6)-LIMITS(1,ir1+6)) .lt.
     5                dabs(axs(ir1+6)-LIMITS(1,ir1+6)))) then
                      iary1(ir1) = iaryx(ir1)
                      axs(ir1+6) = axsx(ir1+6)
                  endif
              endif
          endif
c
c........Determine deltas based on
c........closest to user defined limit
c........or by default, the center of the limits
c
          if (IRTNXT(ir1) .eq. 1 .and. IRTNXF .eq. 1) then
              a3 = dabs(axs(ir1+6) - LIMITS(2,ir1+6))
              a4 = 0.
          else if (IRTNXT(ir1) .eq. 2 .and. IRTNXF .eq. 1) then
              a3 = dabs(axs(ir1+6) - LIMITS(1,ir1+6))
              a4 = 0.
          else
              a3 = dabs(axs(ir1+6) - LIMITS(1,ir1+6))
              a4 = dabs(axs(ir1+6) - LIMITS(2,ir1+6))
          endif
c
c......If both sets of rotaries are within limits
c......then select the one with the least calculated
c......delta values
c
          if (dabs(a1-a2) .lt. dabs(a3-a4)) then
              dlt(ir1)= 0.
              dlt1(ir1)= 1.
          else if (dabs(a3-a4) .lt. dabs(a1-a2)) then
              dlt(ir1) = 1.
              dlt1(ir1) = 0.
          endif
c
c.........Select first set of angles
c
          call copyn (rbas,ROTBAS,4)
          if ((dabs(dlt(ir1)) .lt. dabs(dlt1(ir1)) .and.
     1        iout(ir1+6) .eq. 0) .or. iout1(ir1+6) .ne. 0) then
              ROTANG(ip1,1) = abc2(1)
              call rotlin (ROTANG,ROTBAS,1,iary,dlt)
cc	write (cbuf,10) ROTANG(1,2),iout(7),iary(1),dlt(1)
cc	call trmmsg (cbuf)
c
c.........Select second set of angles
c
          else
              ROTANG(ip1,1) = abc1(1)
              call rotlin (ROTANG,ROTBAS,1,iary1,dlt1)
cc	write (cbuf,20) ROTANG(1,2),iout1(7),iary1(1),dlt1(1)
cc	call trmmsg (cbuf)
          endif
          ROTSTO(ip1,1) = ROTANG(ip1,1)
          ROTSTO(ip1,2) = ROTANG(ip1,2)
          kflg   = 1
c
c...C-axis does not change and
c...align on rapid moves is active
c...get next record
c
      else if (IACFLG(1) .eq. 3 .and. kfrc .eq. 0) then
          if (gabc(2) .eq. abc2(2)) go to 100
      endif
c
c...Reset clfile scan
c
 7000 if (kfrc .eq. 0) call clsamp (-1)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: aclmtc ()
c
c     FUNCTION:  This routine scans the clfile to determine if a limit will
c                be reached based on the current C-axis value.  If so,
c                then the C-axis will be started furthest from that limit.
c
c                Each of the possible starting positions (neg limit, center
c                of limit, and pos limit) will be checked.
c
c                The C-axis will be forced to the recommended position
c                based on the results of this scan.
c
c     INPUT:   gabc     R*8  D2  -  Current calculated rotary axis
c                                   positions.
c
c     OUTPUT:  none.
c
c***********************************************************************
c
      subroutine aclmtc (gabc)
c
      include 'post.inc'
c
      equivalence (ITYPE ,KPOSMP(0003)), (ISUBT ,KPOSMP(0004))
      equivalence (MXCL  ,KPOSMP(0005)), (NPT   ,KPOSMP(0059))
      equivalence (FRMFLG,KPOSMP(1211)), (IRTACT,KPOSMP(1256))
      equivalence (IRTNXT,KPOSMP(1361)), (IRTNXF,KPOSMP(1365))
      equivalence (IRTINC,KPOSMP(1461)), (IACFLG,KPOSMP(1648))
c
      integer*4 ITYPE,NPT,IRTNXT(4),IRTNXF,ISUBT,FRMFLG,IRTACT(2),
     1          MXCL,IACFLG(5),IRTINC(4)
c
      equivalence (CLPT  ,POSMAP(0491)), (TLVEC ,POSMAP(1369))
      equivalence (VECSAV,POSMAP(1372))
      equivalence (ROTBAS,POSMAP(1435)), (ROTBSV,POSMAP(2275))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
c
      real*8 TLVEC(3),CLPT(240),ROTANG(20,2),ROTBAS(4),ROTSTO(20,2),
     1       ROTBSV(4),VECSAV(3)
c
      real*8 gabc(2)
c
      integer*4 ir1,ir2,i,irnf,irnx(4),inxpt,imax,inx(3),iflg,isw,inc,
     1          ierr,imaxpt,ipt,idid,irp,ip1
      logical ltlcmd
c
      real*8 rsto(20,2),rang(20,2),rbas(4),rbsv(4),tvec(3),vsav(3),
     1       mnum(3,4),laxs(6),axs(10),rrot(20,2),rbasv(4),vsto(3)
c
      equivalence (ir1,IRTACT(1)), (ir2,IRTACT(2))
c
c...Save global settings
c
      ip1    = IRTINC(ir1)
      call cpyrot (ROTSTO,rsto)
      call cpyrot (ROTANG,rang)
      call copyn (ROTBSV,rbsv,4)
      call copyn (ROTBAS,rbasv,4)
      irnf   = IRTNXF
      call copynk (IRTNXT,irnx,4)
c
c...Initialize C-axis direction selection
c
      inxpt  = 1
      imax   = 0
      if (IRTNXF .eq. 1 .and. IRTNXT(ir1) .ne. 0) then
          inx(1) = IRTNXT(ir1)
          if (inx(1) .eq. 1) then
              inx(2) = 2
              inx(3) = 3
          else
              inx(2) = 1
              inx(3) = 3
          endif
      else
          inx(1) = 3
          inx(2) = 1
          inx(3) = 2
      endif
c
c...Initialize clfile scan
c
      call copyn (TLVEC,vsav,3)
      call copyn (VECSAV,vsto,3)
   50 call copyn (vsav,tvec,3)
      call copyn (vsto,VECSAV,3)
c
c...Determine initial position for C-axis
c
      call copyn (vsav,TLVEC,3)
      call acposc (0,gabc,iflg)
      call copyn (ROTBAS,rbas,4)
cc      if (iflg .eq. 0) go to 8000
c
c...Scan clfile
c
      isw    = 0
      inc    = 0
      idid   = 0
      irp    = 0
  100 call clsamp (isw)
      isw   = 1
c
c...Check for motion record
c
      if (ITYPE .eq. 14000) go to 6000
      if (ITYPE .eq. 2000) then
          irp    = 0
          if (ISUBT .eq. 5) irp = 1
          if (ltlcmd()) go to 6000
      endif
      if (ITYPE .ne. 5000) go to 100
      if (ISUBT .eq. 3 .and. FRMFLG .ne. 3) go to 100
c
c...Motion record found
c
      inc    = inc    + 1
      do 200 i=1,MXCL*NPT,NPT
          if (NPT .gt. 3) then
              tvec(1) = CLPT(i+3)
              tvec(2) = CLPT(i+4)
              tvec(3) = CLPT(i+5)
          endif
c
c......Calculate machine axes
c
          call copyn (CLPT(i),mnum(1,2),3)
          call tlaxis (mnum,tvec,laxs,axs,rrot,rbas,1,0,ierr)
          call cpyrot (rrot,ROTSTO)
c
c......Tool takes longest route
c......Reset scan and try again from
c......different starting position
c
          if (ierr .eq. 3) then
              if (inc .gt. imax) then
                  imax   = inc
                  imaxpt = inxpt
              endif
              if (inxpt .eq. 3) then
                  inxpt = imaxpt
                  go to 6000
              else
                  inxpt  = inxpt  + 1
                  if (inx(inxpt) .eq. 3) then
                      IRTNXF = 0
                      IRTNXT(ir1) = 0
                  else
                      IRTNXF = 1
                      IRTNXT(ir1) = inx(inxpt)
                  endif
                  call cpyrot (rsto,ROTSTO)
                  call copyn (ROTBAS,rbas,4)
                  call clsamp (-1)
                  go to 50
              endif
          endif
c
c......A-axis is at 0
c......Let's use this initial C-axis position
c
          call tvprtb (rrot,ipt)
          if (ipt .eq. ip1) then
              if (IACFLG(1) .ne. 3 .or. idid .eq. 1 .or. irp .eq. 1)
     1            go to 6000
          else
              idid   = 1
          endif
          call copyn (tvec,VECSAV,3)
  200 continue
c
c...Get next clfile record
c
      irp    = 0
      go to 100
c
c...Found best solution
c...Setup C-axis recommended solution
c
 6000 if (inx(inxpt) .eq. 3) then
          irnf   = 0
      else
          irnf   = 1
          irnx(ir1) = inx(inxpt)
      endif
c
c...Reset clfile scan and
c...restore global variables
c
 7000 call clsamp (-1)
      call cpyrot (rsto,ROTSTO)
      call cpyrot (rang,ROTANG)
      call copyn (rbasv,ROTBAS,4)
      call copyn (rbsv,ROTBSV,4)
      call copyn (vsav,TLVEC,3)
      call copyn (vsto,VECSAV,3)
      IRTNXF = irnf
      call copynk (irnx,IRTNXT,4)
c
c...End of routine
c
 8000 return
      end
