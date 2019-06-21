c
c***********************************************************************
c
c     FILE NAME: rtmset
c
c     CONTAINS:  rtmlin  rtmset  tbmang  gttrot  crad    intrpc  intrpt
c                rtmlim  rtprep  tbplim  tblmio  gtptrc  fixang  movang
c                fixaxs
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        rtmset.f , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        10/29/13 , 16:02:08
c
c***********************************************************************
c
c***********************************************************************
c
c     SUBROUTINE: rtmlin (kflg,kerr,kfln)
c
c     FUNCTION:  This routine generates points for polar interpolation
c                on rotary table within tolerance specified by RTOLER.
c                Points genrated by routine are pushed on stack to be
c                available by motion routine.  The first call creates
c                2 points, the last call only retrieves last point from
c                stack.
c
c     INPUT:   kflg    I*4  D1  -  input flag: 0 - initial call,
c                                  n <> 0 - following calls.
c
c              kerr    I*4  D1  -  tlaxis call error number.
c
c              kfln    I*4  D1  -  saved status after previous call of
c                                  this routine.
c
c     OUTPUT:  kflg    I*4  D1  -  output status:  n = 1 - in lineariza-
c                                  tion procedure, n = -1 post generated
c                                  motion completed, need one more call
c                                  to get last point, n = 2 - retract
c                                  procedure active.
c
c              kfln    I*4  D1  -  status flag: 1 - linearization without
c                                  midle point, -2 - moving to the midle
c                                  point, 0 - last point is on stack.
c
c***********************************************************************
c
      subroutine rtmlin (kflg,kerr,kfln)
c
      include 'post.inc'
      include 'lintol.inc'
c
      integer*4 kerr,kflg,kfln
c
      equivalence (IRTNUM,KPOSMP(1243))
      equivalence (LRTRCT,KPOSMP(1278)), (NROT  ,KPOSMP(1366))
      equivalence (IRAP  ,KPOSMP(3199))
      equivalence (IFLNRS,KPOSMP(1731)), (MDTLIN,KPOSMP(4032))
      equivalence (HLDFLG,KPOSMP(1622))
c
      integer*4 LRTRCT,NROT,IRAP,HLDFLG,IFLNRS,MDTLIN,IRTNUM
c
      equivalence (MCHNUM,POSMAP(1287)), (LINAXS,POSMAP(1299))
      equivalence (AXSOUT,POSMAP(1340)), (TLVEC ,POSMAP(1369))
      equivalence (AXSSTO,POSMAP(1425)), (ROTBAS,POSMAP(1435))
      equivalence (RETFED,POSMAP(2254)), (PTPOST,POSMAP(2472))
      equivalence (TRPOST,POSMAP(4419)), (PTMIDL,POSMAP(4415))
      equivalence (AXSSAV,POSMAP(4423))
      equivalence (ROTANG,POSMAP(5173)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5253))
c
      real*8 MCHNUM(3,4),TRPOST(3),PTMIDL(3),AXSSAV(10),
     -       TLVEC(3),AXSOUT(10),PTPOST(3),LINAXS(6),
     -       HLDROT(20,2),ROTANG(20,2),ROTBAS(4),AXSSTO(10),
     -       ROTSTO(20,2),RETFED(4)
c
      integer*4 ier,iary(10),lmdtl,icnt,ifir,irro,ierr,istp,
     -          naxs(10),nout
c
      real*8 mlax(6),mxout(10),rots(20,2),gmch(3,4)
c
c...Set flags
c
      ifir   = 0
      irro   = 1
      if (kflg .eq. 0) then
         if (IRAP .gt. 0 .or. IRTNUM .eq. 0) go to 8000
         lmdtl  = MDTLIN
         ifir   = 1
         IFLNRS = 0
      end if
c
c...Save ROTSTO for motion
c
      if (HLDFLG .eq. 1) then
          call cpyrot (HLDROT,rots)
      else
          call cpyrot (ROTSTO,rots)
      end if
c
c...Check for table over limit error
c
      if (kerr .eq. 3) then
          call gtlpos
          call copyn (AXSSTO,AXSSAV,10)
          call rtmlim (ifir,istp)
          kerr   = 0
          lmdtl  = 11
          kflg   = 2
          go to 5000
      end if
c
      if (kflg .eq. 0) go to 200
      lmdtl  = kfln
      if (lmdtl .gt. 10) go to 5000
      if (kflg .eq. 1) go to 800
c
c...Get last point (kflg = -1)
c
      call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
      call copyn (ROTBAL,ROTBAS,4)
      if (kflg .eq. -2) then
         if (IFLNRS .eq. 1) call raprst
         if (IFLNRS .eq. 2) call linfed (5,RETFED)
      end if
      kflg   = 0
      MDTLIN = 0
      go to 7000
c
c...Initial call, get first point
c
  200 call gtlpos
      call copyn (AXSSTO,AXSSAV,10)
      if (lmdtl .gt. 0) call copyn (MCHNUL(1,2),PTNXLN(1,2),3)
      if (lmdtl .eq. 2) lmdtl = -2
      kflg   = 1
      go to 1000
c
c...Get next point and set table position
c
  800 if (lmdtl .eq. 1) then
         call tbmang (PTNXLN,AXSSAV,ROTSTO,ier,1,0)
         if (ier .eq. 0) kflg = -1
      else if (lmdtl .lt. 0) then
         call copyn (PTMIDL,gmch(1,2),3)
         call tbmang (gmch,AXSSAV,ROTSTO,ier,1,0)
         if (ier .eq. 0) lmdtl = 1
      end if
c
 1000 call copyn (TRPOST,MCHNUL(1,2),3)
c
c...Set machine point, adjust for table rotation
c
 2000 MDTLIN = lmdtl
      call cpyrot (ROTSTL,ROTSTO)
      call tlaxis (MCHNUL,TLVECL,mlax,mxout,ROTANL,ROTBAL,1,1,ierr)
c
c...Check if any limit error
c...Do not process second limit error!
c
      call lmtchk (mxout,nout,naxs,0)
      if ((ierr .eq. 3 .or. nout .ne. 0) .and. lmdtl .lt. 10) then
          call rtmlim (1,istp)
          kerr   = 0
          lmdtl  = 11
          kflg   = 3
          go to 5000
      end if
      call copyn (mxout,AXSSAV,10)
c
c...Push/pop axes on stack
c
      if (ifir .eq. 1) then
         call pshaxs (mxout,TLVECL)
         call savpos
         ifir  = 0
         if (lmdtl .gt. 10) go to 5000
         go to 800
      else
         call popaxs (MCHNUM,LINAXS,ROTANG,AXSOUT,TLVEC,iary,icnt)
         call pshaxs (mxout,TLVECL)
c
c...Set proper feed rate
c
         if (istp .eq. 4 .and. LRTRCT .ne. 0) then
             IFLNRS = 1
             call linfed (1,RETFED)
         end if
         if (istp .eq. 5 .and. LRTRCT .ne. 0) call linfed (3,RETFED)
         if (istp .eq. 6) then
             call raprst
             call linfed (2,RETFED)
         end if
         if (kflg .lt. 0) then
             lmdtl = 0
         else
             call savpos
         end if
      end if
      kfln   = lmdtl
      go to 7000
c
c...Retract procedure
c
 5000 istp   = lmdtl - 10
      call gtptrc (MCHNUL,istp)
      lmdtl  = 10 + istp
      if (istp .eq. 6) then
         if (kflg .eq. 2) then
             kflg  = -2
         else
             kflg  = 1
             lmdtl = 1
         end if
      end if
      go to 2000
c
c...End of routine,
c...restore ROTSTO
c
 7000 call cpyrot (rots,ROTSTO)
 8000 return
      end
c
c***********************************************************************
c
c     SUBROUTINE: rtmset (krt,gabc1,gabc2)
c
c     FUNCTION:  This routine returns table angles used in tlaxis routine
c                instead of call getang (used when tool vector change
c                generates rotary angles).  'rtmset' is used when tool
c                vector is PERPTO table plane and radial motion is re-
c                quired with simultaneuos table rotation instead of
c                linear interpolation.
c
c     INPUT:   krt      I*4  D1  -  Table axis number.
c
c              gabc1    R*8  D4  -  The first set of rotary axes
c                                   positions (rotary scale).
c
c              gabc2    R*8  D4  -  The second set of rotary axes
c                                   positions (rotary scale).
c
c***********************************************************************
c
      subroutine rtmset (krt,gabc1,gabc2)
c
      include 'post.inc'
c
      integer*4 krt
      real*8 gabc1(*),gabc2(*)
c
      equivalence (MDTLIN,KPOSMP(4032))
c
      integer*4 MDTLIN
c
      equivalence (TBRMOD,POSMAP(4413))
c
      real*8 TBRMOD(2)
c
      gabc1(krt) = TBRMOD(1)
      gabc2(krt) = TBRMOD(2)
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: tbmang (gmch,gxsav,kerr,kfl)
c
c     FUNCTION:  This routine calculates relative table position used
c                when polar interpolation is required and possible on
c                this table (tool vector is PERPTO table plane).
c
c     INPUT:   gmch     R*8  D3.4 -  Destination Point coordinates
c                                    (general).
c
c              gxsav    R*8  D10  -  Last point machine axes (used when
c                                    kfl = 1, last position is not in
c                                    the AXSSTO array).
c
c              kfl      I*4  D1   -  Entry flag: 0 - call from 'tlaxis',
c                                    1 - call from 'rtmlin'.
c
c     OUTPUT:  kerr     I*4  D1   -  Status: 0 - destination point is
c                                    output, 1 - linearized point is ge-
c                                    rated,  2 - same as (1) + midle
c                                    point (closest to table center).
c
c***********************************************************************
c
      subroutine tbmang (gmch,gxsav,grot,kerr,kfl,kfm)
c
      include 'post.inc'
      include 'lintol.inc'
c
      integer*4 kerr,kfl,kfm
      real*8 gmch(3,4),gxsav(10),grot(20,2)
c
      equivalence (MOTREG,KPOSMP(0381))
      equivalence (HLDFLG,KPOSMP(1622)), (MODROT,KPOSMP(4031))
c
      integer*4 MODROT,HLDFLG,MOTREG(24)
c
      equivalence (STONUM,POSMAP(1387)), (AXSSTO,POSMAP(1425))
      equivalence (HLDMCH,POSMAP(2141)), (HLDAXS,POSMAP(2167))
      equivalence (MATRXT,POSMAP(2482)), (TBRMOD,POSMAP(4413))
      equivalence (TBRMID,POSMAP(4418)), (TRPOST,POSMAP(4419))
      equivalence (PTMIDL,POSMAP(4415)), (ROTSTO,POSMAP(5213))
      equivalence (HLDROT,POSMAP(5213)), (TABORG,POSMAP(5374))
c
      real*8 TBRMOD(2),MATRXT(4,3),AXSSTO(10),TABORG(3,20),HLDAXS(10),
     -       ROTSTO(20,2),TBRMID,STONUM(3,4),TRPOST(3),PTMIDL(3),
     -       HLDMCH(3,4),HLDROT(20,2)
c
      integer*4 ier,iax(4)
c
      real*8 rmx(3),smx(3),raxs(10),rlin(6),ptj(3),ang,rsto(20),
     -       org(3),rat,rold,mold(3),rrot(20,2),tvec(3),rmch(3,4)
c
      data iax /13,16,19,22/
c
      kerr   = 0
c
c...Adjust cl point to get machine point,
c...use machine points to get points in table plane
c
      if (kfl .eq. 0) then
         if (HLDFLG .eq. 1) then
            if (kfm .eq. 0) then
               call alladj (gmch,rlin,raxs,HLDROT,2,5)
            end if
            call copyn (HLDAXS,raxs,10)
            rold   = HLDROT(MODROT,1)
            call copyn (HLDMCH(1,2),mold,3)
            call copyn (HLDROT,rsto,20)
         else
            if (kfm .eq. 0) then
               call alladj (gmch,rlin,raxs,ROTSTO,2,5)
            end if
            call copyn (AXSSTO,raxs,10)
            rold   = ROTSTO(MODROT,1)
            call copyn (STONUM(1,2),mold,3)
            call copyn (ROTSTO,rsto,20)
         end if
      else
         if (kfm .eq. 0) then
            call alladj (gmch,rlin,raxs,ROTSTL,2,5)
         end if
         call copyn (gxsav,raxs,10)
         rold   = ROTSTL(MODROT,1)
         call copyn (STONUL(1,2),mold,3)
         call copyn (ROTSTL,rsto,20)
      end if
      call alladr (raxs,rlin,rmch,rrot,tvec,5,3)
      smx(1) = rmch(1,3)
      smx(2) = rmch(2,3)
      smx(3) = rmch(3,3)
      if (kfm .eq. 1) call alladj (gmch,rlin,raxs,grot,2,5)
      rmx(1) = gmch(1,3)
      rmx(2) = gmch(2,3)
      rmx(3) = gmch(3,3)
c
c...Transform points to table plane
c
      call ptmatr (rmx,ptj,MATRXT,1)
      call ptmatr (smx,smx,MATRXT,1)
c
c...Modify table origin for when
c...other rotary axes have moved to
c...get tool axis perpto table
c...FSR 61173
c
cc      org(1) = 0.
cc      org(2) = 0.
cc      org(3) = 0.
      call ptrtad (TABORG(1,MODROT),org,rsto,0)
      call ptmatr (org,org,MATRXT,1)
c
c...Get table rotation
c
      call gttrot (smx,ptj,org,rat,ang,rold,ier)
      call movang (rold,ang,TBRMOD(1))
c
c...Following call no longer needed
c...after above modification to table origin
c...FSR 61357 (second test case)
c...requires this fix when LINTOL/ON is in effect
c...It does not hurt anything being here
c...So will leave it in for now
c
      if (kfl .eq. 0) call fixang (gmch(1,2),rmch(1,3),TBRMOD(1),rsto)
c
c...Work with output numbers
c...to diminish accumulation error
c...FSR 61173
c...Removed for FSR 61357
c...Does not affect 61173, the good fix is for
c...the table origin (above)
c
cc      call codint (MOTREG(iax(MODROT)),TBRMOD(1),TBRMOD(1),inum)
      TBRMOD(2) = TBRMOD(1)
c
c...If linearization required convert point
c...back to part system
c
      if (ier .ne. 0) then
          call intrpt (mold,gmch(1,2),TRPOST,rat)
          if (ier .eq. 2) then
              call intrpt (mold,gmch(1,2),PTMIDL,TBRMID)
          end if
          kerr   = ier
      else
c
c...Use destination point (LINTOL/OFF or in tolerance)
c
          call copyn (gmch(1,2),TRPOST,3)
      end if
c
      return
      end
c
c***********************************************************************
c
c     SUBROUTINE: gttrot (gspt,gptj,gorg,grat,gang,gsrt,kerr)
c
c     FUNCTION:  This routine makes the actual calculations of the
c                table position for polar interpolation and if linea-
c                rization is on, calculates a point in the required
c                chordal tolerance.
c
c     INPUT:   gspt     R*8  D3  - 'From' point in table coord. system.
c
c              gptj     R*8  D3  - 'To' point in table coord. system.
c
c              gorg     R*8  D3  - Table center point in table coord. system.
c
c              gsrt     R*8  D1  - Table position at 'From' point.
c
c     OUTPUT:  grat     R*8  D1  - Relative distance of calculated point
c                                  from 'gspt' (=1 if 'gptj' is output).
c
c              gang     R*8  D1  - Table rotation from current position
c                                  to move to the point defined by 'grat'.
c
c              kerr     I*4  D1  - Status: 0 - linearization not required
c                                  destination point is output,
c                                  1 - linearized point is gerated,
c                                  2 - same as (1) + midle point (closest
c                                  to table center) is saved in common
c                                  arrray TBRMID.
c
c***********************************************************************
c
      subroutine gttrot (gspt,gptj,gorg,grat,gang,gsrt,kerr)
c
      include 'post.inc'
c
      equivalence (IRAP,  KPOSMP(3199))
c
      integer*4 IRAP
c
      integer*4 kerr
      real*8 gspt(3),gptj(3),gorg(3),grat,gsrt,gang
c
c
      equivalence (RAD   ,POSMAP(0002)), (RTOLER,POSMAP(2400))
      equivalence (TBRMID,POSMAP(4418)), (TBMIDL,POSMAP(4422))
      equivalence (POLARS,POSMAP(4435)), (POLARL,POSMAP(4438))
      equivalence (FUZZ4 ,POSMAP(4912))
cc	equivalence (SAVV1 ,POSMAP(30))
cc	real*8 SAVV1(3)
c
      real*8 RAD,TBMIDL,RTOLER,TBRMID,POLARS(3),POLARL(3),FUZZ4
c
      integer*4 i,mm,ier,ifl,frst
      real*8 r,r0,r1,r2,p,c,v1(3),v2(3),ap,a1,a2,fi,pt0(3),
     -       pt1(3),pt2(3),plin(7),ndist,cr1,cr2,td,f,rr(2),
     -       dr1,dr2,ps(3),vs(3),a3,a,spn,tol,t1,t2,s
c
      equivalence (r1,rr(1)), (r2,rr(2))
c
      kerr  = 0
      gang  = 0.d0
      ifl   = 0
      frst  = 0
c
c...Discard Z coordinate on table
c
      do 110 i=1,2
         pt1(i) = gspt(i)
         pt2(i) = gptj(i)
         pt0(i) = gorg(i)
         plin(i) = pt1(i)
         v1(i) = pt1(i) - pt0(i)
  110 continue
      grat  = 1.d0
      td    = ndist(gptj,gspt)
      pt0(3) = 0.d0
      pt1(3) = 0.d0
      pt2(3) = 0.d0
      plin(3) = 0.d0
      plin(6) = 0.d0
      v1(3) = 0.d0
      v2(3) = 0.d0
c
c...Table doesn't rotate if motion is from/to table origin
c...or move is not significant
c
      r      = ndist(pt1,pt2)
      if (r .lt. 1.d-4) go to 8000
      spn    = r
      r1     = ndist(pt1,pt0)
      dr1    = r1
c
c...Get radiuses/vectors for starting & end point on table
c...Start of iteration
c
  200 r2     = ndist(pt2,pt0)
      dr2    = r2
      v2(1) = pt2(1) - pt0(1)
      v2(2) = pt2(2) - pt0(2)
      if (r2 .lt. 1.d-5) then
         v2(1) = v1(1)
         v2(2) = v1(2)
         dr2   = r1
      else if (r1 .lt. 1.d-5) then
         v1(1) = 1.d0 * dcos(gsrt/RAD)
         v1(2) = 1.d0 * dsin(gsrt/RAD)
         dr1   = r2
      end if
c
c...Get motion line on table
c
      if (frst .eq. 0) then
          plin(4) = (pt2(1) - pt1(1)) / r
          plin(5) = (pt2(2) - pt1(2)) / r
          v1(1) = v1(1) / dr1
          v1(2) = v1(2) / dr1
          frst  = 1
      else
          r     = ndist(pt1,pt2)
      end if
      v2(1) = v2(1) / dr2
      v2(2) = v2(2) / dr2
      mm    = 1
      if (r2 .gt. r1) mm = 2
c
c...Get table rotation from current position
c
cc	call betvec (v1,SAVV1,f)
cc	if (f .lt. .0005) call copyn (SAVV1,v1,3)
      call vecang (v1,3,a1)
      call vecang (v2,3,a2)
      call betvec (v1,v2,f)
      fi    = f
      c     = a1 + fi
      if (a2 .lt. FUZZ4 .and. c .gt. 360.d0-FUZZ4 .and. c .lt. 360.d0)
     1        c = 360.d0
      if (c .ge. 360.d0) c = c - 360.d0
cc      if (c .gt. 360.d0-FUZZ4) c = c - 360.d0
      if (dabs(c-a2) .gt. FUZZ4) fi = -fi
      gang  = fi
cc	call copyn (v2,SAVV1,3)
c
c...If linearization not used this is table delta rotation
c...if passing table center do not need linearize the move
c
      if (f .lt. FUZZ4 .or. 180.d0-f .lt. FUZZ4) then
          gang = 0.
          go to 8000
      end if
      if (RTOLER .eq. 0.d0) go to 8000
      a2    = a1 + fi
c
c...If passing midlepoint, get & save this position
c...Get motion line distance from table origin
c
      call ptlnds (pt0,plin,r0)
      if (r0 .gt. 1.d-5 .and. ifl .eq. 0) then
          plin(7) = plin(4)*pt0(1)+plin(5)*pt0(2)
          call plnint (pt1,plin(4),plin(4),ps,ier)
          vs(1) = (ps(1) - pt0(1))/r0
          vs(2) = (ps(2) - pt0(2))/r0
          vs(3) = 0.0
          call vecang (vs,3,a3)
          if ((a2 - a3)*(a3 - a1) .gt. 1.d-6) then
              call betvec (v1,vs,c)
              p    = ndist (pt1,ps)
              if (a3 .gt. 180.d0) a3 = a3 - 360.0d0
              call movang (gsrt,a3,TBMIDL)
              TBRMID = p / spn
              pt2(1) = pt1(1) + plin(4) * p
              pt2(2) = pt1(2) + plin(5) * p
              ifl    = 1
              go to 200
          end if
c
c...If the input points (starting, ending, and origin) are colinear,
c...get out of function.  The ifl flag = 0 on the first time thru.
c...The r0 distance is the distance from the origin to the line
c...connecting pt1 (start point) and pt2 (end point) (actually, plin
c...is a point-vector, based at point pt1 and in the direction of pt2).
c...The distance r0 = 0 when the origin is on the same line as
c...both pt1 and pt2.  They are all colinear.  No rotation needed.
c
      else if (r0 .le. 1.d-5 .and. ifl .eq. 0) then
          kerr = 0
          grat = 0.0
          goto 8000
      end if
      ap     = (r1-r2) / (fi/RAD)
c
c...If RAPID is in effect, don't bother to see if in tolerance.
c
      if (IRAP .ne. 0) then
          kerr = 0
          goto 8000
      endif
c
c...Get chordal error for this motion (Archimedes' spiral)
c
      fi    = -fi
      call crad (ap,fi,r1,cr1,cr2)
      t1    = .5*(cr1 + cr2)
      t2    = t1
      if (cr1 .gt. .5*r) t1 = cr1 - dsqrt(cr1*cr1-.25*r*r)
      if (cr2 .gt. .5*r) t2 = cr2 - dsqrt(cr2*cr2-.25*r*r)
      tol   = t1
      if (t2 .gt. tol) tol = t2
c
c...If not in tolerance decrease distance using factor
c...depending on speed of spiral etc.
c
      if (tol .gt. RTOLER) then
          if (dabs(ap) .gt. 6.*r1) then
              a = .5*rr(3-mm)/rr(mm)
              if (r1 .eq. rr(mm)) a = 1.0 - .5 * rr(3-mm)/rr(mm)
              s = r * a
          else if (dabs(ap) .lt. .3*r1) then
              s = 1.8 * dsqrt(RTOLER * (2.*cr1 - RTOLER))
          else
              a = .8
              if (rr(mm) .gt. 20.*rr(3-mm)) then
                 a = rr(3-mm)/rr(mm)
                 if (r1 .eq. rr(mm)) a = 1.0 - rr(3-mm)/rr(mm)
              else
                 if (tol .gt. 10.*RTOLER) a = (RTOLER/tol)**.33
              end if
              s = r * a
          endif
c
c...Adjust end point & iterate,
c...must be a < 1. !!! otherwise it is loop problem
c
          pt2(1) = pt1(1) + s * plin(4)
          pt2(2) = pt1(2) + s * plin(5)
          r      = ndist(pt1,pt2)
          kerr   = 1
          go to 200
      else
          grat  = r / spn
          if (ifl .eq. 1) kerr = 2
      end if
c
c...Set polar coordinates of points for other use
c
 8000 POLARS(1) = r1
      POLARS(2) = gspt(3)
      POLARS(3) = a1
      POLARL(1) = r2
      POLARL(2) = gspt(3) + grat*(gptj(3) - gspt(3))
      POLARL(3) = a2
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: crad (gapr,gdfi,gr1,gcr1,gcr2)
c
c     FUNCTION:  This routine calculates the curvature radii at
c                the start & end point of Archimedes spiral segment.
c
c     INPUT:   gapr     R*8  D1  - Spiral speed (in 1/RAD).
c
c              gdfi     R*8  D1  - Segment angular span in deg.
c
c              gr1      R*8  D1  - Radius of the starting point of
c                                  spiral segment.
c
c     OUTPUT:  gcr1     R*8  D1  - Curvature radius at the starting
c                                  point of spiral segment.
c
c              gcr2     R*8  D1  - Curvature radius at the end
c                                  point of spiral segment.
c******************************************************************
c
      subroutine crad (gapr,gdfi,gr1,gcr1,gcr2)
c
      include 'post.inc'
c
      real*8 gapr,gdfi,gr1,gcr1,gcr2
c
      equivalence (RAD   ,POSMAP(0002))
c
      real*8 RAD
c
      real*8 a,f,f1,f2
c
      a      = dabs(gapr)
      if (a .lt. 1.d-5) go to 1000
c
c...Regular Archimedean Spiral
c......get starting curvature radius
c
      f      = gdfi / RAD
      f1     = gr1 / gapr
      f2     = f1 * f1
      gcr1   = a * ((f2 + 1.d0)**1.5) / (f2 + 2.d0)
c
c......get ending curvature radius
c
      f1     = f1 - f
      f2     = f1 * f1
      gcr2   = a * ((f2 + 1.d0)**1.5) / (f2 + 2.d0)
      go to 8000
c
c...Circle almost (small speed)
c
 1000 gcr1   = gr1
      gcr2   = gr1
c
 8000 return
      end
c
c******************************************************************
c
c     SUBROUTINE: intrpc (gpt1,gpt2,gpto,grat)
c
c     FUNCTION:  This routine interpolates point at given ratio
c                between two points in cylindr. coordinates assuming
c                spiral interpolation.
c
c     INPUT:   gpt1     R*8  D3  - Point 1 in cyl. coordinates:
c                                  (1) = radius, (2) = Z-axis coord.
c                                  (3) = fi angle of point.
c
c              gpt2     R*8  R3  - Point 2.
c
c              grat     R*8  D1  - Ratio factor.
c
c     OUTPUT:  gpto     R*8  D3  - Interpolated point coordinates
c                                  (cartesian).
c
c******************************************************************
c
      subroutine intrpc (gpt1,gpt2,gorg,gpto,grat)
c
      include 'post.inc'
c
      real*8 gpt1(3),gpt2(3),gpto(3),gorg(3),grat
c
      equivalence (RAD   ,POSMAP(0002)), (MATRXT,POSMAP(2482))
c
      real*8 RAD,MATRXT(3,4)
c
      real*8 an,rr,pp(3)
c
c...Interpolate radius only, use starting angle
c...since alladr will rotate point to intermediate position
c
      rr     = gpt1(1) + grat * (gpt2(1) - gpt1(1))
      an     = gpt1(3) / RAD
c
c...Convert point from cylindrical to cartesian system
c...interpolating Z axis.
c
      pp(1) = gorg(1) + rr * dcos(an)
      pp(2) = gorg(2) + rr * dsin(an)
      pp(3) = gpt1(2) + grat * (gpt2(2) - gpt1(2))
c
c...Translate from table to machine coordinate system
c
      call ptmatr (pp,gpto,MATRXT,1)
c
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: intrpt (gpt1,gpt2,gpto,grat)
c
c     FUNCTION:  This routine interpolates point at given ratio
c                between two points.
c
c     INPUT:   gpt1     R*8  D3  - First point.
c
c              gpt2     R*8  R3  - Second point.
c
c              grat     R*8  D1  - Ratio factor.
c
c     OUTPUT:  gpto     R*8  D3  - Output point coordinates.
c
c******************************************************************
c
      subroutine intrpt (gpt1,gpt2,gpto,grat)
c
      real*8 gpt1(3),gpt2(3),gpto(3),grat
c
      real*8 rr
c
      rr     = grat
      gpto(1) = gpt1(1) + rr * (gpt2(1) - gpt1(1))
      gpto(2) = gpt1(2) + rr * (gpt2(2) - gpt1(2))
      gpto(3) = gpt1(3) + rr * (gpt2(3) - gpt1(3))
c
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: rtmlim (kinp,kout)
c
c     FUNCTION:  This routine checks if it is a limit error.
c
c     INPUT:   kinp     I*4  D1  - Entry flag: 1 - process limit
c                                  error, 0 - exit routine.
c
c              kout     I*4  D1  - output status, 1 - process limit
c                                  error (retract etc.), 2 - point is
c                                  exactly on limit, do not process.
c******************************************************************
      subroutine rtmlim (kinp,kout)
c
      include 'post.inc'
      include 'lintol.inc'
c
      integer*4 kinp,kout
c
      equivalence (LRTRCT,KPOSMP(1278)), (IRTDEF,KPOSMP(1485))
      equivalence (MODROT,KPOSMP(4031))
c
      integer*4 LRTRCT,IRTDEF,MODROT
c
      equivalence (ROTBSV,POSMAP(2275)), (PTDEST,POSMAP(2477))
      equivalence (TBRMOD,POSMAP(4413)), (TRPOST,POSMAP(4419))
c
      real*8 TRPOST(3),ROTBSV(4),
     -       TBRMOD(2),PTDEST(3)
c
      integer*4 i,nout,naxs(10),ibase(4),idir(4),nax,jindex,lim
c
      real*8 gmch(3,4),rlin(6),raxs(10),rot(20,2),rdlt(4),prtx(10)
c
      data ibase /0,0,0,0/
c
      kout   = 0
      do 105 i=1,IRTDEF,1
         idir(i) = 3
  105 continue
c
c...First time here, check what axis is over limit
c
      if (kinp .eq. 1) then
          call copyn (TRPOST,gmch(1,2),3)
          call copyn (TRPOST,PTDEST,3)
          call copyn (ROTSTL,rot,20)
          call alladj (gmch,rlin,prtx,ROTSTL,2,5)
          rot(MODROT,1) = TBRMOD(1)
          call copyn (ROTBSV,ROTBAL,4)
          call rotlin (rot,ROTBSV,ibase,idir,rdlt)
          call alladj (gmch,rlin,raxs,rot,2,5)
c
c...Check which axis limit error
c
          call lmtchk (raxs,nout,naxs,0)
          if (nout .eq. 0) go to 8000
          lim = 1
          nax = jindex(naxs,lim,10)
          if (nax .eq. 0) then
             lim = 2
             nax = jindex(naxs,lim,10)
          end if
c
c...If table limit error: find point on limit &
c...rotate table 180 degree; if linear axis limit error:
c...find point on limit & rotate table to the most distant
c...point from limit plane
c
          if (nax .ne. 0) then
             if (LRTRCT .eq. 0) go to 8000
             call rtprep (raxs,prtx,naxs,nax,rot,lim,kout)
          end if
      end if
c
 8000 return
      end
c******************************************************************
c
c     SUBROUTINE: rtprep (gaxs,gprx,kaxs,knax,grot,klim,kstep)
c
c     FUNCTION:  This routine calculates & saves all table positions
c                and cl point on limit error for retract procedure
c                in the polar simulation of motion.
c
c     INPUT:   gaxs    R*8  D10  - Machine axes at the erroneous point
c                                  (over limit).
c
c              gprx    R*8  D10  - Machine axes at the erroneous point
c                                  without adjusting for table rotation.
c
c              kaxs    I*4  D10  - Flag for each exis that is out of
c                                  limit.
c
c              knax    I*4  D1   - Axis index which is out of limit.
c
c              grot    R*8  D20.2 - Rotary axes position calculated for
c                                 the point over limit.
c
c              klim    I*4  D1   - Which limit: 1 - upper limit error,
c                                  2 - lower limit error.
c
c     OUTPUT:  kstep   I*4  D1   - output status, 1 - process limit
c                                  error (retract etc.), 2 - point is
c                                  exactly on limit, do not process.
c
c***************************************************************************
      subroutine rtprep (gaxs,gprx,kaxs,knax,grot,klim,kstep)
c
      include 'post.inc'
      include 'lintol.inc'
c
      real*8 gaxs(10),gprx(10),grot(20,2)
      integer*4 knax,klim,kstep,kaxs(10)
c
      equivalence (IRTACT,KPOSMP(1256)), (IRTINC,KPOSMP(1461))
      equivalence (MODROT,KPOSMP(4031)), (MDTLIN,KPOSMP(4032))
c
      integer*4 MDTLIN,MODROT,IRTINC(4),IRTACT(2)
c
      equivalence (LIMITS,POSMAP(1254)), (PTPOST,POSMAP(2472))
      equivalence (RTOLER,POSMAP(2400))
      equivalence (TBLIML,POSMAP(2480)), (MATRXT,POSMAP(2482))
      equivalence (TBRMOD,POSMAP(4413))
      equivalence (AXSSAV,POSMAP(4423)), (TBLAST,POSMAP(4433))
      equivalence (POLARS,POSMAP(4435)), (POLARL,POSMAP(4438))
      equivalence (TABORG,POSMAP(5374))
c
      real*8 AXSSAV(10),PTPOST(3),RTOLER,
     -       TBLAST(2),TABORG(3,20),LIMITS(2,10),TBRMOD(2),
     -       TBLIML(2),POLARS(3),POLARL(3),MATRXT(3,4)
c
      integer*4 i,ier,n,na,lmx(3),lnx(5),nx,nud,lx,jindex,inc
c
      real*8 d,r,r1,ptm(3),raxs(10),linx(6),a,pmx(3),smx1(3),
     -       dumr(20,2),dumv(3),mch(3,4),smx(3),rmx(3),rlmn(3),dlt(3),
     -       org(3),mold(3),rold,ddl,do,dd,pti(3),wl(3),ang(2),t(3),
     3       rout(10),rsto(10)
c
      data lnx /1,0,2,0,3/, lmx /1,3,5/
c
c...Work with non-transformed axes
c
      call axsxfr (AXSSAV,rsto)
      call axsxfr (gaxs,rout)
c
c...Set up from & to points in table system
c
      smx(1) = rsto(1)
      smx(2) = rsto(3)
      smx(3) = rsto(5)
      call ptmatr (smx,smx1,MATRXT,1)
      pmx(1) = gprx(1)
      pmx(2) = gprx(3)
      pmx(3) = gprx(5)
      call ptmatr (pmx,pmx,MATRXT,1)
      call ptmatr (TABORG(1,MODROT),org,MATRXT,1)
      org(1) = 0.
      org(2) = 0.
      org(3) = 0.d0
      rold   = ROTSTL(MODROT,1)
      call copyn (STONUL(1,2),mold,3)
c
      rmx(1) = rout(1)
      rmx(2) = rout(3)
      rmx(3) = rout(5)
c
c...Get direction of motion in machine coord.
c
      dlt(1) = rout(1) - rsto(1)
      dlt(2) = rout(3) - rsto(3)
      dlt(3) = rout(5) - rsto(5)
      ddl  = dsqrt (dlt(1)**2 + dlt(2)**2 + dlt(3)**2)
c
c...Table limit error
c
      n      = 6 + MODROT
      inc    = IRTACT(1)
      if (IRTINC(IRTACT(2)) .eq. MODROT) inc = IRTACT(2)
      if (knax .eq. n) then
cc         if (dabs(rsto(inc)-LIMITS(klim,n)) .lt. 1.d-5) go to 500
         d    = LIMITS(klim,n) - rsto(inc)
         ddl  = rout(inc) - rsto(inc)
         r    = 1.d0
         if (dabs(ddl) .gt. 1.d-6) r  = d / ddl
         dd   = grot(MODROT,2) - ROTSTL(MODROT,2)
         r1   = r
         if (RTOLER .ne. 0.d0)
     -         call tblmio (smx1,rmx,pmx,org,dd,r1,d,ier,0)
         ang(1) = 180.d0 * (3 - 2*klim)
         ang(2) = ang(1)
         call movang (rold,d,TBRMOD(1))
         TBRMOD(2) = TBRMOD(1)
         dd    = dd - d
      else
c
c...Linear axis limit error,
c......Get direction (vector) from old to new position
c
         if (ddl .lt. 1.0d-8) go to 500
         rlmn(1) = dlt(1) / ddl
         rlmn(2) = dlt(2) / ddl
         rlmn(3) = dlt(3) / ddl
c
c......Get the closest linear axis limit violation
c
         do      = 0.0
         do 100 i=1,6
             if (kaxs(i) .eq. 0) go to 100
             nx     = i
             nud    = kaxs(i)
             if (jindex(lmx,nx,3) .eq. 0) go to 8000
             lx     = lnx(nx)
             if (rlmn(lx) .eq. 0.0) go to 100
             dd   = (rout(nx) - LIMITS(nud,nx)) / dlt(lx)
             if (dd .gt. do) then
                 do = dd
                 n  = nud
                 na = lx
             end if
  100    continue
c
c......Define point on limit in table coordinates &
c......convert it to the part system (note: interpolation won't work
c......because move is simulated in polar system)
c
         r     = 1.d0 - do
         call intrpt (smx,rmx,t,r)
c
         dd    = (ROTANL(MODROT,2) - ROTSTL(MODROT,2))
         wl(1) = 0.0
         wl(2) = 0.0
         wl(3) = 0.0
         wl(na) = 1.0 * (3 - 2*n)
c
c......Transform points to table plane
c
         call ptmatr (wl,wl,MATRXT,1)
         call ptmatr (t,pti,MATRXT,1)
         call tblmio (smx1,pti,pmx,org,dd,r1,a,ier,1)
         call movang (rold,a,TBRMOD(1))
         TBRMOD(2) = TBRMOD(1)
         dd    = dd - a
c
c......Find the best table positon considering this limit
c
         call tbplim (smx1,org,wl,ang)
      end if
c
c...Set all angles for retract
c
      call clrmot (1)
      call movang (TBRMOD(1),ang(1),TBLIML(1))
      call movang (TBRMOD(1),ang(2),TBLIML(2))
      call movang (TBLIML(1),dd,TBLAST(1))
      call movang (TBLIML(2),dd,TBLAST(2))
c
c...Interpolate point on limit as follows
c...1. If RTOLER=0 in polar coordinates,
c...2. othervise as linear motion (CL points)
c
      if (RTOLER .gt. 0.d0) then
         call intrpt (STONUL(1,2),MCHNUL(1,2),PTPOST,r1)
      else
         call intrpc (POLARS,POLARL,TABORG(1,MODROT),ptm,r1)
c
c...'ptm' is in machine coordinates, convert it to cl point
c
         call copyn (rout,raxs,10)
         raxs(1) = ptm(1)
         raxs(3) = ptm(2)
         raxs(5) = ptm(3)
         raxs(n) = LIMITS(klim,n)
         call alladr (raxs,linx,mch,dumr,dumv,5,2)
         call copyn (mch(1,2),PTPOST,3)
      end if
      kstep  = 1
      go to 8000
c
c...We are exactly on limit
c
  500 kstep  = 2
c
 8000 return
      end
c
c******************************************************************
c
c     SUBROUTINE: tbplim (gsmx,gorg,glim,gang)
c
c     FUNCTION:  This routine calculates delta angle for table
c                to position it at the most distant point from
c                the point on linear axis limit error.
c
c     INPUT:   gsmx    R*8  D3   - Machine axes at previous point
c                                  in table coordinate system.
c
c              gorg    R*8  D3   - Table center in table coord. sys.
c
c              glim    R*8  D3   - Limit plane in table coord. sys.
c
c     OUTPUT:  gprx    R*8  D2   - Angle at what table should be
c                                  rotated to get out of error (both
c                                  CLW or CCLW).
c
c***********************************************************************
      subroutine tbplim (gsmx,gorg,glim,gang)
c
      include 'post.inc'
c
      real*8 gsmx(3),gorg(3),gang(2),glim(4)
c
      real*8 vs(3),ds,amv,alm
c
c...Get direction vector of motion
c
      amv   = 0.
      vs(3) = 0.d0
      ds    = dsqrt ((gsmx(1)-gorg(1))**2 + (gsmx(2)-gorg(2))**2)
      vs(1) = (gsmx(1) - gorg(1)) / ds
      vs(2) = (gsmx(2) - gorg(2)) / ds
c
c...Get delta angle of limit plane from motion direction
c
      call vecang (vs,3,amv)
      call vecang (glim,3,alm)
      gang(1) = amv - alm
      gang(2) = 0.d0
      if (gang(1) .gt. 0.0) then
          gang(2) = gang(1) - 360.
      else
          gang(2) = 360. + gang(1)
      end if
c
      return
      end
c
c***************************************************************************
c
c     SUBROUTINE: tblmio (gsmx,gpti,gpmx,gorg,gdan,grat,gang,kerr,kfl)
c
c     FUNCTION:  This routine calculates the exact position (ratio)
c                of the point on limit in the part system of coord.
c
c     INPUT:   gsmx     R*8  D3  - Coordinates of old point in table
c                                  system.
c
c              gpti     R*8  D3  - Coordinates of point on limit when
c                                  linear limit error (kfl = 1), or
c                                  same as 'gpmx' (in table system).
c
c              gpmx     R*8  D3  - Coordinates of the next point in
c                                  table system after adjustments for
c                                  table rotation defined for this move.
c
c              gorg     R*8  D3  - Table center in table system.
c
c              gdan     R*8  D1  - Table delta angle as calculated without
c                                  considering a limit error.
c
c              grat     R*8  D1  - Ratio factor defining point where is
c                                  the limit error; if kfl = 1 it is
c                                  ratio of linear distances, if kfl = 2
c                                  it is ratio of table delta angles.
c
c              kfl      I*4  D1  - Entry flag: 1 - solve table position
c                                  & point if linear axis limit error,
c                                  2 - solve if table limit error.
c
c     OUTPUT:  gang     R*8  D1  - Table delta angle from old positon
c                                  to the linear limit error (kfl = 1).
c
c              grat     R*8  D1  - Ratio factor defining point where
c                                  limit error occurs in PART SYSTEM.
c
c              kerr     I*4  D1  - Error status.
c
c***************************************************************************
      subroutine tblmio (gsmx,gpti,gpmx,gorg,gdan,grat,gang,kerr,kfl)
c
      include 'post.inc'
c
      integer*4 kerr,kfl
      real*8 gsmx(3),gpti(3),gpmx(3),gorg(3),grat,gdan,gang
c
      equivalence (RAD   ,POSMAP(0002))
      real*8 RAD
c
      real*8 pmd(3),vc2(3),vp(3),dm,dc,dd,p,rpm,rpt,ndist,pl(4),vi(3),
     -       va(3),a,a1
c
      integer*4 i,ier
c
      kerr   = 0
      vc2(3) = 0.d0
      vp(3)  = 0.d0
      vi(3)  = 0.d0
c
      vi(1)  = gpti(1) - gorg(1)
      vi(2)  = gpti(2) - gorg(2)
c
      vp(1)  = gpmx(1) - gsmx(1)
      vp(2)  = gpmx(2) - gsmx(2)
      dm     = dsqrt(vp(1)**2 + vp(2)**2)
c
      vp(1)  = vp(1) / dm
      vp(2)  = vp(2) / dm
c
      rpm    = dsqrt((gpmx(1) - gorg(1))**2 + (gpmx(2) - gorg(2))**2)
      rpt    = dsqrt(vi(1)**2 + vi(2)**2)
      vi(1)  = vi(1) / rpt
      vi(2)  = vi(2) / rpt
c
c...get the midle point on the motion line
c
      pl(1) = -vp(2)
      pl(2) = vp(1)
      pl(3) = 0.d0
      pl(4) = pl(1)*gsmx(1) + pl(2)*gsmx(2)
      call plnint (gorg,pl,pl,pmd,ier)
      dd    = ndist (pmd,gorg)
c
      if (kfl .eq. 1) go to 100
c
c...kfl = 2, solve for table limit error
c
      call vecang (vi,3,a)
      call movang (a,gang,a1)
      va(1) = dcos (a1/RAD)
      va(2) = dsin (a1/RAD)
      va(3) = 0.d0
      call plnint (gorg,va,pl,vc2,ier)
      if (ier .ne. 0) go to 7000
      dc     = dsqrt ((vc2(1) - gsmx(1))**2 + (vc2(2) - gsmx(2))**2)
      grat   = dc / dm
      go to 8000
c
c...kfl = 1, solve for linear axis limit
c...get parameter of point on limit
c
  100 if (rpt .gt. dd) then
          p     = dsqrt (rpt*rpt - dd*dd)
      else
          go to 7000
      end if
c
c...select point on circle which is between gsmx & gpmx
c
      i      = 0
  200 i      = i + 1
      vc2(1) = pmd(1) + p * vp(1)
      vc2(2) = pmd(2) + p * vp(2)
      dc     = dsqrt ((vc2(1) - gsmx(1))**2 + (vc2(2) - gsmx(2))**2)
      if (i .eq. 1 .and. dc .gt. dm) then
          p  = -p
          go to 200
      end if
      grat   = dc / dm
c
c...get angle on limit, using previous sign
c
      vc2(1) = (vc2(1) - gorg(1)) / rpt
      vc2(2) = (vc2(2) - gorg(2)) / rpt
      call betvec (vi,vc2,gang)
      if (gdan .lt. 0.d0) gang = - gang
      go to 8000
c
 7000 kerr  = 1
 8000 return
      end

c***************************************************************************
c
c     SUBROUTINE: gtptrc (gmch,kstp)
c
c     FUNCTION:  This routine performs the retract/plunge sequence
c                in the polar interpolation of motion at the rotary
c                table when an axis limit interupts the motion.
c
c     INPUT:   gmch     R*8  D3.4 - Point on limit in Cl coord system.
c
c              kstp     I*4  D1   - Entry flag, specifies previous step
c                                   1 - initial call, 2 - retract tool,
c                                   3 - move to complimentary position,
c                                   4 - plunge tool, 5 - move to the final
c                                   point.
c
c     OUTPUT:  kstp     I*4  D1   - Exit flag, as above + 1, 6 - end of
c                                   retract procedure.
c
c***************************************************************************
      subroutine gtptrc (gmch,kstp)
c
      include 'post.inc'
      include 'lintol.inc'
c
      real*8 gmch(3,4)
      integer*4 kstp
c
      equivalence (LRTRCT,KPOSMP(1278))
      equivalence (ITP   ,KPOSMP(1801)), (MDTLIN,KPOSMP(4032))
c
      integer*4 LRTRCT,MDTLIN,ITP
c
      equivalence (PTLPLU,POSMAP(2301)), (PTPOST,POSMAP(2472))
      equivalence (PTDEST,POSMAP(2477)), (TBLIML,POSMAP(2480))
      equivalence (TL    ,POSMAP(3601)), (TBRMOD,POSMAP(4413))
      equivalence (TBLAST,POSMAP(4433))
c
      real*8 PTPOST(3),TBLAST(2),TL(120),TBLIML(2),
     -       TBRMOD(2),PTDEST(3),PTLPLU(3)
c
      integer*4 irro,kflg
      real*8 rdis
c
      irro   = 1
      kflg   = kstp
c
c...switch entry for subsequent calls
c
      go to (200,500,600,700,800) kflg
      go to 8000
c
c...Initail call, use point on limit
c
  200 call copyn (PTPOST,gmch(1,2),3)
      kflg   = 2
      go to 2000
c
c...Retract if flag is set
c
  500 if (LRTRCT .eq. 0) then
          go to 600
      else
          call mwrdis (gmch(1,3),ROTSTL,rdis)
          gmch(1,2) = PTPOST(1) + rdis * TLVECL(1)
          gmch(2,2) = PTPOST(2) + rdis * TLVECL(2)
          gmch(3,2) = PTPOST(3) + rdis * TLVECL(3)
          call copyn (gmch(1,2),PTLPLU,3)
          kflg   = 3
      end if
      go to 2000
c
c...output limit point with complimentary table position
c
  600 call copyn (TBLIML,TBRMOD,2)
      if (LRTRCT .ne. 0) then
          call copyn (PTLPLU,gmch(1,2),3)
      else
          call copyn (PTPOST,gmch(1,2),3)
      end if
      kflg   = 4
      go to 2000
c
c...Plunge the tool back
c
  700 if (LRTRCT .eq. 0) go to 800
      call copyn (PTPOST,gmch(1,2),3)
      kflg   = 5
      go to 2000
c
c...Turn off off limit logic and
c...move to the final point
c
  800 call copyn (PTDEST,gmch(1,2),3)
      call copyn (TBLAST,TBRMOD,2)
      kflg   = 6
      go to 2000
c
c...Get new point axes
c
 2000 MDTLIN = 3
      kstp   = kflg
c
c...End of routine
c
 8000 return
      end
c
c******************************************************************
c
c     SUBROUTINE: fixang (gmch,gsto,gang,grot)
c
c     FUNCTION:  This routine adjusts the angle of the rotary axis
c                moving due to the MODE/ROTATE command by .001 in
c                each direction to see if a better position is
c                available by attempting to keep the linear axis from
c                moving.
c
c                It was written due to FSR 61173 which demonstrates
c                some accumulative round off errors while processing
c                the MODE/ROTATE motion.  It is a bit of a kludge,
c                but no other solution readily presented itself at the
c                time.
c
c     INPUT:  gmch   R*8  D3  - Input coordinate.
c
c             gang   R*8  D1  - Calculated angle on a rotary scale.
c
c             gsto   R*8  D3  - Current adjusted tool position.
c
c             grot   R*8  D20 - Current rotary axes position.
c
c     OUTPUT: gang   R*8  D1  - Modified rotary angle.
c
c******************************************************************
c
      subroutine fixang (gmch,gsto,gang,grot)
c
      include 'post.inc'
c
      equivalence (MODROT,KPOSMP(4031))
c
      integer*4 MODROT
c
      real*8 gmch(3),gang,gsto(3),grot(20)
c
      integer*4 i
c
      real*8 rang(20),d,d1,rmch(3),ndist,dd(2),rsto(3)
c
c...Adjust input position to rotary axes
c...and get distance to previous position
c
      call dpoint (gsto(1),rsto(1),4)
      call dpoint (gsto(2),rsto(2),4)
      call dpoint (gsto(3),rsto(3),4)
      call copyn (grot,rang,20)
      rang(MODROT) = gang
      call ptrtad (gmch,rmch,rang,0)
      d = ndist(rmch,rsto)
c
c...Adjust angle by .001 and compare
c...new position with previous position
c
      dd(1) = .001
      dd(2) = -.002
      do 100 i=1,2,1
          rang(MODROT) = rang(MODROT) + dd(i)
          call ptrtad (gmch,rmch,rang,0)
          d1 = ndist(rmch,rsto)
          if (d1 .lt. d) then
              gang = rang(MODROT)
              d = d1
          endif
  100 continue
c
c...End of routine
c
 8000 return
	end
c
c******************************************************************
c
c     SUBROUTINE: movang (gang,gdlt,gout)
c
c     FUNCTION:  This routine shifts angle on rotary scale.
c
c     INPUT:  gang   R*8  D1  - Input angle on rotary scale.
c
c             gdlt   R*8  D1  - Signed delta angle.
c
c     OUTPUT: gout   R*8  D1  - Output angle on rotary scale.
c
c******************************************************************
      subroutine movang (gang,gdlt,gout)
c
      real*8 gang,gdlt,gout
c
      real*8 fula,a
c
      data fula /360.d0/
c
      a     = gang + gdlt
      if (a .lt. 0.d0) a = fula + a
      if (a .ge. fula) a = a - fula
      gout  = a
c
      return
      end
c
c******************************************************************
c
c     SUBROUTINE: fixaxs (gmch,glin,gaxs,grot)
c
c     FUNCTION:  This routine adjusts the linear axes if they are
c                within tolerance of their previous position.  This
c                adjustment is used to keep the linear axes from
c                wandering.
c
c                It was written due to FSR 61510 which demonstrates
c                some accumulative round off errors while processing
c                the MODE/ROTATE motion.  It is a bit of a kludge,
c                but no other solution readily presented itself at the
c                time.
c
c     INPUT:  gmch   R*8  D3,4 - Current input coordinates.
c
c             glin   R*8  D6   - Current linear axes.
c
c             gaxs   R*8  D3   - Current machine axes.
c
c     OUTPUT: gmch   R*8  D3   - Adjusted input coordinates.
c
c             glin   R*8  D6   - Adjusted linear axes.
c
c             gaxs   R*8  D3   - Adjusted machine axes.
c
c******************************************************************
c
      subroutine fixaxs (gmch,glin,gaxs)
c
      include 'post.inc'
c
      equivalence (MODROT,KPOSMP(4031))
c
      integer*4 MODROT
c
      equivalence (AXSSTO,POSMAP(1425))
c
      real*8 AXSSTO(10)
c
      real*8 gmch(3,4),glin(6),gaxs(10)
c
      integer*4 i,idid
c
      real*8 rot(20),tvec(3),acy
c
c...Fix round off problems with linear axes
c...If MODE/ROTATE is in effect
c
      idid   = 0
      call getacy (1,acy)
      do 250 i=1,6,1
          if (gaxs(i) .ne. AXSSTO(i) .and.
     1          dabs(gaxs(i)-AXSSTO(i)) .le. acy) then
              idid   = 1
              gaxs(i) = AXSSTO(i)
           endif
  250 continue
c
c...Axis was adjusted
c...Calculate all position arrays
c
      if (idid .eq. 1) call alladr (gaxs,glin,gmch,rot,tvec,5,1)
c
c...End of routine
c
 8000 return
      end
