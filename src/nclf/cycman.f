C*********************************************************************
C*    NAME         :  cycman.f
C*       CONTAINS:
C*			cycman  cycmn1  rapmod  cycmdo  cycpek  cycshf  pshcyc
C*    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       cycman.f , 25.2
C*    DATE AND TIME OF LAST  MODIFICATION
C*       10/19/15 , 17:24:15
C*********************************************************************
C
c
c***********************************************************************
c
c   SUBROUTINE: cycman (spt,gpt,kcyc,gcyc,kret,gout,knout)
c
c   FUNCTION:  This is the controlling routine for post generated Mill
c              cycles.
c
c   INPUT:  spt     R*8  D6  -  Point coming from.
c
c           gpt     R*8  D6  -  Point to perform cycle at.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c           kret    I*2  D1  -  1 = Retract to rapto plane.  2 = Retract
c                               to clearance plane.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine cycman (spt,gpt,kcyc,gcyc,kret,gout,knout)
c
      include 'com.com'
c
      integer*2 kcyc(10),kret,knout
c
      real*8 spt(6),gpt(6),gcyc(10),gout(4,500)
c
      integer*2 is(3),ids(2,3),iret
      integer*4 ierr
c
      real*8 rfin(3),rpos(3),rclr(3),pln(4)
c
      data ids /2,3, 1,3, 1,2/
c
c...Set up cycle positions
c......Assign major tool axis direction
c
      is(3) = 3
      if (abs(gpt(4)) .gt. abs(gpt(is(3)+3))) is(3) = 1
      if (abs(gpt(5)) .gt. abs(gpt(is(3)+3))) is(3) = 2
      is(1) = ids(1,is(3))
      is(2) = ids(2,is(3))
c
c......Final position
c
      rfin(1) = gpt(1) - gcyc(1) * gpt(4)
      rfin(2) = gpt(2) - gcyc(1) * gpt(5)
      rfin(3) = gpt(3) - gcyc(1) * gpt(6)
c
c......Rapto position
c
      rpos(1) = gpt(1) + gcyc(2) * gpt(4)
      rpos(2) = gpt(2) + gcyc(2) * gpt(5)
      rpos(3) = gpt(3) + gcyc(2) * gpt(6)
c
c......Clearance position
c
      if (kcyc(5) .eq. 0) then
c
c.........RTRCTO overrides clearance plane
c
          if (kcyc(3) .eq. 1) then
              gcyc(7) = gcyc(5)
c
c.........Normal clearance plane
c
          else
              call plnvpt (gpt(4),gpt(1),pln,ierr)
              call plndis (pln,spt,gcyc(7))
              if (gcyc(7) .lt. gcyc(2)) gcyc(7) = gcyc(2)
          endif
          kcyc(5) = 1
      endif
c
c.........Define clearance position
c
      rclr(1) = gpt(1) + gcyc(7) * gpt(4)
      rclr(2) = gpt(2) + gcyc(7) * gpt(5)
      rclr(3) = gpt(3) + gcyc(7) * gpt(6)
c
c......Determine retract plane
c
      iret   = kret
      if (iret .ne. 1 .or. kcyc(3) .eq. 1) iret = 2
c
c...Perform cycle
c
      call cycmdo (rfin,rpos,rclr,gpt(4),kcyc,gcyc,iret,gout,knout)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycmn1 (spt,gpt,kcyc,gcyc,kret,gout,knout)
c
c   FUNCTION:  This is the controlling routine for CYCLE pocket entry.
c
c   INPUT:  spt     R*8  D6  -  Point coming from.
c
c           gpt     R*8  D6  -  Point to perform cycle at.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine cycmn1 (lmdsys,spt0,gpt0,kcyc,gcyc,gout,knout)

      include 'com.com'
      include 'const.com'

      integer*2 kcyc(10),knout,lmdsys

      real*8 spt0(6),gpt0(6),gcyc(10),gout(4,500)

      real*8 tol,htol,hrad,ang,ang0,ang1,dang,dbet,co,si,z0,z1,dz,delr
      real*8 rclw
      real*8 spt(3),gpt(3)
      logical ldown
      integer*2 nramps,maxpt
      integer*2 i2zero /0/, i2one /1/
      integer*4 j
c
      data maxpt /500/

      call gettol(tol)
      htol = tol*5.

      do 100 i=1,6,1
          gpt(i) = gpt0(i)
          spt(i) = spt0(i)
  100 continue
      if (lmdsys .eq. 1) then
        call wcsmcs(i2zero,spt)
        call wcsmcs(i2zero,gpt)
        call wcsmcs(i2one,spt(4))
        call wcsmcs(i2one,gpt(4))
      endif
      knout = 0

      if (kcyc(7) .eq. 0) then ! IN
        hrad = gcyc(2) - gcyc(4)/2
      else
        hrad = gcyc(2)
      endif
      if (hrad .lt. htol) return

      j = 100000.*gpt(3) + 0.5
      gpt(3) = j/100000.

      z0 = gpt(3)
      z1 = gpt(3) - gcyc(1)

      knout = 1
      gout(1,knout) = gpt(1)
      gout(2,knout) = gpt(2)
      gout(3,knout) = gpt(3) + gcyc(7)
      gout(4,knout) = 0
      if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))

      ldown = (kcyc(5) .ne. 1)
      rclw = 1.
      if (kcyc(6) .eq. 1) rclw = -1.

      ang0 = gcyc(5)/RADIAN
      co = cos(ang0)
      si = sin(ang0)
      knout = 2
      gout(1,knout) = gpt(1) + hrad*co
      gout(2,knout) = gpt(2) + hrad*si
      gout(3,knout) = z0
      gout(4,knout) = gcyc(6)
      if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))

      j = 1000.*(gcyc(1)/gcyc(3)) + 0.5
      nramps = j/1000 ! whole part of z/zi
      dramp0 = j/1000. ! z/zi with four digits precision

      delr = dramp0 - j/1000 ! fractional part of z/zi

      ang1 = ang0 + rclw*delr*TWO_PI ! final angle
      if (ang1 .lt. 0) then
        ang1 = ang1 + TWO_PI
      else if (ang1 .ge. TWO_PI) then
        ang1 = ang1 - TWO_PI
      endif

      ang = dramp0*TWO_PI ! total (abs) angle of the helical motion

      dbet = 2.*acos(1. - htol/hrad)

      npts = ang/dbet + 0.5
      if (npts .lt. 2) npts = 2
      if (npts .gt. maxpt-100) npts = maxpt - 100

      dz = gcyc(1) / npts
      dang = (rclw*ang) / npts

      ang = ang0
      z = z0
      do i = 1 , npts
        ang = ang+dang
        co = cos(ang)
        si = sin(ang)
        z = z - dz
        if (i .eq. npts) z = z1
        knout = knout+1
        gout(1,knout) = gpt(1) + hrad*co
        gout(2,knout) = gpt(2) + hrad*si
        gout(3,knout) = z
        gout(4,knout) = gcyc(6)
        if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))
      enddo

      ang = ang1
c
c..... the final circle at the bottom
c
      npts = TWO_PI/dbet + 0.5
      if (npts .lt. 2) npts = 2
      if (npts+knout+5 .gt. maxpt) npts = maxpt - knout - 5
      if (npts .lt. 2) go to 8000
      dang = (rclw*TWO_PI) / npts
c      if (ldown) npts = npts-1
      do i = 1 , npts
        ang = ang+dang
        co = cos(ang)
        si = sin(ang)
        knout = knout+1
        gout(1,knout) = gpt(1) + hrad*co
        gout(2,knout) = gpt(2) + hrad*si
        gout(3,knout) = z1
        gout(4,knout) = gcyc(6)
        if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))
      enddo
c
c..... retract iff UP
c
      if (.not.ldown) then
cc        call vctovc (gout(1,knout),gpt)
cc        if (lmdsys .eq. 1) call wcsmcs(i2zero,gpt)
        knout = knout+1
        gout(1,knout) = gpt(1)
        gout(2,knout) = gpt(2)
        gout(3,knout) = z1 + gcyc(7)
        gout(4,knout) = 0
        if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))
c
        knout = knout+1
        gout(1,knout) = gpt(1)
        gout(2,knout) = gpt(2)
        gout(3,knout) = z0 + gcyc(8)
        gout(4,knout) = 0
        if (lmdsys .eq. 1) call mcswcs(i2zero,gout(1,knout))
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: rapmod (spt,gpt,kmod,gout,knout)
c
c   FUNCTION:  This routine modifies rapid motion per the user's setting.
c
c   INPUT:  spt     R*8  D6  -  Point coming from.
c
c           gpt     R*8  D6  -  Point going to.
c
c           kmod    I*4  D1  -  1 = Modify move along X-axis, 2 = Y-axis,
c                               3 = Z-axis, 4 = Tool axis.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                 for rapid motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine rapmod (spt,gpt,kmod,gout,knout)
c
      include 'com.com'
c
      integer*2 knout
      integer*4 kmod
c
      real*8 spt(6),gpt(6),gout(4,500)
c
      integer*4 imod
c
      real*8 dis,dis1,ept(6),d0
      data d0 /0.0d0/
c
c...Initialize routine
c
      knout = 0
c
c...Modify along major axis as
c...determined by tool axis
c...Calculate major axis
c
      imod   = kmod
      if (kmod .eq. 4) then
          dis    = -1.
          do 100 i=1,3,1
              dis1   = dabs(spt(i+3))
              if (dis1 .gt. dis) then
                  dis    = dis1
                  imod   = i
              endif
  100     continue
      endif
c
c...Rapid in controlling axis first
c
      if (gpt(imod)-spt(imod) .gt. .0001) then
          call conv8_8 (spt,ept,6)
          ept(imod) = gpt(imod)
          call pshcyc (gout,knout,d0,ept)
c
c...Rapid in controlling axis last
c
      else if (gpt(imod)-spt(imod) .lt. -.0001) then
          call conv8_8 (gpt,ept,6)
          ept(imod) = spt(imod)
          call pshcyc (gout,knout,d0,ept)
      endif
c
c...Push final point onto stack
c
      call pshcyc (gout,knout,d0,gpt)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycmdo (gfin,gpos,gclr,tvec,kcyc,gcyc,kret,gout,knout)
c
c   FUNCTION:  This routine performs all of the post-generated Mill
c              cycles.
c
c   INPUT:  gfin    R*8  D3  -  The linear axes position at the bottom
c                               of the hole.
c
c           gpos    R*8  D3  -  The linear axes position at the rapto
c                               plane.
c
c           gclr    R*8  D3  -  The linear axes position at the clear-
c                               ance plane.
c
c           tvec    R*8  D3  -  Current tool axis vector.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c           kret    I*2  D1  -  1 = Retract to rapto plane.  2 = Retract
c                               to clearance plane.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c   OUTPUT: none.
c
c***********************************************************************
c
      subroutine cycmdo (gfin,gpos,gclr,tvec,kcyc,gcyc,kret,gout,knout)
c
      include 'com.com'
c
      integer*2 kret,knout,kcyc(10)
c
      real*8 gfin(3),gpos(3),gclr(3),tvec(3),gout(4,500),gcyc(10)
c
      real*8 rnum, d0
      data d0 /0.0d0/
c
c...Initialize routine
c
      knout = 0
c
c...RETRCT/ON
c...Position to retract plane
c
      if (kret .eq. 2 .or. kcyc(2) .eq. 5) then
          if (kcyc(2) .eq. 5) then
              call cycshf (gout,knout,gclr,tvec,gcyc,1,1)
          else
              call pshcyc (gout,knout,d0,gclr)
          endif
      endif
c
c...Perform shift for BORE9
c
      if (kcyc(2) .eq. 5) then
          call cycshf (gout,knout,gpos,tvec,gcyc,1,2)
      endif
c
c...Position above hole (rapto plane)
c
      call pshcyc (gout,knout,d0,gpos)
c
c...Perform pecking cycles for
c...DEEP & THRU
c
      if (kcyc(2) .eq. 3 .or. kcyc(2) .eq. 4)
     1        call cycpek (gout,knout,gpos,gclr,tvec,kcyc,gcyc)
c
c...Feed in to final depth
c
      call pshcyc (gout,knout,gcyc(6),gfin)
c
c...Shift tool for
c...BORE9, SHIFT
c
      if (kcyc(2) .eq. 5 .or. kcyc(2) .eq. 6) then
          call cycshf (gout,knout,gfin,tvec,gcyc,1,2)
      endif
c
c...CYCLE/MILL
c...End of cycle
c
      if (kcyc(2) .eq. 8) go to 8000
c
c...Retract tool to rapto plane
c
      if ((kret .eq. 1 .or. kcyc(2) .eq. 2) .and. kcyc(2) .ne. 5) then
          if (kcyc(2) .eq. 5 .or. kcyc(2) .eq. 6)
     1            call cycshf (gout,knout,gpos,tvec,gcyc,1,2)
          rnum   = 0.
          if (kcyc(2) .eq. 2) rnum = gcyc(6)
          call pshcyc (gout,knout,rnum,gpos)
      endif
c
c...Retract tool to clearance plane
c
      if (kret .eq. 2 .or. kcyc(2) .eq. 5) then
          if (kcyc(2) .eq. 5 .or. kcyc(2) .eq. 6)
     1            call cycshf (gout,knout,gclr,tvec,gcyc,1,2)
          call pshcyc (gout,knout,d0,gclr)
      endif
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycpek (gout,knout,gpos,gret,gvec,kcyc,gcyc)
c
c   FUNCTION:  This routine performs the pecking portion of the DEEP and
c              THRU cycles.
c
c   INPUT:  gpos    R*8  D3  -  Current tool end point position.  This
c                               is usually the rapto plane position.
c
c           gret    R*8  D3  -  Retract position.
c
c           gvec    R*8  D3  -  Tool axis vector for cycle operation.
c
c           kcyc    I*2  D10 -  Integer cycle parameters.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine cycpek (gout,knout,gpos,gret,gvec,kcyc,gcyc)
c
      include 'com.com'
c
      integer*2 knout,kcyc(10)
c
      real*8 gout(4,500),gpos(3),gret(3),gvec(3),gcyc(10)
c
      real*8 rpt(3),rmch(3),rplng,pdif,rdep,fdep,pck1,pck2,rlst,rdis,
     1       tvec(3),rret(3),dr,d0
      data d0 /0.0d0/
c
c...Initialize routine
c
      if (gcyc(3) .eq. 0.) go to 8000
c
c......Define pecking depths
c
      pck1   = dabs(gcyc(3))
      pck2   = dabs(gcyc(4))
      if (pck2 .eq. 0.) then
          pck2   = pck1
          pdif   = 0.
          rlst   = 0.
      else
          pdif   = pck1   - pck2
          rlst   = pck2   * .2
      endif
c
c......Define final depth
c
      rdep   = 0.
      fdep   = gcyc(1) + gcyc(2)
      if (fdep .ge. 0.) then
          tvec(1) = gvec(1)
          tvec(2) = gvec(2)
          tvec(3) = gvec(3)
      else
          fdep   = dabs(fdep)
          tvec(1) = gvec(1) * (-1.)
          tvec(2) = gvec(2) * (-1.)
          tvec(3) = gvec(3) * (-1.)
      endif
c
c......Store current position
c
      rpt(1) = gpos(1)
      rpt(2) = gpos(2)
      rpt(3) = gpos(3)
c
c...THRU Retract plane
c
      if (kcyc(2) .eq. 4) then
          rret(1) = gpos(1)
          rret(2) = gpos(2)
          rret(3) = gpos(3)
      endif
c
c......Store plunge distance
c...ADJUST FOR METRIC
c
      rplng  = .1
c
c...Perform pecking cycle
c
  100 rdis   = pck1 - (rdep / fdep) * pdif
      if (rdep+rdis .ge. fdep .or. rdep+pck2+rlst .ge. fdep) go to 8000
      rdep   = rdep   + rdis
c
c......Feed in 'peck'
c
      rpt(1) = rpt(1) - rdis * tvec(1)
      rpt(2) = rpt(2) - rdis * tvec(2)
      rpt(3) = rpt(3) - rdis * tvec(3)
      call pshcyc (gout,knout,gcyc(6),rpt)
      dr     = dsqrt ((rret(1)-rpt(1))**2 +
     -                (rret(2)-rpt(2))**2 +
     -                (rret(3)-rpt(3))**2)
c
c.........Retract & plunge logic
c
      if (kcyc(2) .eq. 3) then
          rret(1) = rpt(1) + rplng * tvec(1)
          rret(2) = rpt(2) + rplng * tvec(2)
          rret(3) = rpt(3) + rplng * tvec(3)
      endif
      call pshcyc (gout,knout,d0,rret)
c
c...........Plunge if retracted over plunge level
c
      if (kcyc(2) .eq. 4 .and. dr .gt. rplng) then
          rmch(1) = rpt(1) + rplng * tvec(1)
          rmch(2) = rpt(2) + rplng * tvec(2)
          rmch(3) = rpt(3) + rplng * tvec(3)
          call pshcyc (gout,knout,d0,rmch)
      endif
c
c......Perform next peck
c
      go to 100
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: cycshf (gout,knout,gpos,gvec,gcyc,kdir,kpos)
c
c   FUNCTION:  This routine outputs the shifted point for the BORE9 and
c              shift cycles.
c
c   INPUT:  gpos    R*8  D3  -  Current tool end point position.  The
c                               shift amounts will be applied to this
c                               location.
c
c           gvec    R*8  D3  -  Current tool axis vector.
c
c           gcyc    R*8  D10 -  Real cycle parameters.
c
c           kdir    I*2  D1  -  1 = Apply the shift in the direction of
c                               the cycle OFFSET values.  -1 = Apply the
c                               shift in the opposite direction.
c
c           kpos    I*2  D1  -  1 = Position in rapid mode, 2 = Using
c                               rapid feedrate, 3 = Using programmed
c                               feedrate.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine cycshf (gout,knout,gpos,gvec,gcyc,kdir,kpos)
c
      include 'com.com'
c
      integer*2 knout,kdir,kpos
c
      real*8 gpos(3),gout(4,500),gvec(3),gcyc(10)
c
      integer*4 ids(2,3),is(3)
c
      real*8 rmch(3)
c
      data ids /2,3, 1,3, 1,2/
c
c...Get indices based on major tool axis
c
      is(3) = 3
      if (abs(gvec(1)) .gt. abs(gvec(is(3)))) is(3) = 1
      if (abs(gvec(2)) .gt. abs(gvec(is(3)))) is(3) = 2
      is(1) = ids(1,is(3))
      is(2) = ids(2,is(3))
c
c...Apply shift amounts
c
      rmch(is(1)) = gpos(is(1)) + gcyc(3) * kdir
      rmch(is(2)) = gpos(is(2)) + gcyc(4) * kdir
      rmch(is(3)) = gpos(is(3))
c
c...Output shifted point
c
      call pshcyc (gout,knout,0d0,rmch)
c
c...End of routine
c
 8000 return
      end
c
c***********************************************************************
c
c   SUBROUTINE: pshcyc (gout,knout,gfed,gpos)
c
c   FUNCTION:  This routine stores the cycle position 'gpos' and the
c              feed rate for this move in the output cycle stack 'gout'.
c
c   INPUT:  gpos    R*8  D3  -  Tool end point position to store.
c
c           gfed    R*8  D1  -  Current feed rate.
c
c   OUTPUT: gout    R*8  D4,500 - Array of points and feed rates created
c                                for cycle motion.
c
c           knout   I*2  D1    - Number of points in 'gout'.
c
c***********************************************************************
c
      subroutine pshcyc (gout,knout,gfed,gpos)
c
      include 'com.com'
c
      integer*2 knout
c
      real*8 gpos(3),gout(4,500),gfed
c
c...Store current position
c
      if (knout .lt. 500) then
          knout  = knout  + 1
          gout(1,knout) = gpos(1)
          gout(2,knout) = gpos(2)
          gout(3,knout) = gpos(3)
          gout(4,knout) = gfed
      endif
c
c...End of routine
c
 8000 return
      end
