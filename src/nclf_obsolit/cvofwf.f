C*********************************************************************
C*    NAME         :  cvofwf.f
C*       CONTAINS:
C*    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL
c*       cvofwf.f , 25.1
c*    DATE AND TIME OF LAST  MODIFICATION
c*       04/29/15 , 15:09:48
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cvofwf
c*        Create an NCL curve from a unicad curve.
C*    PARAMETERS   
C*       INPUT  : 
C*          nclkey     - key of unicad curve
C*       OUTPUT :  
C*                Common array W contains NCL curve
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C*********************************************************************
C
      subroutine cvofwf (nclkey)

      include 'com.com'

      integer*4 nclkey

c     old version point storage
      integer*2 mxevpt
      parameter (mxevpt=100)

c     new version point storage
      integer*2 maxpts
      parameter (maxpts=600)
      real*8 pts(maxpts*3)
      real*8 vs(maxpts*3)

      real*8 u, du, asw, dtol,dis,dm(3)
      integer*4 ncevolve
      integer*2 ifl4x, ifl7, ifl8, isf, i, j, npts
      integer*2 izro
      logical lv90,lv94
      data izro /0/

c     version test variable
      lv90 = sc(169).lt.9.049d0
      lv94 = sc(169).lt.9.449d0

      ifl4x = ifl(4)
      ifl7 = ifl(7)
      ifl8 = ifl(8)
      isf = 3
      call evstup (nclkey, isf)
c
c.... check version before execution
c
      if (lv90) then
          u = 0.d0
          du = mxevpt-1
          du = 1.d0/du
          call uevcvt (u, isf, izro, pts, vs, ifl(2))
          do 10 i=2,mxevpt
              j = i*3-2
              u = u+du
              call uevcvt (u, isf, izro, pts(j), vs(4), ifl(2))
10        continue
          npts = mxevpt
          dtol = sc(27)*2.d0
          call ptsmth (pts, npts, vs, dtol)
          call crvgen (npts, pts, vs, asw)
      else
c
c..... version 9.1 or greater
c..... V 9.1 has a smaller tolerance
c
          dtol = sc(27)
          if (ifl(264).eq.1) dtol = dtol/25.4d0
c
c...Pick up number of 3-D points req'd to meet dtol
c...call curve evaluator based on ncl_evolve_curve()
c...when version is 9.4 or lower
c
          if (lv94) then
              npts = ncevolve(isf, dtol, maxpts, pts, vs)
c
c...Otherwise use curve offset logic so that
c...sharp corners are maintained
c
          else
              dis = 0.
              dm(1) = 0.
              dm(2) = 0.
              dm(3) = 1.
              npts = ncevof(isf,dtol,maxpts,dm,dis,pts,vs)
          endif
          
          if (npts.le.0) then
c
c... Eduard 08/19/99. Added "Impossible geometry" error in case npts = 0
c
              ifl(2) = 163
          else if (npts.gt.maxpts) then
c
c..... test allocation error: if greater than allocated space
c
              ifl(2) = 156
          else
              call crvgn1 (npts, pts, vs, asw)
          endif
      endif

      ifl(4) = ifl4x
      ifl(7) = ifl7
      ifl(8) = ifl8

999   RETURN
      END
