C*********************************************************************
C*    NAME         :  transf.f
C*       CONTAINS:
C*           nclf_transf  transf  cidmpl  xyplmx
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       transf.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:48
C*********************************************************************
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine nclf_transf (rd, rmx, nwds, ietype)
c*       C wrapper routine for transf routine.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          (See transf)
c*
C*       OUTPUT :  
C*          (See transf)
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine nclf_transf (rd, rmx, nwds, ietype)
c
      real*8 rd(11), rmx(12)
      integer*2 nwds, ietype
c
      call transf (rd,rmx,nwds,ietype)
c
      return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine transf (rd, rmx, nwds, ietype)
c*       handles transformation of geometric canonical data through a 
c*       matrix for refsys or tracut. 
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          rd     - real*8 array
c*                   array of data to transform
c*          rmx    - real*8 array
c*                   matrix to use for transformation
c*          nwds   - integer*2
c*                   number of values to transform (used for cv and sf)
c*          ietype - integer*2
c*                   type of geometry being transformed
c*                   3 = point (real*8 x, y and z coordinates)
c*                   4 = vector (real*8 i, j and k values)
c*                   5 = line
c*                   6 = plane
c*                   7 = circle
c*                   8 = curve
c*                   21 = pointvector 
c*                   81 = curve segment
c*                   9 = surf
c*                   91 = surf patch
c*
C*       OUTPUT :  
C*          rd - real*8 array
c*                   modified array of data that was transformed
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine transf (rd, rmx, nwds, ietype)

      include 'com8a.com'

      real*8 rd(11), rmx(12)
      integer*2 nwds, ietype

      real*8 q(3), headw8, fv(3), fx,fy,fz, pllim(4)
      real*4 headw4(2)
      integer*2 inp(3), nw
      equivalence (headw4, headw8, inp)

      integer*2 pt, ve, ln, pl, ci, cv, sf, shortv, cvseg, sfpat
      parameter (pt=3, ve=4, ln=5, pl=6, ci=7, cv=8, sf=9)
      parameter (shortv=41, cvseg=81, sfpat=91, pv=21)

c        load first word of data into temporary variable
      headw8 = rd(1)

c                                                   ****  point or vector
      if (ietype .eq. pt .or. ietype .eq. ve) then
          call conent (rd, rmx, ietype)

c                                                   ****  line
      else if (ietype .eq. ln .or. ietype .eq. pv) then
          call conent (rd, rmx, pt)
          call conent (rd(4), rmx, ve)

c                                                   ****  plane
      else if (ietype .eq. pl) then
c                                       Transform display point if given
          if (nwds.eq.7) call conent (rd(5), rmx, pt)
          ix = 1
          go to 210
c                                                   ****  circle
      else if (ietype .eq. ci) then
c
c...  Create a vector from start point to end point to test later to see if
c...  circle direction is reversed
c
          if (rd(8).eq.0.0.and.rd(9).eq.0.0.and.rd(10).eq.0.0) then
            call cidmpl (rd, pllim)
          else
            pllim(1) = rd(8)
            pllim(2) = rd(9)
            pllim(3) = rd(10)
          endif
          fv(1) = rd(5)*pllim(3)-rd(6)*pllim(2)
          fv(2) = rd(6)*pllim(1)-rd(4)*pllim(3)
          fv(3) = rd(4)*pllim(2)-rd(5)*pllim(1)
c
c...  Transform circle center point and normal vector.
c
          call conent (rd, rmx, pt)
          call conent (rd(4), rmx, ve)

c             factor vec to unity and rad by inverse of same
          sec = dsqrt(rd(4)**2+rd(5)**2+rd(6)**2)

c             if sec too small, exit
          if (sec .gt. .00001) then
              rd(4) = rd(4)/sec
              rd(5) = rd(5)/sec
              rd(6) = rd(6)/sec
              rd(7) = rd(7)*sec
              ix = 8
              go to 210
          endif
          ifl(2) = 163

c                                                   ****  curve
      else if (ietype .eq. cv .or. ietype .eq. cvseg) then
          nw = 1

c             if this call is for a full set of curve data (ietype = 8)
c             then bump the pointer to the first segment point data
          if (ietype .eq. cv) then
            nw = (headw4(1)+1)/2+1
            nwds = nw+headw4(1)*6
          endif

c             transform point coordinates
310       call conent (rd(nw), rmx, pt)

c             transform delta values
          call conent (rd(nw+3), rmx, shortv)
          nw = nw+6
          if (nw .lt. nwds) go to 310

c                                                   ****  surface
      else if (ietype .eq. sf .or. ietype .eq. sfpat) then

c             inp(1) = 0 if surface is full surface type
c                      1 if surface is ruled surface type
c             inp(2) = number of patches if ietype = 9
c                      0 (no meaning) if ietype = 91
c             inp(3) = number of real*8's per patch

          if (ietype .eq. sf) then

c                  setup for panel
              inp(3) = 8
              if (inp(1) .eq. 0) inp(3) = 14
              nw = 3+(inp(2)+1)/2
c...
c      write(cout,9010)inp(1),inp(2),inp(3),nw
c9010  format(' transf: sf, imp(',3i4,'), nw =',i4)
c      call putmsg(cout,80)
c...
          else

c                  setup for patch
              inp(2) = 0
              inp(3) = nwds
              nw = 1
          endif

c             transform data
          do 490, j=1, inp(2)+1

c                 transform initial point (real*8)
              call conent (rd(nw), rmx, pt)
              inp(1) = nw+inp(3)-1

c                 bump pointer to point to deltas
              nw = nw+3
410           ix = 0

c                 transform delta offset values
420           call conent (rd(nw), rmx, shortv+ix)
              nw = nw+1+ix
              if (nw .lt. inp(1)) go to 480
              nw = nw+1
              if (nw .ge. nwds) go to 99999
              go to 490
480           if (ix .eq. 1) go to 410
              ix = 1
              go to 420
490       continue
      endif
      go to 99999

c             put npt in q
210   k = ix-1
      dis = rd(k+4)
      q(1) = rd(k+1)*dis
      q(2) = rd(k+2)*dis
      q(3) = rd(k+3)*dis

c         transform npt in place
      call conent (q, rmx, pt)
 
c         tranform pl normal
      call conent (rd(ix), rmx, ve)

c         calc const and unitize the abcd eq.
      rd(k+4) = rd(k+1)*q(1)+rd(k+2)*q(2)+rd(k+3)*q(3)
      sec = dsqrt(rd(k+1)**2+rd(k+2)**2+rd(k+3)**2)

c         if sec very small, must be circle unbounded case.
      if (sec .ge. .0001) then
        rd(k+1) = rd(k+1)/sec
        rd(k+2) = rd(k+2)/sec
        rd(k+3) = rd(k+3)/sec
        rd(k+4) = rd(k+4)/sec
      endif
      if (ietype .eq. PLANE) goto 99999
c
c... If vector from start point to end point of circle has reversed,
c... reverse circle axis so circle direction will be correct.
c
      call conent (fv, rmx, ve)
      call conent (pllim, rmx, ve)
      fx = rd(5)*pllim(3)-rd(6)*pllim(2)
      fy = rd(6)*pllim(1)-rd(4)*pllim(3)
      fz = rd(4)*pllim(2)-rd(5)*pllim(1)
      if (fv(1)*fx+fv(2)*fy+fv(3)*fz .lt. 0.0) then
        rd(4) = -rd(4)
        rd(5) = -rd(5)
        rd(6) = -rd(6)
      endif

99999 return
      end
C*********************************************************************
C*    E_SUBROUTINE     : subroutine cidmpl (buf, pllim)
c*       Create a dummy limit plane for an unbouned circle.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          buf         - Circle data
C*       OUTPUT :  
C*          pllim       - Dummy limit plane
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine cidmpl (buf, pllim)

      include 'com8a.com'

      real*8 buf(7), pllim(4)

      real*8 rx,ry,rz

      pllim(1) = 0.0
      pllim(2) = 0.0
      pllim(3) = 0.0
c
c...  Create a vector in the plane of the circle
c
      if (dabs(buf(5)).gt..9999.or.dabs(buf(6)).gt..9999) then
c
c...  Circle normal is a Y or Z vector, use X vector
c
        pllim(1)=-1.
        if (buf(5).lt.-.999.or.buf(6).lt.-.999) pllim(1)=1.
        goto 60
      endif
      if (dabs(buf(4)).gt..9999) then
c
c...  Circle normal is a X vector, use Y vector
c
        pllim(2)=-1.
        if (buf(4).lt.0.) pllim(2)=1.
        goto 60
      endif
c
c...  Circle normal is not pure X, Y or Z vector. Get an approximate vector,
c...  cross twice to get a vector in the plane of the circle & unitize.
c
      if (buf(4).lt..9.and.buf(4).ge.0.) then
        pllim(1)=-1.
      else if (buf(4).gt.-.9) then
        pllim(1)=1.
      else if (buf(4).lt.0.) then
        pllim(2)=1.
      else
        pllim(2)=-1.
      endif
      rx=buf(5)*pllim(3)-buf(6)*pllim(2)
      ry=buf(6)*pllim(1)-buf(4)*pllim(3)
      rz=buf(4)*pllim(2)-buf(5)*pllim(1)
      pllim(1)=ry*buf(6)-rz*buf(5)
      pllim(2)=rz*buf(4)-rx*buf(6)
      pllim(3)=rx*buf(5)-ry*buf(4)
      sec=dsqrt(pllim(1)**2+pllim(2)**2+pllim(3)**2)
      if (sec.eq.0.)sec=1.
      pllim(1)=pllim(1)/sec
      pllim(2)=pllim(2)/sec
      pllim(3)=pllim(3)/sec

60    pllim(4)=pllim(1)*buf(1)+pllim(2)*buf(2)+pllim(3)*buf(3)-buf(7)

999   return
      end
C
C*********************************************************************
C*    E_SUBROUTINE     : xyplmx (gvec,gmx,kflag)
c*       Creates a matrix that converts the coordinate system from
c*       the input vector to the XY-plane.
c*
C*    PARAMETERS   
C*       INPUT  : 
C*          gvec        - Input vector to rotate to XY-plane.
C*       OUTPUT :  
C*          gmx         - Matrix used to rotate geometry to XY-plane.
C*          kflag       - 0 = Vector is already 0,0,1.  1 = Vector is
C*                        outside of the XY-plane.
c*
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/

      subroutine xyplmx (gvec,gmx,kflag)
c
      include 'com8a.com'
c
      logical kflag
c
      real*8 gvec(3),gmx(12)
c
      real*8 tltmax,dsec
c
c...Initialize routine
c
      tltmax = 1.d0
      kflag  = .false.
      if (sc(169) .lt. 9.049) tltmax = .999999d0
      if (sc(169) .lt. 8.449) tltmax = .9999d0
      if (gvec(3) .lt. tltmax) then
        kflag = .true.
        gmx(9) = gvec(1)
        gmx(10) = gvec(2)
        gmx(11) = gvec(3)
        if (dabs(gvec(2)).lt..9999d0) then
          gmx(1) = gvec(3)
          gmx(2) = 0.d0
          gmx(3) = -gvec(1)
        else
          gmx(1) = 0.d0
          gmx(2) = 0.d0
          gmx(3) = gvec(2)
        endif
        dsec = dsqrt(gmx(1)**2+gmx(2)**2+gmx(3)**2)
        gmx(1) = gmx(1)/dsec
        gmx(2) = gmx(2)/dsec
        gmx(3) = gmx(3)/dsec
        gmx(5) = gmx(10)*gmx(3) - gmx(11)*gmx(2)
        gmx(6) = gmx(11)*gmx(1) - gmx(9)*gmx(3)
        gmx(7) = gmx(9)*gmx(2) - gmx(10)*gmx(1)
        gmx(4) = 0.d0
        gmx(8) = 0.d0
        gmx(12) = 0.d0
      endif
c
c...End of routine
c
 8000 return
      end
